###############################################################################
# app.R - Shiny App Demonstrating SAML Login with Descope as IdP
###############################################################################
library(shiny)
library(shinyjs)
library(httr)
library(xml2)    # For parsing raw SAML XML if needed
library(openssl) # For optional base64 decoding

# 1. Load environment variables
saml_sso_url  <- "https://api.descope.com/v1/auth/saml/idp/sso?app=P2pBl7sYVGg1RWtv4jC7zfO9TnUN-SA2s1P1INGkztIPv1R1gp2fR4gHQN"   # e.g. "https://api.descope.com/<proj>/saml/sso"
saml_acs_url  <- "https://api.descope.com/v1/auth/saml/idp/acs?app=P2pBl7sYVGg1RWtv4jC7zfO9TnUN-SA2s1P1INGkztIPv1R1gp2fR4gHQN"   # e.g. "https://api.descope.com/<proj>/saml/acs"
sp_entity_id  <- "https://api.descope.com/P2pBl7sYVGg1RWtv4jC7zfO9TnUN-SA2s1P1INGkztIPv1R1gp2fR4gHQN"

# This is where we want Descope to redirect back after login:
app_base_url  <- "https://fa5b3030bcc5445f86b37290d1814414.app.posit.cloud/p/e9067a03/"

# 2. Define the UI
ui <- fluidPage(
  useShinyjs(),
  titlePanel("Shiny App with Descope SAML (IdP)"),
  
  # Authentication UI
  div(
    id = "authContainer",
    actionButton("login_saml", "Log In with SAML via Descope")
  ),
  
  # Main content (hidden until successful SAML login)
  hidden(
    div(
      id = "appContent",
      sidebarLayout(
        sidebarPanel(
          fileInput("file", "Upload Expense Data (CSV)", accept = ".csv"),
          selectInput("category", "Select Category:", choices = NULL),
          actionButton("analyze", "Analyze")
        ),
        mainPanel(
          tabsetPanel(
            tabPanel("Overview", plotOutput("expenseTrend"), tableOutput("summaryTable")),
            tabPanel("Category Analysis", plotOutput("categoryTrend"), tableOutput("categoryTable"))
          )
        )
      )
    )
  )
)

# 3. Define the Server
server <- function(input, output, session) {
  
  # Helper to generate random RelayState to link request<->response
  generate_state <- function() {
    paste(sample(c(letters, LETTERS, 0:9), 16, replace = TRUE), collapse = "")
  }
  
  # 3A. On login click: build SSO URL, redirect user to Descope (IdP)
  observeEvent(input$login_saml, {
    state <- generate_state()
    session$userData$state <- state
    
    # This SSO URL typically includes RelayState & entityID
    login_url <- sprintf(
      "%s?RelayState=%s&entityID=%s&redirect_uri=%s",
      saml_sso_url,
      state,
      sp_entity_id,
      app_base_url
    )
    
    cat("SAML login URL:", login_url, "\n")
    
    # If deployed on a cloud service, a JS redirect works well:
    runjs(sprintf("window.location.href = '%s';", login_url))
  })
  
  # 3B. Handle the returned SAMLResponse
  observe({
    # Typically, user is redirected back with ?SAMLResponse=...&RelayState=...
    query <- parseQueryString(session$clientData$url_search)
    
    if (!is.null(query$SAMLResponse)) {
      # Validate RelayState
      if (!is.null(query$RelayState) && query$RelayState != session$userData$state) {
        showNotification("State (RelayState) mismatch. Possible CSRF detected.", type = "error")
        return()
      }
      
      # Extract the base64 SAMLResponse
      saml_response_raw <- query$SAMLResponse
      cat("Raw SAMLResponse (base64):", substr(saml_response_raw, 1, 40), "...", "\n")
      
      # 3C. Post the SAMLResponse to Descope's ACS for validation
      acs_response <- POST(
        saml_acs_url,
        body = list(SAMLResponse = saml_response_raw),
        encode = "form"
      )
      
      if (status_code(acs_response) == 200) {
        # If valid, parse user info from the response
        saml_user <- content(acs_response, as = "parsed")
        
        showNotification("SAML login successful!", type = "message")
        cat("User Info (from Descope ACS):", toString(saml_user), "\n")
        
        # Optionally store user info in the session
        session$userData$user <- saml_user
        
        # Show the main app content
        show("appContent")
        hide("authContainer")
      } else {
        showNotification("SAML assertion validation failed.", type = "error")
        cat("ACS Response Error:", content(acs_response, "text"), "\n")
      }
    }
  })
  
  # 3D. Reactive expense data
  expense_data <- reactiveVal(data.frame(
    Month = rep(month.abb, each = 4),
    Category = rep(c("Rent", "Groceries", "Utilities", "Entertainment"), times = 12),
    Amount = runif(48, 100, 1000)
  ))
  
  # Update expense data when a file is uploaded
  observeEvent(input$file, {
    req(input$file)
    new_data <- read.csv(input$file$datapath)
    expense_data(new_data)
  })
  
  # Dynamically update the category dropdown
  observe({
    data <- expense_data()
    updateSelectInput(session, "category", choices = unique(data$Category))
  })
  
  # 3E. Outputs
  # Overview plot
  output$expenseTrend <- renderPlot({
    data <- expense_data()
    aggregate_data <- aggregate(Amount ~ Month, data, sum)
    barplot(
      aggregate_data$Amount,
      names.arg = aggregate_data$Month,
      col = "skyblue",
      main = "Monthly Expense Trend",
      ylab = "Total Expenses ($)",
      xlab = "Month"
    )
  })
  
  # Overview table
  output$summaryTable <- renderTable({
    data <- expense_data()
    summary <- aggregate(Amount ~ Category, data, sum)
    colnames(summary) <- c("Category", "Total Amount ($)")
    summary
  })
  
  # Category-specific plot
  output$categoryTrend <- renderPlot({
    req(input$category)
    data <- expense_data()
    cat_data <- subset(data, Category == input$category)
    agg_data <- aggregate(Amount ~ Month, cat_data, sum)
    plot(
      agg_data$Amount,
      type = "o",
      col = "darkgreen",
      xaxt = "n",
      main = paste("Trend for", input$category),
      xlab = "Month",
      ylab = "Amount ($)"
    )
    axis(1, at = 1:12, labels = month.abb)
  })
  
  # Category-specific table
  output$categoryTable <- renderTable({
    req(input$category)
    data <- expense_data()
    cat_data <- subset(data, Category == input$category)
    agg_data <- aggregate(Amount ~ Month, cat_data, sum)
    colnames(agg_data) <- c("Month", "Amount ($)")
    agg_data
  })
}

# 4. Run the app
shinyApp(ui, server)
