library(shiny)
library(shinyjs)
library(httr)
library(xml2)      # For parsing XML if needed
library(openssl)   # For base64 decoding, if you do local SAMLResponse parsing

# Load environment variables
saml_sso_url      <- Sys.getenv("DESCOPE_SAML_SSO_URL")
saml_acs_url      <- Sys.getenv("DESCOPE_SAML_ACS_URL")
sp_entity_id      <- Sys.getenv("DESCOPE_SAML_SP_ENTITY_ID")
redirect_uri      <- "https://fa5b3030bcc5445f86b37290d1814414.app.posit.cloud/p/9243d5eb/"

# ----- UI Definition -----
ui <- fluidPage(
  useShinyjs(),
  titlePanel("Shiny App with Descope SAML SSO"),
  div(
    id = "authContainer",
    actionButton("login_saml", "Log In with SAML via Descope")
  ),
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

# ----- Server Definition -----
server <- function(input, output, session) {
  
  generate_state <- function() {
    paste(sample(c(letters, LETTERS, 0:9), 16, replace = TRUE), collapse = "")
  }
  
  observeEvent(input$login_saml, {
    state <- generate_state()
    session$userData$state <- state
    
    login_url <- sprintf(
      "%s?RelayState=%s&entityID=%s&redirect_uri=%s",
      saml_sso_url,
      state,
      sp_entity_id,
      redirect_uri
    )
    
    cat("SAML login URL:", login_url, "\n")
    
    # browseURL(login_url)
    # For a hosted environment, consider:
    runjs(sprintf("window.location.href = '%s';", login_url))
  })
  
  observe({
    query <- parseQueryString(session$clientData$url_search)
    
    # If the IdP (Descope) returns a SAMLResponse param in the URL (HTTP-Redirect binding):
    if (!is.null(query$SAMLResponse)) {
      
      # Check state if you used one
      if (!is.null(query$RelayState) && query$RelayState != session$userData$state) {
        showNotification("State (RelayState) mismatch. Possible CSRF detected.", type = "error")
        return()
      }
      
      saml_response_raw <- query$SAMLResponse
      cat("Raw SAMLResponse (base64):", substr(saml_response_raw, 1, 40), "...", "\n")
      
      acs_response <- POST(
        saml_acs_url,
        body = list(SAMLResponse = saml_response_raw),
        encode = "form"
      )
      
      if (status_code(acs_response) == 200) {
        saml_user <- content(acs_response, as = "parsed")
        
        showNotification("SAML login successful!", type = "message")
        show("appContent")
        hide("authContainer")
        
        cat("User Info (from Descope ACS):", toString(saml_user), "\n")
        
        # Optionally, store in session for further reference:
        session$userData$user <- saml_user
        
      } else {
        showNotification("SAML assertion validation failed.", type = "error")
      }
    }
  })
  
  # Reactive data
  expense_data <- reactiveVal(data.frame(
    Month = rep(month.abb, each = 4),
    Category = rep(c("Rent", "Groceries", "Utilities", "Entertainment"), times = 12),
    Amount = runif(48, 100, 1000)
  ))
  
  # File upload
  observeEvent(input$file, {
    req(input$file)
    uploaded_data <- read.csv(input$file$datapath)
    expense_data(uploaded_data)
  })
  
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
  
  # Overview summary
  output$summaryTable <- renderTable({
    data <- expense_data()
    summary <- aggregate(Amount ~ Category, data, sum)
    colnames(summary) <- c("Category", "Total Amount ($)")
    summary
  })
  
  # Category Analysis plot
  output$categoryTrend <- renderPlot({
    req(input$category)
    data <- expense_data()
    category_data <- subset(data, Category == input$category)
    aggregate_data <- aggregate(Amount ~ Month, category_data, sum)
    plot(
      aggregate_data$Amount,
      type = "o",
      col = "darkgreen",
      xaxt = "n",
      main = paste("Trend for", input$category),
      xlab = "Month",
      ylab = "Amount ($)"
    )
    axis(1, at = 1:12, labels = month.abb)
  })
  
  # Category Analysis table
  output$categoryTable <- renderTable({
    req(input$category)
    data <- expense_data()
    category_data <- subset(data, Category == input$category)
    aggregate_data <- aggregate(Amount ~ Month, category_data, sum)
    colnames(aggregate_data) <- c("Month", "Amount ($)")
    aggregate_data
  })
}

# Run the app
shinyApp(ui, server)
