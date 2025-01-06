library(shiny)
library(shinyjs)
library(httr)

# Load environment variables
client_id <- Sys.getenv("DESCOPE_CLIENT_ID")
client_secret <- Sys.getenv("DESCOPE_CLIENT_SECRET")
auth_url <- Sys.getenv("DESCOPE_AUTH_URL")
token_url <- Sys.getenv("DESCOPE_TOKEN_URL")
user_info_url <- Sys.getenv("DESCOPE_USER_INFO_URL")
redirect_uri <- Sys.getenv("DESCOPE_REDIRECT_URI")

# UI Definition
ui <- fluidPage(
  useShinyjs(), # For dynamic UI control
  titlePanel("Personal Finance Dashboard with Descope OIDC Auth"),
  div(
    id = "authContainer",
    actionButton("login", "Log In with Descope")
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

# Server Definition
server <- function(input, output, session) {
  # Generate a random state
  generate_state <- function() {
    paste(sample(c(letters, LETTERS, 0:9), 16, replace = TRUE), collapse = "")
  }
  
  observeEvent(input$login, {
    state <- generate_state()
    session$userData$state <- state  # Store the state for verification later
    
    # Generate login URL with state
    login_url <- sprintf(
      "%s?client_id=%s&redirect_uri=%s&response_type=code&scope=openid%%20profile%%20email&state=%s",
      auth_url, client_id, redirect_uri, state
    )
    
    cat("login_url:", toString(login_url), "\n")
    browseURL(login_url)
  })
  
  observe({
    # Parse query parameters from the URL
    query <- parseQueryString(session$clientData$url_search)
    
    cat("code", toString(query))
    
    if (!is.null(query$code) && !is.null(query$state)) {
      if (query$state != session$userData$state) {
        showNotification("Invalid state parameter. Possible CSRF detected.", type = "error")
        return()
      }
      
      # Exchange the authorization code for tokens
      token_response <- POST(
        token_url,
        authenticate(client_id, client_secret),
        body = list(
          grant_type = "authorization_code",
          code = query$code,
          redirect_uri = redirect_uri
        ),
        encode = "form"
      )
      
      if (status_code(token_response) == 200) {
        access_token <- content(token_response)$access_token
        
        # Fetch user information
        user_info_response <- GET(
          user_info_url,
          add_headers(Authorization = paste("Bearer", access_token))
        )
        
        if (status_code(user_info_response) == 200) {
          user_info <- content(user_info_response)
          showNotification("Login successful!", type = "message")
          show("appContent")
          hide("authContainer")
          cat("User Info:", toString(user_info), "\n")
        } else {
          showNotification("Failed to fetch user information.", type = "error")
        }
      } else {
        showNotification("Failed to exchange authorization code.", type = "error")
      }
    }
  })
  
  # App logic remains the same
  expense_data <- reactiveVal(data.frame(
    Month = rep(month.abb, each = 4),
    Category = rep(c("Rent", "Groceries", "Utilities", "Entertainment"), times = 12),
    Amount = runif(48, 100, 1000)
  ))
  
  observeEvent(input$file, {
    req(input$file)
    uploaded_data <- read.csv(input$file$datapath)
    expense_data(uploaded_data)
  })
  
  output$expenseTrend <- renderPlot({
    data <- expense_data()
    aggregate_data <- aggregate(Amount ~ Month, data, sum)
    barplot(aggregate_data$Amount, names.arg = aggregate_data$Month, col = "skyblue", main = "Monthly Expense Trend", ylab = "Total Expenses ($)", xlab = "Month")
  })
  
  output$summaryTable <- renderTable({
    data <- expense_data()
    summary <- aggregate(Amount ~ Category, data, sum)
    colnames(summary) <- c("Category", "Total Amount ($)")
    summary
  })
  
  output$categoryTrend <- renderPlot({
    req(input$category)
    data <- expense_data()
    category_data <- subset(data, Category == input$category)
    aggregate_data <- aggregate(Amount ~ Month, category_data, sum)
    plot(aggregate_data$Amount, type = "o", col = "darkgreen", xaxt = "n", main = paste("Trend for", input$category), xlab = "Month", ylab = "Amount ($)")
    axis(1, at = 1:12, labels = month.abb)
  })
  
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