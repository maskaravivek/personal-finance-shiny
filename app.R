library(shiny)
library(shinyjs)

ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$script(src = "https://unpkg.com/@descope/web-component@3.32.0/dist/index.js"),
    tags$script(src = "descope-auth.js")
  ),
  
  titlePanel("Personal Finance Dashboard with Descope Auth"),
  
  # Container for the Descope Authentication flow
  div(id = "authContainer"),
  
  # Main content hidden by default until user logs in
  hidden(
    div(
      id = "appContent",
      sidebarLayout(
        sidebarPanel(
          h3(textOutput("welcomeText")),
          br(),
          
          fileInput("file", "Upload Expense Data (CSV)", accept = ".csv"),
          selectInput("category", "Select Category:", choices = NULL),
          actionButton("analyze", "Analyze"),
          
          actionButton("logout", "Logout", icon = icon("sign-out-alt"))
        ),
        mainPanel(
          tabsetPanel(
            tabPanel("Overview", 
                     plotOutput("expenseTrend"), 
                     tableOutput("summaryTable")),
            tabPanel("Category Analysis", 
                     plotOutput("categoryTrend"), 
                     tableOutput("categoryTable"))
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  session$sendCustomMessage("initDescopeAuth", list(
    projectId = "<YOUR_DESCOPE_PROJECT_ID>",  # <-- Your Descope project ID
    flowId = "sign-up-or-in",                  # <-- Your Descope flow ID
    theme = "light",
    containerId = "authContainer",
    successInput = "user_authenticated",
    errorInput = "auth_error"
  ))
  
  observeEvent(input$user_authenticated, {
    req(input$user_authenticated)
    
    user_info <- input$user_authenticated
    
    # Try "name", else fallback to "email", else "User"
    user_name <- user_info$name
    if (is.null(user_name)) user_name <- user_info$email
    if (is.null(user_name)) user_name <- "User"
    
    output$welcomeText <- renderText({
      paste("Welcome,", user_name)
    })
    
    showNotification("Login successful!", type = "message")
    show("appContent")
  })
  
  observeEvent(input$auth_error, {
    req(input$auth_error)
    showNotification("Authentication failed. Please try again.", type = "error")
  })
  
  observeEvent(input$logout, {
    hide("appContent")  # Hide main content
    session$sendCustomMessage("descopeLogout", list())
    
    session$sendCustomMessage("initDescopeAuth", list(
      projectId = "P2pBl7sYVGg1RWtv4jC7zfO9TnUN",
      flowId = "sign-up-or-in",
      theme = "light",
      containerId = "authContainer",
      successInput = "user_authenticated",
      errorInput = "auth_error"
    ))
  })
  
  # Reactive value for expense data
  expense_data <- reactiveVal(data.frame(
    Month = rep(month.abb, each = 4),
    Category = rep(c("Rent", "Groceries", "Utilities", "Entertainment"), times = 12),
    Amount = runif(48, 100, 1000)
  ))
  
  # Update category choices dynamically
  observe({
    data <- expense_data()
    updateSelectInput(session, "category", choices = unique(data$Category))
  })
  
  # Handle file upload
  observeEvent(input$file, {
    req(input$file)
    uploaded_data <- read.csv(input$file$datapath)
    expense_data(uploaded_data)
  })
  
  # Render the monthly expense trend plot
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
  
  # Render the summary table for total expenses by category
  output$summaryTable <- renderTable({
    data <- expense_data()
    summary <- aggregate(Amount ~ Category, data, sum)
    colnames(summary) <- c("Category", "Total Amount ($)")
    summary
  })
  
  # Render the category-specific trend plot
  output$categoryTrend <- renderPlot({
    req(input$category)
    data <- expense_data()
    category_data <- subset(data, Category == input$category)
    aggregate_data <- aggregate(Amount ~ Month, category_data, sum)
    plot(
      aggregate_data$Amount, type = "o", 
      col = "darkgreen", xaxt = "n", 
      main = paste("Trend for", input$category), 
      xlab = "Month", ylab = "Amount ($)"
    )
    axis(1, at = 1:12, labels = month.abb)
  })
  
  # Render the category-specific table
  output$categoryTable <- renderTable({
    req(input$category)
    data <- expense_data()
    category_data <- subset(data, Category == input$category)
    aggregate_data <- aggregate(Amount ~ Month, category_data, sum)
    colnames(aggregate_data) <- c("Month", "Amount ($)")
    aggregate_data
  })
}

shinyApp(ui, server)
