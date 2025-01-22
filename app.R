library(shiny)
library(shinyjs)
library(httr)

# UI Definition
ui <- fluidPage(
  useShinyjs(), # For dynamic UI control
  titlePanel("Personal Finance Dashboard with Descope OIDC Auth"),
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

# Server Definition
server <- function(input, output, session) {
  # Reactive value to hold the expense data
  expense_data <- reactiveVal(data.frame(
    Month = rep(month.abb, each = 4),
    Category = rep(c("Rent", "Groceries", "Utilities", "Entertainment"), times = 12),
    Amount = runif(48, 100, 1000)
  ))
  
  # Update expense data when a file is uploaded
  observeEvent(input$file, {
    req(input$file)
    uploaded_data <- read.csv(input$file$datapath)
    expense_data(uploaded_data)
  })
  
  # Dynamically update category choices in the selectInput
  observe({
    data <- expense_data()
    updateSelectInput(session, "category", choices = unique(data$Category))
  })
  
  # Render the monthly expense trend plot
  output$expenseTrend <- renderPlot({
    data <- expense_data()
    aggregate_data <- aggregate(Amount ~ Month, data, sum)
    barplot(aggregate_data$Amount, names.arg = aggregate_data$Month, col = "skyblue", main = "Monthly Expense Trend", ylab = "Total Expenses ($)", xlab = "Month")
  })
  
  # Render the summary table for total amount per category
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
    plot(aggregate_data$Amount, type = "o", col = "darkgreen", xaxt = "n", main = paste("Trend for", input$category), xlab = "Month", ylab = "Amount ($)")
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

# Run the app
shinyApp(ui, server)
