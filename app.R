library(shiny)

# Sample data for demonstration
sample_data <- data.frame(
  Month = rep(month.abb, each = 4),
  Category = rep(c("Rent", "Groceries", "Utilities", "Entertainment"), times = 12),
  Amount = runif(48, 100, 1000)
)

# UI Definition
ui <- fluidPage(
  titlePanel("Personal Finance Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload Expense Data (CSV)", accept = ".csv"),
      selectInput("category", "Select Category:", choices = unique(sample_data$Category), selected = "Rent"),
      actionButton("analyze", "Analyze")
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

# Server Definition
server <- function(input, output, session) {
  # Reactive to hold uploaded or default data
  expense_data <- reactiveVal(sample_data)
  
  observeEvent(input$file, {
    req(input$file)
    uploaded_data <- read.csv(input$file$datapath)
    expense_data(uploaded_data)
  })
  
  # Expense Trend Plot
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
  
  # Summary Table
  output$summaryTable <- renderTable({
    data <- expense_data()
    summary <- aggregate(Amount ~ Category, data, sum)
    colnames(summary) <- c("Category", "Total Amount ($)")
    summary
  })
  
  # Category Trend Plot
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
  
  # Category Table
  output$categoryTable <- renderTable({
    req(input$category)
    data <- expense_data()
    category_data <- subset(data, Category == input$category)
    aggregate_data <- aggregate(Amount ~ Month, category_data, sum)
    colnames(aggregate_data) <- c("Month", "Amount ($)")
    aggregate_data
  })
}

# Run the Shiny App
shinyApp(ui, server)
