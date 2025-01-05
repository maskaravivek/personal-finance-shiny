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
