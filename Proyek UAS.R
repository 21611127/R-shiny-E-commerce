library(shiny)
library(shinydashboard)
library(plotly)
library(ggplot2)
library(gridExtra)
library(shinythemes)
library(lmtest)
library(car)
library(rsconnect)

dataset <- data.frame(
  Month = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
  X1 = c(150000, 160000, 170000, 180000, 190000, 200000, 210000, 220000, 230000, 240000, 250000, 260000),
  X2 = c(8000, 9500, 10000, 10500, 11000, 9000, 11500, 12000, 12500, 13000, 14000, 15000),
  X3 = c(5.0, 4.5, 4.8, 4.6, 5.1, 4.7, 4.9, 5.0, 5.2, 5.3, 5.4, 5.5),
  X4 = c(8.5, 8.2, 8.4, 8.5, 8.6, 8.7, 8.8, 8.9, 8.7, 8.8, 8.9, 9.0),
  X5 = c(20000, 22000, 25000, 23000, 30000, 28000, 27000, 35000, 40000, 45000, 50000, 60000),
  Y = c(120, 150, 160, 165, 180, 170, 190, 210, 230, 250, 300, 350)
)

#Create a multiple linear regression model
model <- lm(Y ~ X1 + X2 + X3 + X4 + X5, data = dataset)

#Calculating R-squared
r_squared <- summary(model)$r.squared * 100  # Mengonversi R-squared menjadi persen

#Model interpretation function
model_interpretation <- function() {
  interpretation <- "This multiple linear regression analysis shows that five variables, namely the number of website visitors, number of transactions, average number of items per transaction, customer satisfaction rating, and number of online advertisements, have a significant influence on sales. A one unit increase in each of these variables is expected to increase sales."
  interpretation
}

# UI for Shiny Dashboard
ui <- dashboardPage(
  dashboardHeader(title = "E-Commerce Sales"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data", tabName = "data", icon = icon("database")),
      menuItem("Variable Relationships", tabName = "relationship", icon = icon("line-chart")),
      menuItem("Model & Assumption", tabName = "regression", icon = icon("list-alt"), 
              menuSubItem("Multiple Linier Regression", tabName = "model"),
              menuSubItem("Assumption Tests", tabName = "assumption_tests")),
      menuItem("Prediction", tabName = "prediction", icon = icon("bar-chart"))
    )
  ),
  dashboardBody(
    tabItems(
      ##Menu Data
      tabItem(
        titlePanel(title = div("This is a R Shiny for Estimate a Multiple Linear Regression Equation", 
                               style = "color: #000000; font-size: 40px; font-weight: bold; text-align: center; height: 120px")),
        tabName = "data",
        fluidRow(
          h5("This data is used to analyze e-commerce companies operating in several Southeast Asian countries"),
          h5("The data collected includes the following variables:"),
          
          h5("X1 = Number of Website Visitors per Monthly"),
          h5("X2 = Number of Monthly Transactions"),
          h5("X3 = Average Number of Items per Transaction"),
          h5("X4 = Customer Satisfaction Rating"),
          h5("X5 = Number of Online Advertisements Run per Month"),
          h5("Y = Monthly Sales Volume"),
          
          column(
            width = 12,
            h5("The data from the last twelve months is compiled in the table below:"),
            tableOutput("table_dataset")
          ),
          
          h5("Summury of Dataset"),
          verbatimTextOutput("desc_stats_output")
        ),
      ),
      ##menu Variable Relationships
      tabItem(
        titlePanel(title = div("Variable Relationship Analysis", 
                               style = "color: #000000; font-size: 40px; font-weight: bold; text-align: center; height: 120px")),
        tabName = "relationship",
        sidebarLayout(
          sidebarPanel(
            selectInput("variable", "Select Variable:", choices = colnames(dataset)),
            actionButton(
              "plot", 
              "Generate Plot", 
              style = "background-color: #000000	; 
              color: white; margin-top: 20px;")
          ),
          mainPanel(
            plotOutput("scatter", height = "800px")
          )
        )
      ),
      ##Menu Multiple Linier Regression
      tabItem(
        titlePanel(title = div("Multiple Linier Regression", 
                               style = "color: #000000; font-size: 40px; font-weight: bold; text-align: center; height: 120px")),
        tabName = "model", 
              fluidRow(
                column(6, verbatimTextOutput("summary")),
                column(6, plotOutput("residualPlot")),
                column(
                  width = 12,
                  h4("Model Accuracy"),
                  uiOutput("model_accuracy")),
                column(
                  width = 12,
                  h4("Interpretation of Regression Models"),
                  div(
                    model_interpretation(),
                    class = "justified-text"))
              )
      ),
      ##Assumption Tests
      tabItem(
        titlePanel(title = div("Assumption Tests", 
                               style = "color: #000000; font-size: 40px; font-weight: bold; text-align: center; height: 120px")),
        tabName = "assumption_tests", 
              fluidRow(
                column(6,
                       plotOutput("normalityPlot"),
                       verbatimTextOutput("normalityTest")
                ),
                column(6,
                       plotOutput("multicollinearityPlot"),
                       verbatimTextOutput("multicollinearityTest")
                ),
                column(6,
                       plotOutput("heteroskedasticityPlot"),
                       verbatimTextOutput("heteroskedasticityTest")
                )
              )
      ),
      ##Prediction
      tabItem(
        titlePanel(title = div("Prediction", 
                               style = "color: #000000; font-size: 40px; font-weight: bold; text-align: center; height: 120px")),
        tabName = "prediction",
            fluidRow(
              column(
                width = 6,
                h4("Enter Prediction Data"),
                numericInput(
                  "X1",
                  "Number of Website Visitors per Monthly:",
                  value = 250000
                ),
                numericInput(
                  "X2",
                  "Number of Monthly Transactions:",
                  value = 12000
                ),
                numericInput(
                  "X3",
                  "Average Number of Items per Transaction:",
                  value = 4.5,
                  step = 0.1
                ),
                sliderInput(
                  "X4",
                  "Customer Satisfaction Rating:",
                  min = 1,
                  max = 10,
                  value = 9,
                  step = 0.1
                ),
                numericInput(
                  "X5",
                  "Number of Online Advertisements Run per Month:",
                  value = 27000
                ),
                actionButton(
                  "predictButton", 
                  "Predict Sales", 
                  style = "background-color: #333333	; 
                color: white; margin-top: 20px;")
              ),
              column(
                width = 6,
                div(class = "separator"),
                h4("Table Prediction Results"),
                tableOutput("predicted_table"),
                br(),
                h4("Prediction Results"),
                textOutput("predictedSales")
              )
            )
      )
    )
  )
)


# Server for Shiny Dashboard
server <- function(input, output, session) {
  #####Menu Data#####
  output$desc_stats_output <- renderPrint({
    summary(dataset)
  })
  
  output$table_dataset <- renderTable({
    table <- dataset
    table
  })
  
  output$model_coefficients <- renderTable({
    summary(model)
  })
  
  #####Menu Variable Relationship#####
  # Variable Relationship
  observeEvent(input$plot, {
    variable <- input$variable
    
    output$scatter <- renderPlot({
      
      scatter_plot <- ggplot(dataset, aes_string(x = variable, y = "Y")) +
        geom_point(color = "#333333", size = 5) +
        geom_smooth(method = "lm", se = FALSE, color = "#FF5722") +
        labs(x = variable, y = "Y", title = paste("Scatter Plot of", variable, "vs. Y")) +
        theme_minimal() +
        theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5, color = "#333333"),
              axis.title = element_text(size = 14),
              axis.text = element_text(size = 12),
              legend.position = "none")
      grid.arrange(scatter_plot, nrow = 1)
    })
  })
  
  #####Menu Model#####
  # Regression Model
  output$summary <- renderPrint({
    summary(model)
  })
  
  output$residualPlot <- renderPlot({
    plot(model, which = 1)
  })
  
  output$model_accuracy <- renderUI({
    strong(paste(round(r_squared, 2), "%"), class = "model-accuracy")
  })
  
  #####Menu Assumption Tests#####
  output$normalityTest <- renderPrint({
    shapiro.test(model$residuals)
  })
  
  output$normalityPlot <- renderPlot({
    ggplot(dataset, aes(sample = model$residuals)) +
      geom_qq() +
      geom_qq_line() +
      theme_minimal() +
      theme(panel.background = element_rect(fill = "lightblue", color = "darkblue")) +
      ggtitle("Normality Assumption")
  })
  
  output$multicollinearityTest <- renderPrint({
    vif(model)
  })
  
  output$multicollinearityPlot <- renderPlot({
    plot(model)
  })
  
  output$heteroskedasticityTest <- renderPrint({
    bptest(model)
  })
  
  output$heteroskedasticityPlot <- renderPlot({
    ggplot(dataset, aes(x = fitted(model), y = sqrt(abs(model$residuals)))) +
      geom_point() +
      geom_smooth() +
      theme_minimal() +
      theme(panel.background = element_rect(fill = "lightblue", color = "darkblue")) +
      ggtitle("Homoskedasticity Assumption")
  })
  
  #####Menu Prediction#####
  output$predicted_table <- renderTable({
    data.frame(
      Variabel = c(
        "Number of Website Visitors per Monthly",
        "Number of Monthly Transactions",
        "Average Number of Items per Transaction",
        "Customer Satisfaction Rating",
        "Number of Online Advertisements Run per Month"
      ),
      "Nilai" = c(
        input$X1,
        input$X2,
        input$X3,
        input$X4,
        input$X5
      )
    )
  })
  
  observeEvent(input$predictButton, {
    user_input <- data.frame(
      X1 = input$X1,
      X2 = input$X2,
      X3 = input$X3,
      X4 = input$X4,
      X5 = input$X5
    )
    
    predicted_sales <- predict(model, newdata = user_input)
    
    output$predictedSales <- renderText(paste("Predicted Sales: $", round(predicted_sales, 3)))
  })
}
  

shinyApp(ui = ui, server = server)

