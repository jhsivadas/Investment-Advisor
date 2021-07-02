library(shiny)
library(quantmod)
library(ggplot2)
library(finreportr)
library(DT)
source("PortfolioCreator.R")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Stock Investment Advisor"),
 
    # Gets user input
    sidebarLayout(
        sidebarPanel(
           numericInput("bins", label="What is your Risk Tolerance? 0-10", 
                        value = 0, min = 0, max = 10),
           numericInput("length", label="Shorterm(1) or Longterm(2) Investing?", 
                        value = 0, min = 0, max = 2),
           numericInput("retirement", 
                        label="How many years until your planned retirement?", 
                        value = 0, min = 0, max = 100),
           numericInput("savings", 
                        label="How much money do you have Invested (USD)?", 
                        value = 0, min = 0, max = 1000000),
           numericInput("income", label="What is your annual Income (USD)?", 
                        value = 0, min = 0, max = 10000000),
           numericInput("bonus", label="What is your annual Bonus (USD)?", 
                        value = 0, min = 0, max = 10000000),
           actionButton("goButton", "Go!")
        ),
        

        # Splits information into 3 tabs
        # - Stock Portfolio
        # - Graph
        # - Mean Variance Weights
        mainPanel(
            tabsetPanel(type="tab",
                        tabPanel("Stock List", textOutput("recommended"),
                                               dataTableOutput("stocks")),
                        tabPanel("Stock Chart", 
                                 textInput("search", 
                                           label="Enter a stock to view"),
                                 plotOutput("distPlot")),
                        tabPanel("Mean-Variance Analysis", 
                                 textOutput("loading"),
                                 dataTableOutput("weights")))
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$recommended <- renderText("Recommended Stocks")
    
    # Outputs stock portfolio on first tab
    output$stocks <- renderDataTable({
        input$goButton
        isolate(createRec(input$bins, input$length, input$retirement, 
                          input$savings, input$income, input$bonus))
    })
    
    # Outputs Stock Graph/Chart
    output$distPlot <- renderPlot({
        graphStock(input$search)
    })
    
    # Outputs Mean Variance Weights.
    output$loading <- renderText("If you don't see data, it is loading still")
    output$weights <- renderDataTable({
        input$goButton
        isolate(getWeights(createRec(input$bins, input$length, input$retirement, 
                                     input$savings, input$income, 
                                     input$bonus)[,1]))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)