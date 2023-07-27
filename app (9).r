#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# importing libraries

library(shiny)
library(ggplot2)
library(plotly)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Linear Modeling Script"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(

            
            # Input: Select a file ----
            fileInput("file1", "Choose CSV File",
                      multiple = FALSE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),
            
            # Horizontal line ----
            tags$hr(),
            
            # Input: Checkbox if file has header ----
            checkboxInput("header", "Header", TRUE),
            
            # Input: Select separator ----
            radioButtons("sep", "Separator",
                         choices = c(Comma = ",",
                                     Semicolon = ";",
                                     Tab = "\t"),
                         selected = ","),
            
            # Input: Select quotes ----
            radioButtons("quote", "Quote",
                         choices = c(None = "",
                                     "Double Quote" = '"',
                                     "Single Quote" = "'"),
                         selected = '"'),
            
            # Horizontal line ----
            tags$hr(),
            # Action button for interactive
            actionButton("graphing", "Click here"),
            # Horizontal line ----
            tags$hr(),
            
            # Input: Select number of rows to display ----
            radioButtons("disp", "Display",
                         choices = c(Head = "head",
                                     All = "all"),
                         selected = "head")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotlyOutput("combinedPlot"),
           tableOutput("contents")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    dataInput <- reactive({
        req(input$file1)
        
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
        return(df)
    })
    
    # output$distPlot <- renderPlotly({
    #     # generate bins based on input$bins from ui.R
    #     x    <- linear model[, 2]
    #     bins <- seq(min(x), max(x), length.out = input$bins + 1)
    #     print(bins)
    #     # draw the histogram with the specified number of bins
    #     hist(x, breaks = bins, col = 'darkgray', border = 'white')
    # })
    # 
    
    output$combinedPlot <- renderPlotly({
    gg <- ggplot(data = dataInput(), aes(x = x, y = y)) +
      geom_point(color = 'orange') +
      ggtitle('x vs y') +
      xlab('x') +
      ylab('y')

    if (input$graphing > 0) {
      model <- lm(formula = y ~ x, data = dataInput())
      slope <- coef(model)[2]
      intercept <- coef(model)[1]
      correlation <- cor(dataInput()$x, dataInput()$y)
        
    gg <- gg +
        geom_line(aes(y = predict(model, newdata = dataInput())),
                  color = 'purple') +
        annotate("text", x = 10, y = 11, label = paste("intercept =", intercept)) +
        annotate("text", x = 10, y = 12, label = paste("slope =", slope)) +
        annotate("text", x = 10, y = 13, label = paste("correlation =", round(correlation, 2)))
    }
        ggplotly(gg)
        })
    
    model <- eventReactive(input$graphing, {
        lm(formula = y ~ x,
           data = dataInput())
        })

    output$contents <- renderTable({
        
        # input$file1 will be NULL initially. After the user selects
        # and uploads a file, head of that data file by default,
        # or all rows if selected, will be shown.
        
        
        if(input$disp == "head") {
            return(head(dataInput()))
        }
        else {
            return(dataInput())
        }
        
    })
        
}

# Run the application 
shinyApp(ui = ui, server = server)
