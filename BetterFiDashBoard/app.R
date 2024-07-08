#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  titlePanel("BetterFi Interactive Dashboard"),
  
  navlistPanel(
    tabPanel("Introduction",
             h3("Here is our introduction")
    ),
    tabPanel("Interactive Graphs",
             h3("Here is our interactive graph panel")
    ),
    tabPanel("Interactive Maps",
             h3("Here is our interactive map panel")
    ),
    tabPanel("Interactive Vunerability Model",
             h3("Here is our interactive model panel")
    ),
    tabPanel("Meet The Team",
             h3("The BetterFi Team")
    ),
    widths = c(2, 10)
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white',
         xlab = 'Waiting time to next eruption (in mins)',
         main = 'Histogram of waiting times')
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
