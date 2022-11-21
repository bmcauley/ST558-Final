# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.


library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  genshin <- read.csv("genshin.csv")
  
  getData <- reactive({
    newData <- genshin
  })
  
  #create output of observations    
  output$table <- renderTable({
    getData()
  })

})
