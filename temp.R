library(shiny)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel (
      selectInput( "inState", "Select a field to create histogram", choices = names(dfmerge) )
    ),
  
  
  mainPanel(
    plotOutput("grafico")
  )
)
)

server <- (function(input, output) {
  output$grafico <- renderPlot({
  hist(dfmerge[,input$inState],col = "#75AADB")
  })
  
})

shinyApp(ui, server)
