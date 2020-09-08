library(shiny)


ui <- fluidPage(
  titlePanel("Server"),
  sidebarLayout(
    sidebarPanel(
      textInput(inputId = 'textoentrada',
                label = 'Escribe algo')),
    mainPanel(
    textOutput(outputId = 'textosalida'),
    plotOutput(outputId = 'plot_1')
  )))

server <- function(input, output)
  
  output$textosalida <- renderText(input$textoentrada)

shinyApp(ui = ui, server = server)

install.packages('shinythemes')
