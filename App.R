library(shiny)


ui <- fluidPage(
  titlePanel(h1('Proyecto Oscoy', align = 'center')),
  sidebarLayout(position = "right",
                sidebarPanel(code("print('Hola Mundo')"),
                selectInput(inputId = 'choice',
                          label = 'Selecciona una opcion',
                          choices = list("Opcion 1" = 1, "Opcion 2" = 2))),
                          mainPanel(p('Mi proyecto se llama DODOFactory'),
                          br(),
                          p('Base de datos comercializadora con operaciones en todo el mundo')
                )
  ))
  
  server <- function(input, output){ }
  shinyApp(ui = ui, server = server)
  