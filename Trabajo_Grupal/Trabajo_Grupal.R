library(shiny)
library(readxl)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Pruebas de Calidad de Software"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Sube tu archivo Excel:", accept = c(".xlsx", ".xls")),
      uiOutput("column_select"),
      actionButton("plot_button", "Generar Gráfico")
    ),
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

server <- function(input, output, session) {
  data <- reactive({
    req(input$file)
    df <- read_excel(input$file$datapath)
    names(df) <- make.names(names(df))
    print(str(df))  # Depuración
    df
  })
  
  output$column_select <- renderUI({
    req(data())
    selectInput("selected_column", "Selecciona una columna:", choices = names(data()))
  })
  
  observeEvent(input$plot_button, {
    print("Botón clickeado")  # Confirmar clic en consola
    output$distPlot <- renderPlot({
      req(input$selected_column)
      column_data <- data()[[input$selected_column]]
      req(is.numeric(column_data))
      ggplot(data(), aes_string(x = input$selected_column)) +
        geom_histogram(binwidth = 10, fill = "blue", color = "black") +
        labs(title = "Histograma de la Columna Seleccionada", 
             x = input$selected_column, 
             y = "Frecuencia")
    })
  })
}

shinyApp(ui = ui, server = server)
