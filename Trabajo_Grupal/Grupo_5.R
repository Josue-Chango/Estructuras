library(shiny)
library(ggplot2)
library(dplyr)
library(readxl)
library(ISLR)

# Pruebas de calidad De software(Fornite)
# ruta del archivo

file_path <- "C:/Users/LeonixKPG/Documents/estadistica/Trabajo_Grupal/BBD_Grupo_5.xlsx"

quality_data <- read_excel(file_path)
# Interfaz de usuario
ui <- fluidPage(
  titlePanel("Análisis Descriptivo de Pruebas de Calidad En fornite"),
  sidebarLayout(
    sidebarPanel(
      selectInput("variable", "Seleccione una variable:",
                  choices = c("consumo de energia", "flujos", "nucleos")),
      checkboxInput("show_summary", "Mostrar resumen estadístico", TRUE),
      checkboxInput("show_plot", "Mostrar gráficos", TRUE)
    ),
    mainPanel(
      h3("Resumen Estadístico"),
      tableOutput("summary_table"),
      h3("Gráficos"),
      plotOutput("histogram"),
      plotOutput("boxplot")
    )
  )
)

# Lógica del servidor
server <- function(input, output) {
  
  # Resumen estadístico
  
  output$summary_table <- renderTable({
    if (input$show_summary) {
      quality_data %>%
        summarize(
          Media = mean(.data[[input$variable]], na.rm = TRUE),
          Mediana = median(.data[[input$variable]], na.rm = TRUE),
          `Desviación Estándar` = sd(.data[[input$variable]], na.rm = TRUE),
          Mínimo = min(.data[[input$variable]], na.rm = TRUE),
          Máximo = max(.data[[input$variable]], na.rm = TRUE)
        )
    }
  })
  
  # Histograma
  output$histogram <- renderPlot({
    if (input$show_plot) {
      ggplot(quality_data, aes(x = .data[[input$variable]])) +
        geom_histogram(binwidth = 10, fill = "blue", color = "black") +
        theme_minimal() +
        labs(title = paste("Histograma de", input$variable),
             x = input$variable,
             y = "Frecuencia")
    }
  })
  
  # Boxplot
  output$boxplot <- renderPlot({
    if (input$show_plot) {
      ggplot(quality_data, aes(y = .data[[input$variable]], x = factor(1))) +
        geom_boxplot(fill = "orange") +
        theme_minimal() +
        labs(title = paste("Boxplot de", input$variable),
             x = "",
             y = input$variable)
    }
  })
}

# Correr la aplicación
shinyApp(ui = ui, server = server)