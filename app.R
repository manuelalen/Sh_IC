library(shiny)
library(ggplot2)
library(jsonlite)  # En caso de tener que manejar JSON. Futuras versiones

# Defino los elementos UI
ui <- fluidPage(
  # Creación del slicer con la que establezco la n deseada.
  sliderInput("n_value", "Select n:", min = 10, max = 10000, value = 100),
  
  # Creación del gráfico.
  plotOutput("intervalPlot")
)

# Defino la lógica del server
server <- function(input, output) {
  output$intervalPlot <- renderPlot({
    # datos en base a la n que he tenido que establecer. Datos de MediaPob y sd de ejemplo
    datos <- generaIntervalos(MediaPob = 6.60, DesvTPob = 0.75, n = input$n_value, numMuestras = 100)
    
    datos$EnIntervalo <- ifelse(datos$Media >= datos$LInferior & datos$Media <= datos$LSuperior, "Yes", "No")
    
    # Getsión del color de los intervalos de confianza en base a si están dentro del 95% o no.
    datos$ErrorColor <- ifelse(datos$LSuperior < 6.6 & datos$LInferior < 6.6, "red",  
                               ifelse(datos$LSuperior > 6.6 & datos$LInferior > 6.6, "red",  
                                      "green"))  
    
    ggplot(datos, aes(Muestra, Media)) +
      geom_point(size = 3, colour = "black") +  
      geom_errorbar(aes(ymin = LInferior, ymax = LSuperior), color = datos$ErrorColor) +  # Color a las líneas
      geom_hline(yintercept = 6.60, linetype = "dashed", color = "blue") +
      geom_ribbon(aes(ymin = LInferior, ymax = LSuperior), fill = datos$ErrorColor, alpha = 0.5) +  
      labs(x = "Muestra", y = "Media", colour = "Dentro del intervalo de confianza (95%)") +
      theme_minimal() +
      scale_colour_manual(values = c("green", "red"))
  })
}


options(jsonlite.warn_named_vec = FALSE)

# Corre la app.
shinyApp(ui = ui, server = server)
