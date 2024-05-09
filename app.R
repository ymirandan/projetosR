library(shiny)
library(ggplot2)
#install.packages("corrplot")
#install.packages("RColorBrewer")
#library("corrplot")
#library(RColorBrewer)

#diamonds: A data frame with 53940 rows and 10 variables:
# price: price in US dollars ($326--$18,823)
#carat:weight of the diamond (0.2--5.01)
#cut:quality of the cut (Fair, Good, Very Good, Premium, Ideal)
#color:diamond colour, from D (best) to J (worst)
#clarity:a measurement of how clear the diamond is (I1 (worst), SI2, SI1, VS2, VS1, VVS2, VVS1, IF (best))
#x:length in mm (0--10.74)
#y:width in mm (0--58.9)
#z:depth in mm (0--31.8)
#depth:total depth percentage = z / mean(x, y) = 2 * z / (x + y) (43--79)
#table:width of top of diamond relative to widest point (43--95)

ui = fluidPage(
  "Análise da Base de Dados 'Diamond' do pacote ggplot2, com dados referentes a mais de 53 mil diamantes",
  sliderInput(
    inputId = "preco",
    label = "Selecione o preço do diamante ($)",
    min = min(diamonds$price),
    max = max(diamonds$price),
    value =c(2000,2020)),
    textOutput(outputId="SliderPreco"),
  selectInput(
    inputId = "variaveis",
    label = "Selecione a variavel para visualizar seu grafico",
    choices = names(diamonds)),
  plotOutput(outputId = "barplot")
)

server <- function(input, output, session) {
  my_range <- reactive({
    cbind(input$preco[1], input$preco[2])
  })
  
  output$SliderPreco <- renderText({
    paste("Intervalo do Preço:", my_range()[1], "-", my_range()[2])
  })
  
  output$barplot <- renderPlot({
    filtered_data <- diamonds[diamonds$price >= my_range()[1] & diamonds$price <= my_range()[2], ]
    plot(diamonds[[input$variaveis]])
    })
}
shinyApp(ui = ui, server = server)