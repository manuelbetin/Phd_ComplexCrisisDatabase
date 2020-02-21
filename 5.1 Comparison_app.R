# Move package loading in the main section:

library(shiny)

# Initialise the app:

ui <- fluidPage(
  titlePanel("A new crisis index: text-mining IMF documents"),
  # For the moment simple layout:
  sidebarLayout(
    sidebarPanel(selectInput("countryInput","Country:", output[["comparison_dataframe"]]$ISO3_Code)),
    radioButtons("indexInput", "Type of index:", c("currency"))),
    mainPanel("Results")
  )
server <- function(input, output) {}
shinyApp(ui = ui, server = server)

