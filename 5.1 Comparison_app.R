# Move package loading in the main section:

library(shiny)
library(plotly)
# Initialise the app:

ui <- fluidPage(
  titlePanel("A new crisis index: text-mining IMF documents"),
  # For the moment simple layout:
  sidebarLayout(
    sidebarPanel(selectInput("countryInput","Country:", unique(output[["comparison_dataframe"]]$ISO3_Code)),
    selectInput("typeindexInput", "Type of index:", unique(output[["comparison_dataframe"]]$type_index)),
    radioButtons("crisisInput", "Type of Crisis:",unique(output[["comparison_dataframe"]]$type_crisis)),
    radioButtons("crisisdbInput", "Name of Database:", unique(output[["comparison_dataframe"]]$database))),
    mainPanel(plotlyOutput("index_plot"))
  ))

server <- function(input, output) {
 
  output$index_plot <- renderPlotly({
    
    filtered <- comparison_dataframe %>%
      filter(ISO3_Code == input$countryInput,
             type_index == input$typeindexInput,
             type_crisis == input$crisisInput,
             database == input$crisisdbInput) %>% 
      mutate(value = as.numeric(value*100),
             year = as.numeric(year)) %>% 
      rename(TF = value,
             Year = year)
    
    filtered$dummy_crisis <- ifelse(filtered$dummy_crisis > 0,
                                                filtered$Year,
                                                NA)
  no_interactive <- filtered %>% 
    ggplot(aes(Year, TF, group = 1, col = input$typeindexInput)) +
      geom_line() +
      geom_vline(aes(xintercept = as.numeric(filtered$dummy_crisis), col = unique(filtered$database)), linetype = "dotted") +
      theme_bw() +
      xlab("Year") + 
      ylab("% of characters") +
      ggtitle("Term Frequency") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      theme(plot.title = element_text(hjust = 0.5)) +
      theme(legend.title=element_blank()) 
      
  ggplotly(no_interactive)
  })
}
shinyApp(ui = ui, server = server)

