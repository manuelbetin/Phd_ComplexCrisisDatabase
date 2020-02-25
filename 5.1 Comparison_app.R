# Move package loading in the main section:

library(shiny)
library(plotly)
# Initialise the app:

ui <- fluidPage(
  tabsetPanel(
    # First tab: introduction to the index.
    tabPanel("A new crisis index: text-mining IMF documents"),
    # Second tab: comparison across countries for single index (Figure 1, Romer & Romer) and across indexes for single country. 
    tabPanel("Comparison: cross-country and cross-index",
              sidebarLayout(
              sidebarPanel(
              radioButtons("choice_comparisonInput", "Comparison:", c("Across countries", "Across indexes")),
              uiOutput("first_choiceInput"),
              uiOutput("second_choiceInput"),
              sliderInput("timeInput", "Time range:",
                          min(as.numeric(output[["comparison_dataframe"]]$year)), 
                          max(as.numeric(output[["comparison_dataframe"]]$year)), 
                          c(1960, 2010), sep = "")),
              mainPanel(plotlyOutput("multi_comparison_plot")))),
    # Third tab: comparison with standard crisis databases (Figure 2, Romer & Romer).
    tabPanel("Comparison: standard crisis databases",
    sidebarLayout(
    sidebarPanel(selectInput("countryInput","Country:", unique(output[["comparison_dataframe"]]$ISO3_Code)),
    selectInput("typeindexInput", "Type of index:", c("Banking_crisis","Currency_crisis_severe","Sovereign_default")),
    radioButtons("crisisInput", "Type of Crisis:",sort(unique(output[["comparison_dataframe"]]$type_crisis))),
    radioButtons("crisisdbInput", "Name of Database:", sort(unique(output[["comparison_dataframe"]]$database))),
    uiOutput("definitionInput")),
    mainPanel(plotlyOutput("comparison_crisis_plot"))
  ))
  )
)

server <- function(input, output) {
  
  
  # Second tab: -----
  # Render inputs dinamically: depending on the choice of comparison, different inputs.
  
  output$first_choiceInput <- renderUI({
    if(input$choice_comparisonInput == "Across countries"){
    selectInput("first_choiceInput", "Country:", unique(comparison_dataframe$ISO3_Code), selected = "ARG", multiple = TRUE)
    }
    else{
      selectInput("first_choiceInput", "Type of index:", unique(comparison_dataframe$type_index), selected = "Balance_payment_crisis", multiple = TRUE)
    }
  })
  
  output$second_choiceInput <- renderUI({
    if(input$choice_comparisonInput == "Across countries"){
      selectInput("second_choiceInput", "Type of index:", unique(comparison_dataframe$type_index))
    }
    else{
      selectInput("second_choiceInput", "Country:", unique(comparison_dataframe$ISO3_Code))
    }
  })
  
  # Plots:

  output$multi_comparison_plot <- renderPlotly({
    
    if(input$choice_comparisonInput == "Across countries"){
    
    # Rendering of input takes a while: customize error message to be blank.
      
    validate(need(input$first_choiceInput != "",""))  
      
    filtered <- comparison_dataframe %>% 
      mutate(year = as.numeric(year)) %>% 
      filter(ISO3_Code == input$first_choiceInput,
        type_index == input$second_choiceInput,
        year >= input$timeInput[1],
        year <= input$timeInput[2]) %>% 
      rename(TF = value,
            Year = year)

    
   no_interecative <- filtered %>% 
      ggplot(aes(Year, scale(TF), group = 1, col = ISO3_Code)) +
      geom_line() +
      scale_colour_discrete(name = "Country") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
     theme_bw() +
     xlab("Year") + 
     ylab("Standard Deviations") +
     ggtitle("Term Frequency") +
     theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
     theme(plot.title = element_text(hjust = 0.5)) 

   ggplotly(no_interecative)
  
    }
    else{
      
      # Rendering of input takes a while: customize error message to be blank.
      
      validate(need(input$first_choiceInput != "",""))
      
      filtered <- comparison_dataframe %>% 
        mutate(value = as.numeric(value)*100,
               year = as.numeric(year)) %>% 
        filter(type_index == input$first_choiceInput,
                ISO3_Code == input$second_choiceInput,
               year >= input$timeInput[1],
               year <= input$timeInput[2]) %>% 
        rename(TF = value,
               Year = year)
      
      no_interactive <- filtered %>% 
        ggplot(aes(Year, TF, group = 1, col = type_index)) +
        geom_line() +
        scale_colour_discrete(name = "Type of Index") +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        theme_bw() +
        xlab("Year") + 
        ylab("% of characters") +
        ggtitle("Term Frequency") +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        theme(plot.title = element_text(hjust = 0.5)) 
      
      ggplotly(no_interactive)
    }

  }
  )
  
  # Third tab: -----
  
  output$definitionInput <- renderUI({
    if(input$crisisInput == "Banking Crisis"){
      if(input$crisisdbInput == "Reinhart & Rogoff"){
      helpText("Definition: no data available.")
      }
      else{
        helpText("Not yet written")
      }
    }
    if(input$crisisInput == "Currency Crisis"){
      if(input$crisisdbInput == "Reinhart & Rogoff"){
      helpText(strong("Definition"), br(), br(),
               "Data:",br(),
               "Nominal exchange rate vis-a-vis US dollar or relevant anchor, eop.", br(),br(),
               "Two necessary conditions:",br(),
               "1) Year-on-year 15% depreciation",br(),
               "2) 10% points higher than previous year",br(),br(),
               "Consecutive episodes:", br(),
               "First of five-year consecutive window")
      }
      else{
      helpText(strong("Definition"), br(), br(),
               "Data:",br(),
               "Nominal exchange rate vis-a-vis US dollar, eop.", br(),br(),
               "Two necessary conditions:",br(),
               "1) Year-on-year 30% depreciation",br(),
               "2) 10% points higher than previous year",br(),br(),
               "Consecutive episodes:", br(),
               "First of five-year consecutive window")
      }
    }
  })
 
  output$comparison_crisis_plot <- renderPlotly({
    
    filtered <- comparison_dataframe %>%
      filter(ISO3_Code == input$countryInput,
             type_index == input$typeindexInput,
             type_crisis == input$crisisInput,
             database == input$crisisdbInput) %>% 
      mutate(value = as.numeric(value*100),
             year = as.numeric(year)) %>% 
      rename(TF = value,
             Year = year)
    
    # Substitute dummy with corresponding year.
    
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

