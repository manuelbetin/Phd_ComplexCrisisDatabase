# Table four into UI and server modules.

# UI:

tabFour_UI <- function(id, label = "Table Four"){
  
  # Create a namespace function using the provided id:
  ns <- NS(id)
  
  # Body of the tab.
  # Sidebar contains the choice of comparison (across countries or indexes), two reactive elements and a slider for the choice of time span.
  # For the reactive elements refer to the server equivalent.
  tagList(
  sidebarLayout(
  sidebarPanel(
    radioButtons(ns("choice_comparisonInput"), "Comparison:", c("Across countries", "Across indexes")),
    uiOutput(ns("first_choiceInput")),
    uiOutput(ns("second_choiceInput")),
    sliderInput(ns("timeInput"), "Time range:",
                min(as.numeric(output[["comparison_dataframe"]]$year)), 
                max(as.numeric(output[["comparison_dataframe"]]$year)), 
                c(1960, 2010), sep = "")),
  mainPanel(plotlyOutput(ns("multi_comparison_plot")))
  )
  )
}


# Server:

tabFour <- function(input, output, session){
  
  # Render inputs dinamically: depending on the choice of comparison, different inputs.
  
  output$first_choiceInput <- renderUI({
    
    ns <- session$ns
    
    if(input$choice_comparisonInput == "Across countries"){
      selectizeInput(ns("first_choiceInput"), "Country:", unique(comparison_dataframe$ISO3_Code), selected = "ARG", multiple = TRUE, options = list(maxItems = 8))
    }
    else{
      selectizeInput(ns("first_choiceInput"), "Type of index:", unique(comparison_dataframe$type_index), selected = c("Currency_crisis_severe", "Banking_crisis","Sovereign_default"), multiple = TRUE, options = list(maxItems = 3))
    }
  })
  
  output$second_choiceInput <- renderUI({
    
    ns <- session$ns
    
    if(input$choice_comparisonInput == "Across countries"){
      selectizeInput(ns("second_choiceInput"), "Type of index:", unique(comparison_dataframe$type_index))
    }
    else{
      selectizeInput(ns("second_choiceInput"), "Country:", unique(comparison_dataframe$ISO3_Code), selected = "RUS")
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
        ggplot() +
        geom_line(aes(Year, scale(TF), group = 1, col = ISO3_Code)) +
        geom_point(aes(Year, scale(TF), group = 1, col = ISO3_Code)) +
        scale_colour_discrete(name = "Country") +
        theme(axis.text.x = element_text(angle = 270, hjust = 1)) +
        theme_bw() +
        xlab("Year") + 
        ylab("Standard Deviations") +
        ggtitle("Term Frequency") +
        theme(legend.position = "bottom") +
        theme(axis.text.x = element_text(angle = 270, hjust = 1)) +
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
        ggplot() +
        geom_line(aes(Year, TF, group = 1, col = type_index)) +
        geom_point(aes(Year, TF, group = 1, col = type_index)) +
        scale_colour_discrete(name = "Type of Index") +
        theme_bw() +
        xlab("Year") + 
        ylab("% of characters") +
        ggtitle("Term Frequency") +
        scale_x_continuous(seq(1960,2010, by = 5)) +
        theme(axis.text.x = element_text(angle = 270, hjust = 1)) +
        theme(plot.title = element_text(hjust = 0.5)) +
        theme(legend.position = "bottom")
      
      ggplotly(no_interactive)
    }
    
  }
  )
  
  
  
}
