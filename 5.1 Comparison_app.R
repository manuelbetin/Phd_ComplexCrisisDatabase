# Initialise the app:

ui <- fluidPage(theme = shinytheme("paper"),
  navbarPage("A new crisis index: text-mining IMF documents"),
  tabsetPanel(
    # First tab: explanation of methodology
    tabPanel("Methodology"),
    # Second tab: number of events by type and year (Reinhart & Rogoff)
    tabPanel("First dimension: is there a crisis?", 
             sidebarLayout(
             sidebarPanel(
                selectInput("sharecrisisInput", "Type of index:", c("Banking_crisis","Currency_crisis_severe", "Sovereign_default"), selected = "Currency_crisis_severe"),
                radioButtons("sharecrisisInput2", "Database:",unique(output[["comparison_dataframe"]]$database))),
             mainPanel(plotlyOutput("sharecrisis_plot"),
                       tableOutput("share_crisis_correlation"))
             )),
    # Third tab: comparison with standard crisis databases (Figure 2, Romer & Romer).
    tabPanel("First dimension: important differences",
             sidebarLayout(
               sidebarPanel(selectInput("countryInput","Country:", unique(output[["comparison_dataframe"]]$ISO3_Code)),
                            selectInput("typeindexInput", "Type of index:", c("Banking_crisis","Currency_crisis_severe","Sovereign_default"), selected = "Currency_crisis_severe"),
                            radioButtons("crisisdbInput", "Name of Database:", sort(unique(output[["comparison_dataframe"]]$database)), selected = "Reinhart & Rogoff"),
                            uiOutput("crisisInput"),
                            uiOutput("definitionInput")),
               mainPanel(plotlyOutput("comparison_crisis_plot"))
             )),
    # Fourth tab: comparison across countries for single index (Figure 1, Romer & Romer) and across indexes for single country. 
    tabPanel("Second dimension: intensity of the crisis",
              sidebarLayout(
              sidebarPanel(
              radioButtons("choice_comparisonInput", "Comparison:", c("Across countries", "Across indexes")),
              uiOutput("first_choiceInput"),
              uiOutput("second_choiceInput"),
              sliderInput("timeInput", "Time range:",
                          min(as.numeric(output[["comparison_dataframe"]]$year)), 
                          max(as.numeric(output[["comparison_dataframe"]]$year)), 
                          c(1960, 2010), sep = "")),
              mainPanel(plotlyOutput("multi_comparison_plot"))))
  )
)

server <- function(input, output) {
  
  # Second tab: ----
  
  output$sharecrisis_plot <- renderPlotly({
    
    # Filter by type of crisis:
    
    share_crisis <- comparison_dataframe %>%
      filter(type_index %in% input$sharecrisisInput) %>% 
      mutate(year = as.numeric(year))
    
    if(input$sharecrisisInput == "Currency_crisis_severe"){
      share_crisis <- share_crisis %>% 
      filter(type_crisis %in% "Currency Crisis") 
    }
    if(input$sharecrisisInput == "Banking_crisis"){
      share_crisis <- share_crisis %>% 
      filter(type_crisis %in% "Banking Crisis")  
    }  
    if(input$sharecrisisInput == "Sovereign_default"){
      share_crisis <- share_crisis %>% 
        filter(type_crisis %in% "Sovereign Debt Crisis")
    }
    
    share_crisis <- share_crisis %>% 
      filter(database %in% input$sharecrisisInput2) %>% 
      group_by(year) %>% 
      summarise(n.events_own = sum(occurence, na.rm = TRUE), n.events_others = sum(dummy_crisis, na.rm = TRUE))
    
      no_interactive <- share_crisis %>% 
        ggplot(aes(x = n.events_own, y = n.events_others, group = 1)) +
        geom_point(aes(col = "comparison")) +
        geom_smooth(method = "lm") +
        theme_bw() +
        xlab("") +
        ylab("") +
        theme(legend.position = "none") +
        theme(axis.text.x = element_text(angle = 270, hjust = 1))
        
      
      ggplotly(no_interactive)
        
  # The indexes seem to behave all in the same way: weird, probably some mistake to correct.
    
  })
  
  
  output$share_crisis_correlation <- renderTable({
    
    share_crisis <- comparison_dataframe %>%
      filter(type_index %in% input$sharecrisisInput) %>% 
      mutate(year = as.numeric(year))
    
    if(input$sharecrisisInput == "Currency_crisis_severe"){
      share_crisis <- share_crisis %>% 
        filter(type_crisis %in% "Currency Crisis") 
    }
    if(input$sharecrisisInput == "Banking_crisis"){
      share_crisis <- share_crisis %>% 
        filter(type_crisis %in% "Banking Crisis")  
    }  
    if(input$sharecrisisInput == "Sovereign_default"){
      share_crisis <- share_crisis %>% 
        filter(type_crisis %in% "Sovereign Debt Crisis")
    }
    
    share_crisis <- share_crisis %>% 
      filter(database %in% input$sharecrisisInput2) %>% 
      group_by(year) %>% 
      summarise(n.events_own = sum(occurence, na.rm = TRUE), n.events_others = sum(dummy_crisis, na.rm = TRUE))
    
      correlation <- data.frame(
        cor(share_crisis$n.events_own, share_crisis$n.events_others)
      )
      names(correlation) <- "Correlation share of countries:"
      correlation
      
  }, striped = TRUE)
  

  # Third tab: -----
  
  # Render inputs dinamically: depending on the choice of comparison, different inputs.
  
  output$first_choiceInput <- renderUI({
    if(input$choice_comparisonInput == "Across countries"){
    selectizeInput("first_choiceInput", "Country:", unique(comparison_dataframe$ISO3_Code), selected = "ARG", multiple = TRUE, options = list(maxItems = 8))
    }
    else{
      selectizeInput("first_choiceInput", "Type of index:", unique(comparison_dataframe$type_index), selected = c("Currency_crisis_severe", "Banking_crisis","Sovereign_default"), multiple = TRUE, options = list(maxItems = 3))
    }
  })
  
  output$second_choiceInput <- renderUI({
    if(input$choice_comparisonInput == "Across countries"){
      selectizeInput("second_choiceInput", "Type of index:", unique(comparison_dataframe$type_index))
    }
    else{
      selectizeInput("second_choiceInput", "Country:", unique(comparison_dataframe$ISO3_Code), selected = "RUS")
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
        ggplot(aes(Year, TF, group = 1, col = type_index)) +
        geom_line() +
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
  
  # Third tab: -----
  
  # List of crises type depends on choice of database:
  
  output$crisisInput <- renderUI({
    if(input$crisisdbInput == "Laeven & Valencia"){
      radioButtons("crisisInput", "Type of Crisis:",c("Banking Crisis", "Currency Crisis","Sovereign Debt Crisis"), selected = "Currency Crisis")
    }else{
      radioButtons("crisisInput", "Type of Crisis:",c("Banking Crisis", "Currency Crisis", "Sovereign Debt Crisis",
                                                      "Domestic Sovereign Debt Crisis",
                                                      "External Sovereign Debt Crisis"), selected = "Currency Crisis")
    }
  })

  # Part of description crises:

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
                "Nominal exchange rate vis-a-vis US dollar or relevant anchor - Eop",br(),br(),
                "Sample:", br(),
                "1946-2010",br(),br(),
                "Necessary condition:",br(),
                "Year-on-year 15% depreciation",br(),br(),
                "Consecutive episodes:", br(),
                "No treatment.", br(),
                "Crisis ends when exchange rate stabilizes.")
       }
       else{
       helpText(strong("Definition"), br(), br(),
                "Data:",br(),
                "Nominal exchange rate vis-a-vis US dollar - Eop.", br(),br(),
                "Sample:",br(),
                "1970-2017", br(),br(),
                "Necessary conditions:",br(),
                "- Year-on-year 30% depreciation",br(),
                "- 10% points higher than previous year",br(),br(),
                "Consecutive episodes:", br(),
                "First of five-year consecutive window")
       }
     }
   })
 
  output$comparison_crisis_plot <- renderPlotly({
    
    # Rendering of input takes a while: customize error message to be blank.
    
    validate(need(input$crisisInput != "","")) 
    
    # Filtering:
    
    filtered <- comparison_dataframe %>%
      filter(ISO3_Code == input$countryInput,
             type_index == input$typeindexInput,
             database == input$crisisdbInput, 
             type_crisis == input$crisisInput) %>% 
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

