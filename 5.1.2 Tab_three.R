# Table three UI and server into modules.

tabThree_UI <- function(id, label = "Table Three"){
  
  # Create a namespace function using the provided id:
  ns <- NS(id)
  
  # Body of the tab.
  # Description:
  # Side panel contains choice of country of interest, type of index of interest, name of the database we want to compare to and two reactive elements.
  # Refer to the server function of the module for the reactive elements.
  tagList(
    sidebarLayout(
      sidebarPanel(selectInput(ns("countryInput"),"Country:", unique(output[["comparison_dataframe"]]$ISO3_Code)),
                   selectInput(ns("typeindexInput"), "Type of index:", c("Banking_crisis","Currency_crisis_severe","Sovereign_default"), selected = "Currency_crisis_severe"),
                   radioButtons(ns("crisisdbInput"), "Name of Database:", sort(unique(output[["comparison_dataframe"]]$database)), selected = "Reinhart & Rogoff"),
                   uiOutput(ns("crisisInput")),
                   uiOutput(ns("definitionInput"))),
      mainPanel(plotlyOutput(ns("comparison_crisis_plot")))
    )
  )
}


tabThree <- function(input, output, session){
  
  # List of crises type depends on choice of database (for RR we have the division between domestic and external default): ----
  
  output$crisisInput <- renderUI({
    
    ns <- session$ns
    
    if(input$crisisdbInput == "Laeven & Valencia"){
      radioButtons(ns("crisisInput"), "Type of Crisis:",c("Banking Crisis", "Currency Crisis","Sovereign Debt Crisis"), selected = "Currency Crisis")
    }else{
      radioButtons(ns("crisisInput"), "Type of Crisis:",c("Banking Crisis", "Currency Crisis", "Sovereign Debt Crisis",
                                                      "Domestic Sovereign Debt Crisis",
                                                      "External Sovereign Debt Crisis"), selected = "Currency Crisis")
    }
  })
  
  # Part of description crises in the database of comparison: -----
  # Data used, coverage, criterion for identification event and strategy to cope with multiple subsequent episodes.
  
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
  
  # Plot comparison, index chosen and vertical lines for comparison databases dummies: -----
  
  
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
      ggplot() +
      geom_line(aes(Year, TF, group = 1, col = input$typeindexInput)) +
      geom_point(aes(Year, TF, group = 1, col = input$typeindexInput)) +
      geom_vline(aes(xintercept = as.numeric(filtered$dummy_crisis), col = unique(filtered$database)), linetype = "dotted") +
      theme_bw() +
      xlab("") + 
      ylab("") +
      ggtitle("") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      theme(plot.title = element_text(hjust = 0.5)) +
      theme(legend.title=element_blank()) +
      theme(legend.position = "none")
    
    ggplotly(no_interactive)
  })  
}

