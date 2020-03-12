  # First experiments with modules representation Shiny app.

# UI


tabTwo_UI <- function(id, label = "Table Two"){
  
  # Create a namespace function using the provided id:
  ns <- NS(id)
  
  # Body of the tab.
  # Description:
  # Side panel contains choice of type index in cascade manner (choices are banking crisis,
  # currency crisis and sovereign default) and choice of database to which compare.
  # Main panel contains scatter plot by year and correlation tab.
  
  tagList(
    sidebarLayout(
    sidebarPanel(
    selectInput(ns("sharecrisisInput"), "Type of index:", c("Banking_crisis_severe","Currency_crisis_severe", "Sovereign_default"), selected = "Currency_crisis_severe"),
    radioButtons(ns("sharecrisisInput2"), "Database:",unique(output[["comparison_dataframe"]]$database))
    ),
    mainPanel(plotlyOutput(ns("sharecrisis_plot")),
              tableOutput(ns("share_crisis_correlation"))
    )
  )
)
}


# Server function

tabTwo <- function(input, output, session){
  
  
  # First output component, scatterplot. -----
  
  output$sharecrisis_plot <- renderPlotly({
    
    # Filter by type of index:
    
    share_crisis <- comparison_dataframe %>%
      filter(type_index %in% input$sharecrisisInput) %>% 
      mutate(year = as.numeric(year))
    
    # Depending on the type of index chosen, filter by crisis type:
    
    if(input$sharecrisisInput == "Currency_crisis_severe"){
      share_crisis <- share_crisis %>% 
        filter(type_crisis %in% "Currency Crisis") 
    }
    if(input$sharecrisisInput == "Banking_crisis_severe"){
      share_crisis <- share_crisis %>% 
        filter(type_crisis %in% "Banking Crisis")  
    }  
    if(input$sharecrisisInput == "Sovereign_default"){
      share_crisis <- share_crisis %>% 
        filter(type_crisis %in% "Sovereign Debt Crisis")
    }
    
    # Filter by database of comparison chosen and calculate events count by year:
    
    share_crisis <- share_crisis %>% 
      filter(database %in% input$sharecrisisInput2) %>%
      filter(year<=2010) %>%
      group_by(year) %>% 
      summarise(n.events_own = sum(occurence, na.rm = TRUE), n.events_others = sum(dummy_crisis, na.rm = TRUE))
    
    # Plot static graph:
    
    no_interactive <- share_crisis %>% 
      ggplot(aes(x = n.events_own, y = n.events_others, group = 1,label=year)) +
      geom_point(aes(col = "comparison")) +
      geom_smooth(method = "lm",se = FALSE, linetype = "dotted",color='lightgrey',size=1) +
      annotate(x=2,y=max(share_crisis$n.events_others),geom="text",
               label=paste0("y=",round(cor(share_crisis$n.events_own,share_crisis$n.events_others),2),"x"),
               color="lightgrey")+
      theme_minimal() +
      labs(x="Betin-Collodel",
           y=input$sharecrisisInput2,
           caption="Number of countries experiencing a crisis each year")+
      theme(legend.position = "none") +
      theme(axis.text.x = element_text(angle = 270, hjust = 1))
    
    # Render interactive:
    
    
    ggplotly(no_interactive)
    

  })
  
  # Second output component, correlation tab. -----
  
  
  output$share_crisis_correlation <- renderTable({
    
    # Filter by type index:
    
    share_crisis <- comparison_dataframe %>%
      filter(type_index %in% input$sharecrisisInput) %>% 
      filter(year<=2010) %>% # only include periods prior to 2010 because RR start in 2010
      mutate(year = as.numeric(year))
    
    # Filter by equivalent type crisis:
    
    if(input$sharecrisisInput == "Currency_crisis_severe"){
      share_crisis <- share_crisis %>% 
        filter(type_crisis %in% "Currency Crisis") 
    }
    if(input$sharecrisisInput == "Banking_crisis_severe"){
      share_crisis <- share_crisis %>% 
        filter(type_crisis %in% "Banking Crisis")  
    }  
    if(input$sharecrisisInput == "Sovereign_default"){
      share_crisis <- share_crisis %>% 
        filter(type_crisis %in% "Sovereign Debt Crisis")
    }
    
    # Filter by database of choice and calculate events count per year:
    
    share_crisis <- share_crisis %>% 
      filter(database %in% input$sharecrisisInput2) %>% 
      group_by(year) %>% 
      summarise(n.events_own = sum(occurence, na.rm = TRUE), n.events_others = sum(dummy_crisis, na.rm = TRUE))
    
    # Correlation:
    
    correlation <- data.frame(
      cor(share_crisis$n.events_own, share_crisis$n.events_others)
    )
    names(correlation) <- "Correlation share of countries:" # column name will display.
    correlation
    
  }, striped = TRUE)
}



