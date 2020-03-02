# Skeleton structure of the app.

# UI part (inputs) -----

ui <- fluidPage(
  # Set theme and table disposition/title.
  theme = shinytheme("paper"),
  navbarPage("A new crisis index: text-mining IMF documents"),
  # Individual tabs.
  tabsetPanel(
    # First tab: explanation of methodology and introduction index.
    tabPanel("Methodology"),
    # Second tab: comparison of dummy indicators for banking, currency and sovereign debt crises with Reinhart & Rogoff and Laeven & Valencia.
    tabPanel("First dimension: is there a crisis?", 
             tabTwo_UI("tab_two")),
    # Third tab: comparison with standard crisis databases by country (equivalent of Figure 2, Romer & Romer). Check differences and intensity.
    tabPanel("First dimension: important differences",
             tabThree_UI("tab_three")),
    # Fourth tab: comparison across countries for single index (Figure 1, Romer & Romer) and introduction to complexity of a crisis (across indexes). 
    tabPanel("Second dimension: intensity of the crisis",
              tabFour_UI("tab_four"))
  )
)


# Server part (outputs) -----

server <- function(input, output) {
  
  # Second tab: ----
  callModule(tabTwo, "tab_two")
  # Third tab: -----
  callModule(tabThree, "tab_three")
  # Fourth tab:
  callModule(tabFour, "tab_four")
  
}


shinyApp(ui = ui, server = server)

