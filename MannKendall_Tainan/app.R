#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

pacman::p_load(shiny, sf, tidyverse, sfdep, ggplot2)

gi_stars <- readRDS("data/gi_stars.rds")

# Define UI for application that draws a histogram

ui <- fluidPage(
  titlePanel("Mann Kendall Test for Dengue Fever Villages in Tainan City, Taiwan"),
  sidebarLayout(
    sidebarPanel(
      actionButton("uncheckbutton", "Clear Selection"),
      checkboxGroupInput(
        inputId = "locations",
        label = "Locations (Village, Town ID):",
        choices = NULL,
        selected = "Qingcao Vil. , D06"
      )
      
    ),
    mainPanel(
      plotOutput("villagePlot"),
      p(em("dashed lines as lines of reference, where Gi* = 1.96 and Gi* = -1.96")),
      p(strong("Interpretation of Gi* values:")),
      p("Positive Gi*: Potential hotspot (clusters of high number of dengue cases)"),
      p("Negative Gi*: Potential coldspot (clusters of low number of dengue cases)"),
      p(strong("The hotspot (coldspot) is significant if Gi* > 1.96 ( < 1.96) at 95% confidence level."))
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  observe({
    # Update choices dynamically based on the 'location' column of your data
    updateCheckboxGroupInput(session, "locations", choices = (unique(gi_stars$location)))
  })
  
  observeEvent(input$uncheckbutton, {
    # Uncheck the selected locations
    updateCheckboxGroupInput(session, "locations", selected = character(0))
  })
  
  output$villagePlot <- renderPlot({
    
    # Check if at least one location is selected
    if (length(input$locations) > 0) {
      
      # Filter data based on selected locations
      filtered_data <- gi_stars %>% 
        ungroup() %>%
        filter(location %in% input$locations) %>%
        select(location, `epiweek(發病日)`, gi_star)
      
      # Plot data for selected locations
      ggplot(data = filtered_data,
             aes(x = `epiweek(發病日)`, y = gi_star, color = location)) + 
        geom_line() +
        labs(title = "Gi* of Villages over Epidemiology Weeks",
             x = "Epiweek",
             y = "Gi* Value",
             color = "Location") +
        geom_line(y=0, color = "black") + 
        geom_line(y=1.96, color = "#383938", linetype = "dashed") +
        geom_line(y=-1.96, color = "#383938", linetype = "dashed")
      
    } else {
      # Handle the case when no location is selected
      ggplot() + 
        theme_minimal() +
        ggtitle("No locations selected")
    }
  })
}



# Run the application 
shinyApp(ui = ui, server = server)
