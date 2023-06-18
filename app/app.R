# Load packages ----
library(shiny)
library(maps)
library(mapproj)


# Source helper functions -----
source("power_plotter.R")


  
# User interface ----
ui <- fluidPage(
  titlePanel("censusVis"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Create demographic maps with 
        information from the 2010 US Census."),
      
      
      
      selectInput("sim_n", 
                  label = "Number of simulations",
                  choices = c(1, 10,100, 1000),
                  selected = 100),
      
      selectInput("es_steps", 
                  label = "Steps between min and max effect size",
                  choices = c(10, 20, 30, 40),
                  selected = 10),
      
      sliderInput("es_range", 
                  label = "Range of effect sizes",
                  min = 0, max = 0.8, value = c(0, 0.8)),

      sliderInput("n_ppt", 
                  label = "Number of participants",
                  min = 0, max = 100, value = 50),
      
      actionButton("go", "Update")
      
    ),
    
    mainPanel(plotOutput("map"))
  )
)


# Server logic ----
server <- function(input, output) {
  
  output$map <- renderPlot({
    input$go
    isolate(power_plot(as.numeric(input$n_ppt), as.numeric(input$es_range[1]), as.numeric(input$es_range[2]), as.numeric(input$es_steps), as.numeric(input$sim_n)))
  })
  
  
}

# Run app ----
shinyApp(ui, server)