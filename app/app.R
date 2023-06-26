# Load packages ----
library(shiny)
library(maps)
library(mapproj)


# Source helper functions -----
source("power_plotter.R")


  
# User interface ----
ui <- fluidPage(
  titlePanel("powerPlotter"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Plot power simulations for indipendent sample t-test. 
               Change the settings below and press update to see the plot. 
               The more simulations your are running the longer it will take to
               display the plots."),
      
      
      
      selectInput("sim_n", 
                  label = "Number of simulations",
                  choices = c(1, 10,100, 1000),
                  selected = 100),
      sliderInput("n_ppt", 
                  label = "Number of data points in each sample group",
                  min = 0, max = 100, value = c(10,100)),
      
      selectInput("n_steps", 
                  label = "Steps between min and max sample size",
                  choices = c(1, 2, 5, 10, 20),
                  selected = 1),
      
      sliderInput("es_range", 
                  label = "Range of effect sizes",
                  min = 0, max = 1, value = c(0, 1)),
      
      selectInput("es_steps", 
                  label = "Steps between min and max effect size",
                  choices = c(10, 20, 30, 40),
                  selected = 10),

      actionButton("go", "Update")
      
    ),
    
    mainPanel(plotOutput("map"))
  )
)


# Server logic ----
server <- function(input, output) {
  
  output$map <- renderPlot({
    input$go
    isolate(power_plot(as.numeric(input$n_ppt[1]), as.numeric(input$n_ppt[2]), as.numeric(input$n_steps), as.numeric(input$es_range[1]), as.numeric(input$es_range[2]), as.numeric(input$es_steps), as.numeric(input$sim_n)))
  })
  
  
}

# Run app ----
shinyApp(ui, server)