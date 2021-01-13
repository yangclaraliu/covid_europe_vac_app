library(shiny)

# Define UI for dataset viewer app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Shiny Text"),
  
  # Sidebar layout with a input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Selector for choosing dataset ----
      selectInput(inputId = "dataset",
                  label = "Choose a dataset:",
                  choices = c("rock", "pressure", "cars")),
      
      # Input: Numeric entry for number of obs to view ----
      numericInput(inputId = "obs",
                   label = "Number of observations to view:",
                   value = 10)
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Verbatim text for data summary ----
      verbatimTextOutput("summary"),
      
      # Output: HTML table with requested number of observations ----
      # tableOutput("view"),
      tableOutput("dyna")
      
    )
  )
)

server <- function(input, output) {
  require(covidm)
  require(tidyverse)
  cm_matrices     = readRDS("all_matrices.rds")
  cm_populations  = readRDS("wpp2019_pop2020.rds")
  
  # Return the requested dataset ----
  datasetInput <- reactive({
    switch(input$dataset,
           "rock" = rock,
           "pressure" = pressure,
           "cars" = cars)
  })
  
  dyna <- reactive({
    params <- cm_parameters_SEI3R("Belgium")
    res <- cm_simulate(params)
    r <- res$dynamics
    return(r)
  })
  
  # Generate a summary of the dataset ----
  output$summary <- renderPrint({
    dataset <- datasetInput()
    summary(dataset)
  })
  
  # Show the first "n" observations ----
  # output$view <- renderTable({
  #   head(dyna(), n = input$obs)
  # })
  
  # Show the first "n" observations ----
  output$dyna <- renderTable({
    head(dyna(), n = input$obs)
  })
  
}

shinyApp(ui, server)