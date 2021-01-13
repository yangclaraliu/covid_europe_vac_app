server <- function(input, output) {
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