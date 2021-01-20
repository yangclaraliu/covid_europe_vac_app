ui <- fluidPage(
  # set up shiny js
  shinyjs::useShinyjs(),
  shinyjs::inlineCSS(list(.big = "font-size: 2em")),
  
  # title
  titlePanel("Covid Vax"),
  
  fluidRow(
    column(3,
           #### Input = cn ####
           selectInput(
             "cn",
             "Choose a country:",
             selected = "Belgium",
             list(`countries` = as.list(members$country_name))
           ),
           a(id = "toggleEpi",
             "Show/Hide Epidemic Parameters",
             href = "#"),
           shinyjs::hidden(
             div(
               id = "Epi",
               #### Input = rn #### 
               numericInput("rn", 
                            "Basic Reproduction Number", 
                            value = 2.7, 
                            min = 1, max = 3, step = 0.1),
               #### Input = waning_nat #### 
               numericInput("waning_nat", 
                            "Waning of Natural Immunity (Weeks)", 
                            value = 45, 
                            min = 20, max = 104, step = 1),
               #### Input = date_start ####
               dateInput("date_start",
                         "Simulation Start Date",
                         value = "2020-02-05"), #
               #### Input = date_end ####
               dateInput("date_end",
                         "Simulation End Date",
                         value = "2022-12-31",
                         min = "2022-12-31",
                         max = "2022-12-31")
             )
           ),
           h4("Vaccine Characteristics"),
           #### Input = ve  #### 
           sliderInput("ve",
                       "Vaccine Efficacy",
                       min = 0,
                       max = 1,
                       value = 0.9),
           #### Input = waning_vac #### 
           sliderInput("waning_vac", 
                        "Waning of Vaccine Induced Immunity (Year)", 
                        value = 1, 
                        min = 0.5, max = 2, step = 0.5),
           #### Input = max_cov #### 
           sliderInput("max_cov", 
                        "Maximum Vaccination Uptake", 
                        value = 0.8,
                        min = 0, max = 1, step = 0.1)
    ),
    column(8,
           h3("Vaccine Strategy Set Up"),
           h4("Priority Setting"),
           HTML(paste0(
             p("Strategy 0: No vaccine deployed"),
             "</p>",
             p("Strategy 1: All Adults"),
             "</p>",
             p("Strategy 2: All 60+, then all younger adults"),
             "</p>",
             p("Strategy 3: All younger adults, then all elderly"),
             "</p>",
             p("Strategy 4: From the oldest to the youngest adults"))),
           #### Input = n_ms #### 
           selectInput(
             inputId = "type_ms",
             label = "Milestones Setup",
             choices = c("Preload",
                         "Customised"),
             selected = "Preload"
           ),
           uiOutput("panel_ms")
    )
  ),
  
  hr(),
  actionButton("update", "Run with Current Parameters"),
  hr(),
  tabsetPanel(
    # tabPanel(
    #   title = "Test",
    #   verbatimTextOutput('test')
    # ),
    tabPanel(
      title = "Overall Vaccine Supply",
      plotOutput('supply', height = 800)
    ),
    tabPanel(
      title = "Vaccination Progress (by age)",
      plotOutput('daily_vac', height = 800)
    ),
    tabPanel(
      title = "Public Health Outcomes",
      plotOutput('pho', height = 800)
    ),
    tabPanel(
      title = "Health Economics Outcomes",
      plotOutput('econ')
    )
  )
)
