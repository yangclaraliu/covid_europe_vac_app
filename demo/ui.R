ui <- fluidPage(
  # set up shiny js
  shinyjs::useShinyjs(),
  shinyjs::inlineCSS(list(.big = "font-size: 2em")),
  
  # title
  titlePanel("Covid Vax"),
  
  strong("Authors"),
  p("Yang Liu, Frank Sandmann, Rosanna Barnard, Nicholas G Davies, Mark Jit"),
  
  fluidRow(
    column(3,
           HTML(paste0(
             strong("Aim"),
             h5("To estimate the outbreak trajectories and health economic endpoints under different vaccination strategies in the WHO - Europe & Central Asia Region."),
             "</p>",
             strong("Key Assumptions"),
             h5("- The natural progression of COVID-19 in this application is implemented using COVID-M, which accounts for pre-symptomatic and asymptomatic individuals, age-specific suspecibility and clinical progression rates, age-specific infection progression rates, and severity rates."),
             "</p>",
             h5("- Basic epidemic parameters are based on our best knowledge although could be changed if needed."),
             h5("- Current epidemic is fitted for the smoothed trends observed in COVID-19 deaths by country."),
             h5("- Changes in contact patterns are informed by Google mobility and Government Stringency Index (OxCGRT). We assume that in a year's time, human behaviour will return to 90% the level prior to the pandemic."),
             h5("- We assume the vaccine in use is 'leaky'. Vaccines could protect again infection, disease, or both - independent of clinical status."),
             h5("- Our current decision frame ends on 2022-12-31."),
             "</p>",
             strong("Limitations"),
             h5("- Current vaccination under consideration assumes one-dose vaccination schedule. In otherwords, expected protection is achieved using only one dose. We hope to update the parameter sets as more information on immunogeneicity emerges."),
             h5("- Current epidemic is fitted for the smoothed trends observed in COVID-19 deaths by country. With additional information such as changes in underascertainment rates or different public health endpoints such as daily number of hospitalisations, better estimatation regarding the current epidemics could be achieved.")
             
             ))),
    column(3,
           #### Input = cn ####
           pickerInput("cn", "cn", multiple = F,
                       label = "Choose a country:",
                       selected = "Albania",
                       choices = as.list(members$country_name),
                       
                       choicesOpt = list(content =  
                                           mapply(c(members$country_name), 
                                                  flags, 
                                                  FUN = function(country, flagUrl) {
                                             HTML(paste(
                                               tags$img(src=flagUrl, 
                                                        width=20, 
                                                        height=15),
                                               country
                                             ))
                                           }, SIMPLIFY = FALSE, USE.NAMES = FALSE)
                                         
                       )),
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
           sliderInput("ve_i",
                       "Vaccine Efficacy against Infection",
                       min = 0,
                       max = 1,
                       value = 0.9),
           sliderInput("ve_d",
                       "Vaccine Efficacy against Disease",
                       min = 0,
                       max = 1,
                       value = 0),
           #### Input = waning_vac #### 
           sliderInput("waning_vac", 
                        "Waning of Vaccine Induced Immunity (Year)", 
                        value = 1, 
                        min = 0.5, max = 10, step = 0.5),
           #### Input = max_cov #### 
           sliderInput("max_cov", 
                        "Maximum Vaccination Uptake", 
                        value = 0.8,
                        min = 0, max = 1, step = 0.1)
    ),
    column(6,
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
      plotOutput('econ', height = 1000)
    )
  )
)
