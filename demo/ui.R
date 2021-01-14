ui <- fluidPage(
  titlePanel("Covid Vax"),
  
  fluidRow(
    column(4,
           selectInput(
             "cn",
             "Choose a country:",
             selected = "Belgium",
             list(`countries` = as.list(members$country_name))
           ),
           numericInput("rn", 
                        "Basic Reproduction Number", 
                        value = 2.7, 
                        min = 1, max = 3, step = 0.1),
           numericInput("waning_nat", 
                        "Waning of Natural Immunity (Weeks)", 
                        value = 45, 
                        min = 20, max = 104, step = 1),
           numericInput("waning_vac", 
                        "Waning of Vaccine Induced Immunity (Weeks)", 
                        value = 52, 
                        min = 20, max = 104, step = 1),
           numericInput("cov_tar", 
                        "Vaccination Uptake Cap", 
                        value = 0.8, 
                        min = 0, max = 1, step = 0.1),
           selectInput("priority", 
                       "Priority Setting",
                       c("Strategy 1",
                         "Strategy 2",
                         "Strategy 3",
                         "Strategy 4")),
           HTML(paste0(p("Strategy 1: All Adults"),
                       "</p>",
                       p("Strategy 2: All 60+, then all younger adults"),
                       "</p>",
                       p("Strategy 3: All younger adults, then all elderly"),
                       "</p>",
                       p("Strategy 4: From the oldest to the youngest adults")))
    ),
    column(4,
           h4("Vaccination Progress Milestones"),
           dateInput("date1", "Start date", value = "2021-01-01"),
           dateInput("date2", "Second", value = "2021-06-30"),
           dateInput("date3", "Third", value = "2021-12-31"),
           dateInput("date4", "Fourth", value = NA),
           dateInput("date5", "Last", value = "2022-12-31")
    ),
    column(4,
           h4("Vaccination Coverage Milestones"),
           numericInput("cov1", HTML("Starting from"), value = 0, 
                        min = 0, max = 0, step = 0.00),
           numericInput("cov2", "Second", value = 0.03, 
                        min = 0, max = 1, step = 0.01),
           numericInput("cov3", "Third", value = 0.20, 
                        min = 0, max = 1, step = 0.01),
           numericInput("cov4", "Fourth", value = NA, 
                        min = 0, max = 1, step = 0.01),
           numericInput("cov5", "Last", value = 0.6, 
                        min = 0, max = 1, step = 0.01),
           br(),
           p("*Vaccination coverage will always start from 0."))
  ),
  
  hr(),
  
  actionButton("update", "Run with Current Parameters"),
  
  tabsetPanel(
    type = "tabs",
    
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
      DT::dataTableOutput('econ')
    )
  )
)
