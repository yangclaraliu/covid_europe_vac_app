ui <- fluidPage(
  # set up shiny js
  shinyjs::useShinyjs(),
  shinyjs::inlineCSS(list(.big = "font-size: 2em")),
  
  # title
  titlePanel("Country Level COVID-19 Vaccination Prioritisation Decision Support Tool for Europe and Central Asia"),
  p("Version -1; Updated on 2021/02/05."),
  
  strong(HTML("Authors:")),
  tags$a(href = "https://www.lshtm.ac.uk/aboutus/people/liu.yang", "Yang Liu,"),
  HTML("Frank Sandmann, Rosanna Barnard, Carl AB Pearson, Nicholas G Davies, Mark Jit </p>"),
  
  fluidRow(
    column(3,
           HTML(paste0(
             strong("Aim"),
             h5("To estimate the outbreak trajectories and health economic 
                endpoints under different vaccination strategies in the WHO 
                - Europe & Central Asia Region."),
             "</p>",
             strong("Key Assumptions"),
             h5("- The natural progression of COVID-19 in this application is
                implemented using COVID-M, which accounts for pre-symptomatic 
                and asymptomatic individuals, seasonality, waning of 
                infection-induced natural immunity, age-specific suspecibility 
                and clinical progression rates, age-specific infection 
                progression rates, and severity rates."),
             "</p>",
             h5("- Basic epidemic parameters are based on our best knowledge 
                although could be changed if needed."),
             h5("- Current epidemic is fitted for the smoothed trends observed 
                in COVID-19 deaths by country."),
             h5("- Changes in contact patterns are informed by Google mobility
                and Government Stringency Index (OxCGRT). We assume that in a 
                year's time, human behaviour will return to 90% the level prior
                to the pandemic."),
             h5("- We assume the vaccine in use is 'leaky'. Vaccines could 
                protect again infection, disease, or both - independent of 
                clinical status."),
             h5("- Our current decision frame ends on 2022-12-31."),
             "</p>",
             strong("Limitations"),
             h5("- Current vaccination under consideration assumes one-dose 
                vaccination schedule. In otherwords, expected protection is 
                achieved using only one dose. We hope to update the parameter 
                sets as more information on immunogeneicity emerges."),
             h5("- Current epidemic is fitted for the smoothed trends observed
                in COVID-19 deaths by country. With additional information such
                as changes in underascertainment rates or different public 
                health endpoints such as daily number of hospitalisations, 
                better estimatation regarding the current epidemics could be 
                achieved."),
             h5("- Uncertainty is not currently presented in this application."),
             "</p>",
             strong("The application may take a few seconds to compile. "),
             tags$a(href="https://docs.google.com/forms/d/e/1FAIpQLSfd8BL-9DHMWBRSGY6MHezWwlIHCqNt5vJxSstD8jxtp48S0A/viewform", "We welcome any feedbacks regarding this application!")
             ))),
    column(3,
           leafletOutput("loc_map", width = "100%", height = "300px"),
           #### Input = cn ####
           pickerInput("cn", 
                       "cn", 
                       multiple = F,
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
                            value = 2, 
                            min = 1, max = 5, step = 0.1),
               #### Input = waning_nat #### 
               # numericInput("waning_nat", 
               #              "Waning of Natural Immunity (Weeks)", 
               #              value = 45, 
               #              min = 20, max = 104, step = 1),
               #### Input = date_start ####
               dateInput("date_start",
                         "Infection Introduction Date",
                         value = as.character(lubridate::ymd("2019-12-01") + 54))#,#
               #### Input = date_end ####
               # dateInput("date_end",
               #           "Simulation End Date",
               #           value = "2022-12-31",
               #           min = "2022-12-31",
               #           max = "2022-12-31")
             )
           ),
           
           HTML("</p>"),
           
           a(id = "toggleVac",
             "Show/Hide Vaccine & Vaccination Parameters",
             href = "#"),
           
           shinyjs::hidden(
             div(
               id = "Vac",
               #### some description about vaccine setups ####
               p("VE against symptomatic infection is the most direct and 
                  commonly measured endpoint in a vaccine trial. VE against 
                 all infection and asymptomatic infections are not easily 
                 observable and thus often rely on assumptions. The default 
                 setting in this application corresponds to a hypothetical 
                 and highly effective vaccine. For a conservative estimate, 
                 please try with VE against symptomatic infection at 0.5 and 
                 VE against all infection at 0. Note VE against asymptomatic 
                 infection is based on initial asymtpomatic rate of 50%."),
               
               #### Input = ve  #### 
               sliderInput("Ts", 
                           "VE against disease:",
                           min = 0,
                           max = 0.99,
                           step = 0.01,
                           value = 0.95),
               sliderInput("ve_i",
                           "VE against infection",
                           min = 0,
                           max = 0.99,
                           value = 0.95),
               h5(tags$b("VE against asymptomatic infection is thus:")),
               verbatimTextOutput("message1"),
               # sliderInput("ve_d",
               #             "Vaccine Efficacy against Disease",
               #             min = 0,
               #             max = 1,
               #             value = 0.5),
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
             ))
    ),
    column(6,
           h3("Vaccine Strategy Set Up"),
           h4("Priority Setting"),
           HTML(paste0(
             p("Strategy V-: No vaccine deployed"),
             "</p>",
             p("Strategy V+: The entire adult population will start vaccination
               simultaneously."),
             "</p>",
             p("Strategy V60: Those 60+ years old will start vaccinate first, 
               then all other adults (i.e., 20-59 yo)."),
             "</p>",
             p("Strategy V20: Those between 20 and 59 years old will start 
               vaccinate first, then all other adults (i.e., 60+)."),
             "</p>",
             p("Strategy V75: Starting from the oldest age group, moving towards 
               younger elderlies. When vaccination among elderlies are completed, 
               start vaccinating those between 20-59 simultaneously."))),
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
  downloadButton("dl_epi",
                 "Download Current Epicurves"),
  downloadButton("dl_he",
                 "Download Current Health Economics Outcomes"),
  hr(),
  tabsetPanel(
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
    ),
    tabPanel(
      title = "Model Fit & Effective R",
      imageOutput("epi_fit")
    )
  )
)
