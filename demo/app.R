## app.R
# loading basic parameters
# proj_path <- "C:/Users/eideyliu/Documents/GitHub/covid_europe_vaccine"
# source(paste0(proj_path,"/code/0_LoadData.R"))
# source(paste0(proj_path,"/code/1_GenRun.R"))
# source(paste0(proj_path,"/code/f_gen_econ.R"))
# source(paste0(proj_path,"/code/f_simulate_within_app.R"))
# source(paste0(proj_path,"/code/f_gen_econ_within_app.R"))

source("0_LoadData.R")
source("1_GenRun.R")
source("f_simulate_within_app.R")
source("f_gen_econ_within_app.R")
library("shiny")

# user interface
ui <- fluidPage(
  titlePanel("Covid Vax"),
  
  actionButton("update", "Run with Current Parameters"),
  
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

server <- function(input, output, session) {
  
  dataInput <- eventReactive(input$update, {
    predict_deriv(
      # country name
      cn = input$cn,
      # vaccination saturation cap
      cov_tar = input$cov_tar,
      # vaccination milestones (date)
      ms_date = c(input$date1, input$date2, 
                  input$date3, input$date4, 
                  input$date5),
      ms_cov = c(input$cov1, input$cov2,
                 input$cov3, input$cov4,
                 input$cov5),
      # priority strategy selected
      priority = input$priority,
      # c(natural immunity duration, vaccine induced immunity duration)
      wane = c(input$waning_nat, input$waning_vac),
      # basic reproduction number 
      R = input$rn
                  )
  })
  
  #### supply pplot ####
  output$supply <- renderPlot({
    dataInput()[["supply"]]%>% 
      mutate(tot = if_else(is.na(doses), 0, doses),
             tot = cumsum(tot)) %>% 
      full_join(data.frame(date = seq(as.Date("2020-02-05"),
                                      as.Date("2022-12-31"),
                                      1)),
                by = c("milestone_date" = "date")) %>% 
      arrange(milestone_date) %>% 
      mutate(doses_daily = zoo::na.locf(doses_daily, na.rm = F),
             doses_daily = if_else(is.na(doses_daily), 0, doses_daily)) %>%
      # filter(!is.na(tot)) %>% 
      pivot_longer(cols = c(tot, doses_daily)) %>% 
      filter(!(name == "tot" & is.na(value))) %>%
      mutate(name = factor(name,
                           levels = c("tot", "doses_daily"),
                           labels = c("Total Vaccine Doses Available",
                                      "Daily Vaccine Doses Available"))) %>% 
      ggplot(., aes(x = milestone_date,
                    y = value)) +
      geom_line() +
      geom_point(aes(alpha = name)) +
      scale_alpha_manual(values = c(1,0)) +
      theme_bw() +
      theme(legend.position = "none",
            title = element_text(size = 20),
            strip.text = element_text(size = 16),
            axis.title = element_text(size = 16)) +
      facet_wrap(~name,
                 scale = "free") +
      labs(title = "Vaccine Supply (Daily)",
           subtitle = "*based on input assumptions.",
           y = "# of Vaccines",
           x = "Date") 
  })
  
  #### renderplot for daily_vac ####
 output$daily_vac <- renderPlot({
   dataInput()[["daily_vac"]] %>% 
     mutate(date =   dataInput()[["supply"]]$milestone_date[1] + as.numeric(t)) %>% 
     dplyr::select(-t, -supply) %>%
     mutate_at(vars(starts_with("Y")), cumsum) %>% 
     pivot_longer(cols = starts_with("Y")) %>% 
     # filter(value > 0) %>% 
     left_join(data.frame(name = paste0("Y",1:16),
                          pop = dataInput()[["size"]]),# params$param$pop[[1]]$size),
               by = "name") %>% 
     mutate(p = value/pop,
            name = factor(name,
                          levels = paste0("Y",1:16),
                          labels = c(paste0(seq(0,74,5),
                                            "-",
                                            seq(4,74,5)),
                                     "75+"))) %>%
     ggplot(., aes(x = date,
                   y = p,
                   group = name)) +
     # geom_point() +
     geom_line() +
     facet_wrap(~name) +
     theme_bw() +
     theme(legend.position = "none",
           title = element_text(size = 20),
           strip.text = element_text(size = 16),
           axis.title = element_text(size = 16),
           axis.text.x = element_text(angle = 90)) + 
     labs(x = "Date",
          y = "Coverage",
          title = "Age Specific Vaccination Progress")
 })
 
 
 #### renderPlot for Public Health Outcomes ####
 output$pho <- renderPlot({
   dataInput()[["dynamics"]] %>% 
     filter(compartment %in% c("death_o", "cases")) %>%
     dplyr::select(t, compartment, value, group) %>% 
     group_by(t, compartment) %>% 
     summarise(value = sum(value),
               .groups = "drop") %>% 
     pivot_wider(names_from = compartment,
                 values_from = value) %>% 
     mutate(cases_cum = cumsum(cases),
            death_cum = cumsum(death_o)) %>% 
     dplyr::select(-cases, -death_o) %>% 
     full_join(dataInput()[["dynamics_baseline"]] %>% 
                 filter(compartment %in% c("death_o", "cases")) %>%
                 dplyr::select(t, compartment, value, group) %>% 
                 group_by(t, compartment) %>% 
                 summarise(value = sum(value),
                           .groups = "drop") %>% 
                 pivot_wider(names_from = compartment,
                             values_from = value) %>% 
                 mutate(cases_cum_baseline = cumsum(cases),
                        death_cum_baseline = cumsum(death_o)) %>% 
                 dplyr::select(-cases, -death_o),
               by = "t") %>% 
     pivot_longer(cols = c(cases_cum, cases_cum_baseline,
                           death_cum, death_cum_baseline)) %>% 
     mutate(date = t + dataInput()[["supply"]]$milestone_date[1]) %>%
     separate(name, into = c("endpoint","metric", "type")) %>% 
     mutate(type = if_else(is.na(type), "predicted", type),
            type = factor(type,
                          levels = c("baseline",
                                     "predicted"),
                          labels = c("No vaccination",
                                     "Current vaccnation strategy")),
            endpoint = factor(endpoint,
                              levels = c("cases", "death"),
                              labels = c("Cumulative Cases",
                                         "Cumulative Deaths"))) %>% 
     ggplot(., aes(x = date,
                   y = value,
                   group = type,
                   color = type)) +
     geom_line(size = 1.5) +
     facet_wrap(~endpoint, scale = "free")+
     theme_bw() +
     theme(legend.position = "bottom",
           title = element_text(size = 20),
           strip.text = element_text(size = 16),
           legend.text = element_text(size = 16),
           axis.title = element_text(size = 16)) +
     ggsci::scale_color_lancet() +
     labs(x = "Date",
          y = "",
          color = "",
          title = "")
 })
 
 output$econ <- DT::renderDataTable({
   dataInput()[["econ"]]  %>% 
     separate(name, into = c("policy", "var","unit")) %>% 
     mutate(unit = if_else(is.na(unit), 
                           "Overall Loss Reduction", 
                           "Per-dose Loss Reduction")) %>% 
     pivot_wider(names_from = unit,
                 values_from = value) %>% 
     dplyr::select(-wb, -policy) %>% 
     mutate(var = c("Life Expectancy",
                    "Comorbidity-adjusted Life Expectancy",
                    "Quality-adjusted Life Expectancy",
                    "VSL (mln. USD)",
                    "Morbidity-related QALY",
                    "Total QALY (AEFI + morbidity + mortality)")) %>% 
     rename(Metrics = var) %>% 
     mutate(`Overall Loss Reduction` = round(`Overall Loss Reduction`),
            `Per-dose Loss Reduction` = round(`Per-dose Loss Reduction`,4))
   
 })
}

shinyApp(ui, server)