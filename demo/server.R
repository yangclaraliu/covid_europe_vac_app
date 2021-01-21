server <- function(input, output, session) {
  output$ms_dates <- renderUI({
    lapply(1:as.numeric(input$n_ms), function(x) {
      dateInput(paste0("date",x), 
                paste0("Milestone Date (", x, ")"), 
                format = "yyyy-mm-dd",
                value = if_else(x == 1, 
                                as.character(as.Date(Sys.time())),
                                "2022-12-31"
                                ), # as.character(as.Date(Sys.time())),
                min = "2020-01-01",
                max = "2022-12-31"
      )}
    )
  })
  
  output$ms_covs <- renderUI({
    lapply(1:as.numeric(input$n_ms), function(x) {
      numericInput(paste0("cov",x), 
                   paste0("Milestone Coverage (", x, ")"), 
                   value = case_when(
                     x == 1 ~ 0,
                     x == input$n_ms ~ 0.5,
                     TRUE ~ as.numeric(NA)
                   ),
                   min = 0, 
                   max = 1, 
                   step = 0.01)
    })
  })
  
  output$panel_ms <- renderUI({
    if (is.null(input$type_ms))
      return()
    
    switch(input$type_ms,
           "Preload" = list(
             selectInput(
               inputId = "preload_ms",
               label = "Choose Vaccination Rollout Plan from the Dropdown Menu:",
               choices = c("Linear Increase",
                           "Exponential Increase",
                           "Sigmoid Increase"),
               selected = "Linear Increase"
             ),
             helpText("Overall vaccine coverage is assumed to reach 50% on 2021-12-31. Additional revaccination programs are included to keep population level stablised at 50% level while accounting for waning immunity.")
           ),
           "Customised" = list(numericInput(
             inputId = "n_ms",
             label = "Number of Milestones",
             min = 2,
             max = 10,
             step = 1,
             value = 2
           ),
           # actionButton("refresh", "Update Milestones"),
           fluidRow(
             column(3,
                    h4("Vaccination Progress Milestones"),
                    uiOutput("ms_dates")
                    ),
             column(3,
                    h4("Vaccination Coverage Milestones"),
                    uiOutput("ms_covs"),
                    br(),
                    p("*Vaccination coverage will always start from 0.")
             )
           )
           ))
  })
  
  observe({
    cn_label <- countrycode::countrycode(input$cn, "country.name", "wb")
    updateDateInput(session,
                    inputId = "date_start",
                    value = model_selected[model_selected$WB == cn_label,
                                           "start_date"]  %>% 
                      unlist %>% 
                      min %>% 
                      as.numeric %>% 
                      as.Date(., origin = "1970-01-01") %>% 
                      as.character) 
  })
  
  observe({
    shinyjs::onclick("toggleEpi",
                     shinyjs::toggle(id = "Epi", anim = T))
  })
  
  
  dataInput <- eventReactive(input$update, {
    predict_deriv(
      # country name
      cn = input$cn,
      # vaccination saturation cap
      cov_tar = input$max_cov,
      type_ms = input$type_ms,
      pattern_label = input$preload_ms,
      # vaccination milestones (date)
      ms_date = sapply(
        1:input$n_ms, 
        function(x){
        req(input[[paste0("date", x)]]);
        input[[paste0("date", x)]]
        }),
                # as.vector(input[[paste0("date", 1:input$n_ms)]]),
                # input[startsWith("date", names(input))] %>% unlist,
                # sort_input(input, "date"),
                # c(input$date1, input$date2), 
                # input$date3, input$date4, 
                # input$date5),
      ms_cov =  sapply(
        1:input$n_ms, 
        function(x){
        req(input[[paste0("cov", x)]]);
        input[[paste0("cov", x)]]
        }),
                #as.vector(input[[paste0("cov", 1:input$n_ms)]]),
                # input[startsWith("cov", names(input))] %>% unlist,
                # sort_input(input, "cov"),
                # c(input$cov1, input$cov2),
                # input$cov3, input$cov4,
                # input$cov5),
      # priority strategy selected
      date_start = input$date_start,
      ve_i = input$ve_i,
      ve_d = input$ve_d,
      # c(natural immunity duration, vaccine induced immunity duration)
      wane = c(input$waning_nat, input$waning_vac),
      # basic reproduction number 
      R = input$rn
    )
  })
  
  # 
  # output$test <- renderPrint({
  #   input$date_start
  # })
  
  #### supply plot ####
  output$supply <- renderPlot({
   dataInput()[["main"]]%>% 
   # main %>%
      dplyr::filter(policy == 1) %>% 
      dplyr::select(date, doses_daily, supply) %>%
      pivot_longer(cols = c(doses_daily, supply)) %>% 
      mutate(name = factor(name,
                           levels = c("doses_daily", "supply"),
                           labels = c(
                             "Daily Vaccine Doses Available",
                             "Total Vaccine Doses Available"))) %>% 
      ggplot(., aes(x = date,
                    y = value)) +
      # geom_point() +
      geom_line(size = 1.5) +
      scale_alpha_manual(values = c(1,0)) +
      theme_bw() +
      theme(legend.position = "bottom",
            title = element_text(size = 24),
            strip.text = element_text(size = 20),
            axis.text = element_text(size = 20),
            axis.title = element_text(size = 20)) +
      facet_wrap(~name,
                 scale = "free") +
      labs(title = "Vaccine Supply (Daily)",
           subtitle = "*based on input assumptions.",
           y = "# of Vaccines",
           x = "Date") 
  })
  
  #### renderplot for daily_vac by age ####
  output$daily_vac <- renderPlot({
    dataInput()[["main"]] %>% 
    # main %>%   
      dplyr::select(date, policy, starts_with("Y", ignore.case = F)) %>% 
      replace(., is.na(.), 0) %>% 
      group_by(policy) %>% group_split() %>% map(arrange, date) %>% 
      map(mutate_at, vars(starts_with("Y", ignore.case = F)),
          cumsum) %>%
      bind_rows() %>% 
      pivot_longer(cols = starts_with("Y")) %>% 
      left_join(data.frame(name = paste0("Y",1:16),
                           pop = dataInput()[["size"]]),
                           # pop = r[["size"]]),
                by = "name") %>%
      mutate(name = factor(name,
                           levels = paste0("Y",1:16),
                           labels = c(paste0(seq(0,74,5),
                                             "-",
                                             seq(4,74,5)),
                                      "75+"))) %>% 
      ggplot(., aes(x = date,
                    y = value,
                    color = policy,
                    group = interaction(name, policy))) +
      geom_line() +
      facet_wrap(~name) +
      theme_bw() +
      theme(legend.position = "bottom",
            title = element_text(size = 24),
            strip.text = element_text(size = 20),
            axis.text = element_text(size = 20),
            axis.text.x = element_text(angle = 90),
            axis.title = element_text(size = 20)) +
      labs(x = "Date",
           y = "Vaccines Allocated",
           color = "Strategy",
           title = "Age Specific Vaccines Allocated") +
      ggsci::scale_color_lancet() +
      scale_y_continuous(labels = scientific_format())
  })
  
  
  #### renderPlot for Public Health Outcomes ####
  output$pho <- renderPlot({
    dataInput()[["main"]] %>% 
      # main %>% 
      dplyr::select(date, policy, death_o, cases, supply) %>% 
      pivot_longer(cols = c("death_o", "cases")) %>% 
      # filter(supply > 0) %>% 
      # mutate(value = if_else(is.na(value), 0, value)) %>% 
      # filter(compartment %in% c("death_o", "cases")) %>%
      # pivot_longer(starts_with("value")) %>% 
      # mutate(policy = if_else(name == "value.y", "0", policy)) %>%
      # distinct %>% dplyr::select(-name) %>% 
      # group_by(policy, t, compartment, run, population) %>% 
      # summarise(value = sum(value), .groups = "drop") %>% 
      group_by(policy, name) %>% group_split() %>%
      map(arrange, t) %>%
      map(mutate, value_cum = cumsum(value)) %>%
      bind_rows() %>% 
      pivot_longer(cols = c(value, value_cum),
                   names_to = "metric",
                   values_to = "value") %>% 
      # mutate(date = lubridate::ymd(dataInput()[["date_start"]]) + t) %>% 
      filter(date >= "2021-01-01") %>% 
      ggplot(., aes(x = date,
                    y = value,
                    group = policy,
                    color = policy)) +
      geom_line(size = 1.5, alpha = 0.8) +
      # geom_line(aes(x = date, y = supply))
      facet_wrap(~interaction(name, metric),
                 ncol = 2,
                 scale = "free") +
      theme_bw() +
      theme(legend.position = "bottom",
            title = element_text(size = 24),
            strip.text = element_text(size = 20),
            axis.text = element_text(size = 20), 
            axis.text.x = element_text(angle = 90),
            axis.title = element_text(size = 20)) +
      ggsci::scale_color_futurama() +
      labs(x = "Date",
           y = "",
           color = "Strategy",
           title = "")
  })
  
  output$econ <- renderPlot({
    # dataInput()[["econ"]]  %>% 
    econ %>% 
      separate(variable, into = c("tag", "unit")) %>% 
      mutate(unit = if_else(is.na(unit), 
                            "Overall Loss", 
                            "Per-dose Loss Reduction")) %>%
      mutate(var = factor(tag,
                          levels = c("LE", "adjLE", "adjQALEdisc",
                                     "VSLmlns", "QALYcases", "QALYloss",
                                     "doses"),
                          labels = c("Life Expectancy",
                                     "Comorbidity-adjusted Life Expectancy",
                                     "Quality-adjusted Life Expectancy",
                                     "VSL (mln. USD)",
                                     "Morbidity-related QALY",
                                     "Total QALY (AEFI + morbidity + mortality)",
                                     "Doses")),
             policy = parse_number(policy),
             policy = factor(policy,
                             levels = c(0:4))) %>% 
      filter(var != "Doses") %>% 
      ggplot(., aes(x = policy, 
                    y = value,
                    color = policy,
                    fill = policy)) +
      geom_bar(stat = "identity") +
      # geom_point() +
      facet_wrap(var~unit, scale =  "free") +
      ggsci::scale_color_futurama() +
      ggsci::scale_fill_futurama() +
      theme_bw() +
      theme(legend.position = "bottom",
            title = element_text(size = 24),
            strip.text = element_text(size = 18),
            legend.text = element_text(size = 18),
            axis.text = element_text(size = 18),
            axis.title = element_text(size = 18)) +
      labs(color = "", x = "Strategy", fill = "", y = "")
  })
}
