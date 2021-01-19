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
             helpText("Overall vaccine coverage is assumed to reach 50% on 2022-12-31.")
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
             column(4,
                    h4("Vaccination Progress Milestones"),
                    uiOutput("ms_dates")
                    ),
             column(4,
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
      eff = rep(input$ve, 16),
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
   #  main %>%
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
      geom_point() +
      scale_alpha_manual(values = c(1,0)) +
      theme_bw() +
      theme(legend.position = "none",
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
  
  #### renderplot for daily_vac ####
  output$daily_vac <- renderPlot({
    dataInput()[["daily_vac"]] %>% 
      mutate(date =   dataInput()[["supply"]]$milestone_date[1] + as.numeric(t)) %>% 
      dplyr::select(-t, -supply) %>%
      group_by(policy) %>% group_split() %>% 
      map(mutate_at, vars(starts_with("Y")), cumsum) %>% 
      bind_rows() %>% 
      # mutate_at(vars(starts_with("Y")), cumsum) %>% 
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
                    color = policy,
                    group = interaction(name, policy))) +
      # geom_point() +
      geom_line() +
      facet_wrap(~name) +
      theme_bw() +
      theme(legend.position = "bottom",
            title = element_text(size = 20),
            strip.text = element_text(size = 16),
            axis.title = element_text(size = 16),
            axis.text.x = element_text(angle = 90)) + 
      labs(x = "Date",
           y = "Coverage",
           color = "Strategy",
           title = "Age Specific Vaccination Progress") +
      ggsci::scale_color_lancet()
  })
  
  
  #### renderPlot for Public Health Outcomes ####
  output$pho <- renderPlot({
    dataInput()[["dynamics"]] %>% 
      filter(compartment %in% c("death_o", "cases")) %>%
      full_join(dataInput()[["dynamics_baseline"]] %>% 
                  filter(compartment %in% c("death_o", "cases")),
                by = c("t", "group", "compartment","population", "run")) %>% 
      pivot_longer(starts_with("value")) %>% 
      mutate(policy = if_else(name == "value.y", "0", policy)) %>%
      distinct %>% dplyr::select(-name) %>% 
      group_by(policy, t, compartment, run, population) %>% 
      summarise(value = sum(value), .groups = "drop") %>% 
      group_by(policy, compartment) %>% group_split() %>% 
      map(arrange, t) %>% 
      map(mutate, value_cum = cumsum(value)) %>% 
      bind_rows() %>% 
      pivot_longer(starts_with("value")) %>% 
      mutate(date = lubridate::ymd(dataInput()[["date_start"]]) + t) %>% 
      ggplot(., aes(x = date,
                    y = value,
                    group = policy,
                    color = policy)) +
      geom_line(size = 1.5) +
      facet_wrap(~interaction(compartment, name), scale = "free")+
      theme_bw() +
      theme(legend.position = "bottom",
            title = element_text(size = 20),
            strip.text = element_text(size = 16),
            legend.text = element_text(size = 16),
            axis.title = element_text(size = 16)) +
      ggsci::scale_color_lancet() +
      labs(x = "Date",
           y = "",
           color = "Strategy",
           title = "")
  })
  
  output$econ <- renderPlot({
    dataInput()[["econ"]]  %>% 
      separate(name, into = c("tag", "var","unit")) %>% 
      mutate(unit = if_else(is.na(unit), 
                            "Overall Loss Reduction", 
                            "Per-dose Loss Reduction")) %>%
      mutate(var = factor(var,
                          levels = c("LE", "adjLE", "adjQALEdisc",
                                     "VSLmlns", "QALYcases", "QALYloss"),
                          labels = c("Life Expectancy",
                                     "Comorbidity-adjusted Life Expectancy",
                                     "Quality-adjusted Life Expectancy",
                                     "VSL (mln. USD)",
                                     "Morbidity-related QALY",
                                     "Total QALY (AEFI + morbidity + mortality)")),
             policy = parse_number(policy),
             policy = factor(policy,
                             levels = c(0:4))) %>% 
      ggplot(., aes(x = policy, 
                    y = value,
                    color = policy,
                    fill = policy)) +
      geom_bar(stat = "identity") +
      facet_wrap(~var) +
      ggsci::scale_color_lancet() +
      ggsci::scale_fill_lancet() +
      theme_bw() +
      theme(legend.position = "bottom",
            title = element_text(size = 20),
            strip.text = element_text(size = 16),
            legend.text = element_text(size = 16),
            axis.title = element_text(size = 16))
  })
}