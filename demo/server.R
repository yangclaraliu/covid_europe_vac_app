server <- function(input, output, session) {
  output$ms_dates <- renderUI({
    lapply(1:input$n_ms, function(x) dateInput(paste0("date",x), 
                                      paste0("Milestone Date (", x, ")"), 
                                      value = as.character(as.Date(Sys.time())), # as.character(NA),
                                      format = "yyyy-mm-dd",
                                      min = "2020-01-01",
                                      max = "2022-12-31"
                                      ))
  })
  
  output$ms_covs <- renderUI({
    lapply(1:input$n_ms, function(x) numericInput(paste0("cov",x), 
                                                  paste0("Milestone Coverage (", x, ")"), 
                                                  value = if_else(x == 1, 
                                                                  0, 
                                                                  as.numeric(NA)
                                                                  ),
                                                  min = 0, 
                                                  max = if_else(x == 1, 0, 1), 
                                                  step = 0.01))
  })
  
  observe({
    cn_label <- countrycode::countrycode(input$cn, "country.name", "wb")
    updateDateInput(session,
                    inputId = "date_start",
                    value = model_selected[model_selected$WB == cn_label,
                                           "start_date"]  %>% 
                      as.numeric %>% 
                      as.Date(., origin = "1970-01-01") )
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
      # vaccination milestones (date)
      ms_date = sapply(1:input$n_ms, function(x){
        req(input[[paste0("date", x)]]);
        input[[paste0("date", x)]]
      }),
                # as.vector(input[[paste0("date", 1:input$n_ms)]]),
                # input[startsWith("date", names(input))] %>% unlist,
                # sort_input(input, "date"),
                # c(input$date1, input$date2), 
                # input$date3, input$date4, 
                # input$date5),
      ms_cov =  sapply(1:input$n_ms, function(x){
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
      priority = priority_policy,
      eff = rep(input$ve, 16),
      # c(natural immunity duration, vaccine induced immunity duration)
      wane = c(input$waning_nat, input$waning_vac),
      # basic reproduction number 
      R = input$rn
    )
  })
  
  # 
  output$test <- renderPrint({
    sapply(1:input$n_ms, function(x){
      input[[paste0("date", x)]]
    }) %>% print
    # paste(sapply(1:length(names(input)), function(x) (input[[names(input)[[x]]]])),
    #       collapse = "++++")
  })
  
  #### supply plot ####
  output$supply <- renderPlot({
    dataInput()[["supply"]]%>% 
    # r$supply %>% 
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
      mutate(date = lubridate::ymd(dataInput()[["date_start"]])+ t) %>% 
      ggplot(., aes(x = t,
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