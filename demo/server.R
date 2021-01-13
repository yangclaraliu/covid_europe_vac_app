
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