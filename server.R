function(input, output, session) {
  
  reactive_data <- reactive({
    list(
      gdp = gdp_data %>% filter(Year >= input$year_range[1], Year <= input$year_range[2]),
      population = pop_data %>% filter(Year >= input$year_range[1], Year <= input$year_range[2]),
      co2 = co2_data %>% filter(Year >= input$year_range[1], Year <= input$year_range[2])
    )
  })
  
  output$maps <- renderPlot({
    data_list <- reactive_data()
    selected_years <- c(input$year_range[1], input$year_range[2])
    variables <- input$variables
    plots <- list()
    
    for (variable in variables) {
      for (year in selected_years) {
        if (variable == "gdp") {
          plot <- process_and_plot_map(data_list$gdp, "GDP", "GDP", year, world)
        } else if (variable == "population") {
          plot <- process_and_plot_map(data_list$population, "Population", "Population", year, world)
        } else if (variable == "co2") {
          plot <- process_and_plot_map(data_list$co2, "CO2_Emissions", "CO2 Emissions", year, world)
        }
        plots <- c(plots, list(plot))
      }
    }
    
    wrap_plots(plots, ncol = 2)
  })
  
  output$line_plots <- renderPlot({
    data_list <- reactive_data()
    variables <- input$variables
    year_range <- input$year_range
    
    map_countries <- world$iso_a3 %>% na.omit() %>% unique()
    
    all_countries <- unique(unlist(lapply(variables, function(variable) {
      if (variable == "gdp") {
        data_list$gdp %>% filter(Country_Code %in% map_countries) %>% pull(Country)
      } else if (variable == "population") {
        data_list$population %>% filter(Country_Code %in% map_countries) %>% pull(Country)
      } else if (variable == "co2") {
        data_list$co2 %>% filter(Country_Code %in% map_countries) %>% pull(Country)
      }
    })))
    
    plots <- list()
    
    for (variable in variables) {
      if (variable == "gdp") {
        filtered_data <- data_list$gdp %>% filter(Country_Code %in% map_countries)
        top_countries <- filtered_data %>%
          group_by(Country) %>%
          summarize(MaxValue = max(GDP, na.rm = TRUE)) %>%
          arrange(desc(MaxValue)) %>%
          slice_head(n = 5) %>%
          pull(Country)
        plot <- process_and_plot_line(filtered_data, "GDP", "GDP", top_countries, year_range, map_countries)
      } else if (variable == "population") {
        filtered_data <- data_list$population %>% filter(Country_Code %in% map_countries)
        top_countries <- filtered_data %>%
          group_by(Country) %>%
          summarize(MaxValue = max(Population, na.rm = TRUE)) %>%
          arrange(desc(MaxValue)) %>%
          slice_head(n = 5) %>%
          pull(Country)
        plot <- process_and_plot_line(filtered_data, "Population", "Population", top_countries, year_range, map_countries)
      } else if (variable == "co2") {
        filtered_data <- data_list$co2 %>% filter(Country_Code %in% map_countries)
        top_countries <- filtered_data %>%
          group_by(Country) %>%
          summarize(MaxValue = max(CO2_Emissions, na.rm = TRUE)) %>%
          arrange(desc(MaxValue)) %>%
          slice_head(n = 5) %>%
          pull(Country)
        plot <- process_and_plot_line(filtered_data, "CO2_Emissions", "CO2 Emissions", top_countries, year_range, map_countries)
      }
      plots <- c(plots, list(plot))
    }
    
    wrap_plots(plots, ncol = 3)
  })
}