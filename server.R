server <- function(input, output, session) {
  reactive_data <- reactive({
    list(
      gdp = gdp_data %>% filter(Year >= input$year_range[1], Year <= input$year_range[2]),
      population = pop_data %>% filter(Year >= input$year_range[1], Year <= input$year_range[2]),
      co2 = co2_data %>% filter(Year >= input$year_range[1], Year <= input$year_range[2])
    )
  })
  
  output$bar_plots <- renderPlot({
    data_list <- reactive_data()
    variables <- input$variables
    year_range <- c(input$year_range[1], input$year_range[2])
    change_type <- input$change_type
    
    if (change_type == "percentage") {
      all_percentage_changes <- unlist(lapply(variables, function(variable) {
        data <- data_list[[variable]]
        value_col <- if (variable == "gdp") "GDP" else if (variable == "population") "Population" else "CO2_Emissions"
        start_year <- as.integer(year_range[1])
        end_year <- as.integer(year_range[2])
        
        data_filtered <- data %>%
          filter(Year %in% c(start_year, end_year)) %>%
          dplyr::select(Year, Country, !!sym(value_col)) %>%
          pivot_wider(names_from = Year, values_from = !!sym(value_col)) %>%
          drop_na(c(as.character(start_year), as.character(end_year))) %>%
          filter(!!sym(as.character(start_year)) != 0) %>%
          mutate(
            Percentage_Change = ((!!sym(as.character(end_year))) - (!!sym(as.character(start_year)))) / abs(!!sym(as.character(start_year))) * 100
          ) %>%
          filter(is.finite(Percentage_Change)) %>%
          pull(Percentage_Change)
      }))
      
      max_limit <- ceiling(max(all_percentage_changes, na.rm = TRUE))
      min_limit <- floor(min(all_percentage_changes, na.rm = TRUE))
      global_limits <- list(percentage = c(min_limit, max_limit))
    } else {
      global_limits <- list(absolute = list())
      for (variable in variables) {
        data <- data_list[[variable]]
        value_col <- if (variable == "gdp") "GDP" else if (variable == "population") "Population" else "CO2_Emissions"
        start_year <- as.integer(year_range[1])
        end_year <- as.integer(year_range[2])
        
        changes <- data %>%
          filter(Year %in% c(start_year, end_year)) %>%
          dplyr::select(Year, Country, !!sym(value_col)) %>%
          pivot_wider(names_from = Year, values_from = !!sym(value_col)) %>%
          drop_na(c(as.character(start_year), as.character(end_year))) %>%
          mutate(
            Change = (!!sym(as.character(end_year))) - (!!sym(as.character(start_year)))
          ) %>%
          pull(Change)
        
        max_limit <- ceiling(max(changes, na.rm = TRUE))
        min_limit <- floor(min(changes, na.rm = TRUE))
        global_limits$absolute[[variable]] <- c(min_limit, max_limit)
      }
    }
    
    plot <- process_and_plot_bar(data_list, variables, year_range, global_limits, change_type)
    plot
  })
  
  output$line_plots <- renderPlot({
    data_list <- reactive_data()
    variables <- input$variables
    year_range <- c(input$year_range[1], input$year_range[2])
    
    plot <- process_and_plot_line(data_list, variables, year_range)
    plot
  })
}
