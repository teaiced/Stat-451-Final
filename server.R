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
    countries <- input$countries
    
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
    
    plot <- process_and_plot_bar(data_list, variables, year_range, global_limits, change_type, countries)
    plot
  })
  
  output$line_plots <- renderPlot({
    data_list <- reactive_data()
    variables <- input$variables
    year_range <- c(input$year_range[1], input$year_range[2])
    countries <- input$countries
    
    plot <- process_and_plot_line(data_list, variables, year_range, countries)
    plot
  })
  
  output$scatter_plot <- renderPlot({
    data_list <- reactive_data()
    x_var <- input$scatter_x
    y_var <- input$scatter_y
    countries <- input$countries
    
    # Define the columns based on selected variables
    x_col <- if (x_var == "gdp") "GDP" else if (x_var == "population") "Population" else "CO2_Emissions"
    y_col <- if (y_var == "gdp") "GDP" else if (y_var == "population") "Population" else "CO2_Emissions"
    
    # Filter data for selected countries and variables
    scatter_data <- data_list[[x_var]] %>%
      select(Country, Year, x = !!sym(x_col)) %>%
      inner_join(
        data_list[[y_var]] %>%
          select(Country, Year, y = !!sym(y_col)),
        by = c("Country", "Year")
      ) %>%
      filter(Country %in% countries) %>%
      drop_na(x, y)
    
    # Check if data exists
    if (nrow(scatter_data) == 0) {
      return(
        ggplot() +
          geom_blank() +
          theme_minimal() +
          labs(
            title = "No Data Available for Scatter Plot",
            x = ifelse(x_var == "gdp", "Gross Domestic Product", tools::toTitleCase(input$scatter_x)),
            y = ifelse(y_var == "gdp", "Gross Domestic Product", tools::toTitleCase(input$scatter_y))
          )
      )
    }
    
    scatter_data <- scatter_data %>%
      group_by(Country) %>%
      mutate(Year_Scaled = scales::rescale(Year, to = c(0.5, 1))) %>%
      ungroup()
    
    last_year_data <- scatter_data %>%
      group_by(Country) %>%
      filter(Year == max(Year)) %>%
      ungroup()
    
    # Define custom axis label formatting
    x_label_format <- if (x_var == "gdp") {
      scales::label_number(scale = 1e-12, suffix = " Trillion USD")  # GDP in trillions of USD
    } else if (x_var == "population") {
      scales::label_number(scale = 1e-9, suffix = " Billion People")  # Population in billions
    } else if (x_var == "co2") {
      scales::label_number(scale = 1e-3, suffix = " Billion Metric Tons")  # CO2 in millions of metric tons
    } else {
      scales::label_number()
    }
    
    y_label_format <- if (y_var == "gdp") {
      scales::label_number(scale = 1e-12, suffix = " Trillion USD")  # GDP in trillions of USD
    } else if (y_var == "population") {
      scales::label_number(scale = 1e-9, suffix = " Billion People")  # Population in billions
    } else if (y_var == "co2") {
      scales::label_number(scale = 1e-3, suffix = " Billion Metric Tons")  # CO2 in millions of metric tons
    } else {
      scales::label_number()
    }
    
    
    # Scatter plot with improved aesthetics and formatted axis labels
    ggplot(scatter_data, aes(x = x, y = y, group = Country, color = Country, alpha = Year_Scaled)) +
      # Draw paths as arrows
      geom_segment(
        data = scatter_data %>% dplyr::arrange(Country, Year) %>% dplyr::group_by(Country) %>%
          dplyr::mutate(xend = dplyr::lead(x), yend = dplyr::lead(y)) %>% dplyr::ungroup(),
        aes(x = x, y = y, xend = xend, yend = yend),
        arrow = arrow(length = unit(0.15, "cm"), type = "closed"),
        lineend = "round",
        size = 1
      ) +
      # Scale adjustments
      scale_color_manual(
        values = shared_palette,
        guide = "none"  # Remove legend for color
      ) +
      scale_alpha(range = c(0.3, 1), guide = "none") +  # Alpha for year shading
      scale_x_continuous(labels = x_label_format) +
      scale_y_continuous(labels = y_label_format) +
      # Add last-year labels
      geom_text_repel(
        data = last_year_data,
        aes(label = Country),
        size = 6,
        nudge_x = 0.2,
        nudge_y = 0.2,
        box.padding = 0.3,
        point.padding = 0.1,
        show.legend = FALSE,
        color = "black"
      ) +
      # Theme and labels
      theme_minimal(base_size = 14) +
      labs(
        title = paste("Scatter Plot of", ifelse(x_var == "gdp", "Gross Domestic Product", tools::toTitleCase(input$scatter_x)),
                      "vs", ifelse(y_var == "gdp", "Gross Domestic Product", tools::toTitleCase(input$scatter_y)), "Over Selected Years"),
        x = ifelse(x_var == "gdp", "Gross Domestic Product", tools::toTitleCase(input$scatter_x)),
        y = ifelse(y_var == "gdp", "Gross Domestic Product", tools::toTitleCase(input$scatter_y))
      ) +
      theme(
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),  # Centered and bold title
        axis.title = element_text(size = 14),  # Larger axis titles
        axis.text = element_text(size = 12),  # Larger axis text
        panel.grid.major = element_line(color = "gray85"),  # Subtle grid lines
        panel.grid.minor = element_blank(),  # Hide minor grid lines
        panel.border = element_rect(color = "gray80", fill = NA)  # Add subtle panel border
      )
  })
  
}
