function(input, output) {
  
  filtered_gdp <- reactive({
    gdp %>% filter(Year >= input$slider[1] & Year <= input$slider[2])
  })
  
  output$gdp_line <- renderPlot({ ggplot(filtered_gdp(), aes(x = `Year`, y = `GDP`, group = `Country Name`, color = `Country Name`)) + geom_line() + ggtitle("How have GDPs of China, India, the United States, \nand the World Changed Over Time?") + ylab("GDP (constant 2017 international $)") + theme(legend.title = element_blank()) + scale_color_manual(limits=c('Global', 'China', 'United States', 'India'), values=c('darkgreen', 'red', 'purple', 'blue')) + scale_y_continuous(labels = scales::label_number(suffix = " trillion", accuracy = 1.0))
  })
  
  co2_filtered <- reactive({
    co2 %>%
      filter(Year >= input$years[1] & Year <= input$years[2]) %>%
      filter(Country %in% c("USA", "India", "China", "Global"))
  })
  
  output$co2_line_plot <- renderPlot({
    ggplot(co2_filtered(), aes(x = Year, y = Total, color = Country)) +
      geom_line() +
      labs(
        title = "How Have CO2 Emissions Changed Over Time?",
        subtitle = "Carbon Dioxide emissions in metric tonnes",
        y = "Emissions (billion tonnes)",
        color = "Country"
      ) +
      
      scale_y_continuous(
        labels = label_number(scale = 1e-3, suffix = " billion t"),
        breaks = seq(0, max(co2_filtered()$Total, na.rm = TRUE), by = 5e3)
      ) +
      theme_minimal()
  })
  
  output$finalPlot <- renderPlot({
    selected_years <- input$year_range
    
    if (length(selected_years) != 2) {
      stop("Please select exactly two years.")
    }
    
    selected_years <- sort(selected_years)
    
    plots <- list()
    for (year in selected_years) {
      gdp_plot <- process_and_plot(gdp_data, "GDP", paste("GDP -", year), year, world)
      pop_plot <- process_and_plot(pop_data, "Population", paste("Population -", year), year, world)
      co2_plot <- process_and_plot(co2_data, "CO2_Emissions", paste("CO2 Emissions -", year), year, world)
      
      combined_plot <- (gdp_plot / pop_plot / co2_plot) +
        plot_annotation(title = paste("Year:", year))
      
      plots <- c(plots, list(combined_plot))
    }
    
    final_plot <- (plots[[1]] | plots[[2]]) +
      plot_annotation(title = paste("Comparison of GDP, Population, and CO2 Emissions for", selected_years[1], "and", selected_years[2]))
    
    print(final_plot)
  })
    
    filtered_pop <- reactive({
      pop %>% filter(year >= input$years[1] & year <= input$years[2])
      
    })
    
    output$filtered_table <- renderTable({
      filtered_pop()
    })
    
    output$pop_line <- renderPlot({
      ggplot(filtered_pop() %>% filter(Country.Name %in% c("Global", "India", "China", "USA")), 
             aes(x=year, y=pop, color=Country.Name)) +
        geom_line(aes(color=Country.Name)) +
        scale_color_brewer(palette = "Set1") +
        
        ggtitle("How has population changed over time?") +
        labs(subtitle = "Population in billions of people",
             color = "") +
        xlab("Year") +
        ylab("") +
        scale_y_continuous(labels = scales::label_number(suffix = " billion", accuracy = 1.0))
    })
}