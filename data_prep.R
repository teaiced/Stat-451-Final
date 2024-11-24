library(shiny)
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(rnaturalearth)
library(sf)
library(patchwork)
library(scales)
library(RColorBrewer)
library(bslib)
library(ggrepel)

#setwd("~/Documents/Classes/Stat451")

gdp_data <- read_csv("data/gdp.csv", skip = 4, show_col_types = FALSE) %>%
  dplyr::select(-contains("...")) %>%
  dplyr::rename(Country = `Country Name`, Country_Code = `Country Code`) %>%
  pivot_longer(cols = matches("^\\d{4}$"), names_to = "Year", values_to = "GDP") %>%
  mutate(Year = as.integer(Year),
         Country = recode(Country, "Russian Federation" = "Russia"),
         Country = recode(Country, "United States" = "USA"),
         Country = recode(Country, "World" = "Global"))

pop_data <- read_csv("data/pop.csv", skip = 4, show_col_types = FALSE) %>%
  dplyr::select(-contains("...")) %>%
  dplyr::rename(Country = `Country Name`, Country_Code = `Country Code`) %>%
  pivot_longer(cols = matches("^\\d{4}$"), names_to = "Year", values_to = "Population") %>%
  mutate(Year = as.integer(Year),
         Country = recode(Country, "Russian Federation" = "Russia"),
         Country = recode(Country, "United States" = "USA"),
         Country = recode(Country, "World" = "Global"))

co2_data <- read_csv("data/co2.csv", show_col_types = FALSE) %>%
  dplyr::select(-contains("...")) %>%
  dplyr::rename(
    Country = `Country`,
    CO2_Emissions = `Total`,
    Country_Code = `ISO 3166-1 alpha-3`
  ) %>%
  mutate(
    Year = as.integer(Year)
  )

common_countries <- Reduce(intersect, list(gdp_data$Country, pop_data$Country, co2_data$Country))

gdp_data <- gdp_data %>% filter(Country %in% common_countries)
pop_data <- pop_data %>% filter(Country %in% common_countries)
co2_data <- co2_data %>% filter(Country %in% common_countries)

common_years <- Reduce(intersect, list(unique(gdp_data$Year), unique(pop_data$Year), unique(co2_data$Year)))
start_year <- min(common_years)
end_year <- max(common_years)

generate_shared_palette <- function(countries) {
  base_colors <- brewer_pal(palette = "Set1")(min(length(countries), 5))  
  extended_colors <- colorRampPalette(base_colors)(length(countries))    
  visible_colors <- setdiff(extended_colors, c("#FFFF33"))               
  names(visible_colors) <- countries                                    
  visible_colors
}


shared_palette <- scales::brewer_pal(palette = "Set4")(length(common_countries))
shared_palette <- generate_shared_palette(common_countries)

process_and_plot_bar <- function(data_list, variables, year_range, global_limits, change_type, countries) {   
  plots <- list()  
  global_min <- Inf  
  global_max <- -Inf 
  
  if (change_type == "percentage") {
    for (variable in variables) {
      data <- data_list[[variable]]
      value_col <- if (variable == "gdp") "GDP" else if (variable == "population") "Population" else "CO2_Emissions"
      
      filtered_data <- data %>%
        dplyr::filter(Country %in% countries, !is.na(!!rlang::sym(value_col)))
      
      for (country in unique(filtered_data$Country)) {
        country_data <- filtered_data[filtered_data$Country == country, ]
        
        start_year <- as.integer(year_range[1])
        end_year <- as.integer(year_range[2])
        start_year_actual <- country_data$Year[which.min(abs(country_data$Year - start_year))]
        end_year_actual <- country_data$Year[which.min(abs(country_data$Year - end_year))]
        
        if (is.na(start_year_actual) || is.na(end_year_actual)) next
        
        start_value <- country_data[[value_col]][which(country_data$Year == start_year_actual)]
        end_value <- country_data[[value_col]][which(country_data$Year == end_year_actual)]
        
        if (is.na(start_value) || is.na(end_value)) next
        
        percentage_change <- ifelse(start_value == 0, NA, (end_value - start_value) / abs(start_value) * 100)
        
        global_min <- min(global_min, percentage_change, na.rm = TRUE)
        global_max <- max(global_max, percentage_change, na.rm = TRUE)
      }
    }
    
    global_x_limits <- c(global_min * 1.1, global_max * 1.1)
    if (global_x_limits[1] > 0) global_x_limits[1] <- 0
    if (global_x_limits[2] < 0) global_x_limits[2] <- 0
  }
  
  for (variable in variables) {
    data <- data_list[[variable]]
    value_col <- if (variable == "gdp") "GDP" else if (variable == "population") "Population" else "CO2_Emissions"
    variable_name <- if (variable == "gdp") "GDP" else if (variable == "population") "Population" else "CO2 Emissions"
    
    filtered_data <- data %>%
      dplyr::filter(Country %in% countries, !is.na(!!rlang::sym(value_col)))
    
    combined_data <- data.frame()  # Initialize an empty data frame
    
    for (country in unique(filtered_data$Country)) {
      country_data <- filtered_data[filtered_data$Country == country, ]
      
      start_year <- as.integer(year_range[1])
      end_year <- as.integer(year_range[2])
      start_year_actual <- country_data$Year[which.min(abs(country_data$Year - start_year))]
      end_year_actual <- country_data$Year[which.min(abs(country_data$Year - end_year))]
      
      if (is.na(start_year_actual) || is.na(end_year_actual)) next
      
      start_value <- country_data[[value_col]][which(country_data$Year == start_year_actual)]
      end_value <- country_data[[value_col]][which(country_data$Year == end_year_actual)]
      
      if (is.na(start_value) || is.na(end_value)) next
      
      combined_data <- rbind(
        combined_data,
        data.frame(
          Country = country,
          Start_Value = start_value,
          Start_Year_Actual = start_year_actual,
          End_Value = end_value,
          End_Year_Actual = end_year_actual,
          Change = end_value - start_value,
          Percentage_Change = ifelse(start_value == 0, NA, (end_value - start_value) / abs(start_value) * 100)
        )
      )
    }
    
    if (change_type == "percentage") {
      combined_data <- combined_data[!is.na(combined_data$Percentage_Change), ]
      combined_data$x_var <- combined_data$Percentage_Change
      x_label <- "Percentage Change (%)"
      axis_label_format <- scales::label_number(suffix = " %")
      plot_x_limits <- global_x_limits
      title_suffix <- "Percentage Change"
    } else { 
      scaling_factor <- if (variable == "gdp") 1e12 else 1e6 
      unit_label <- if (variable == "gdp") "Trillion USD" else if (variable == "population") "Million People" else "Million Metric Tons"
      
      combined_data <- combined_data[!is.na(combined_data$Change), ]
      combined_data$x_var <- combined_data$Change / scaling_factor
      x_label <- paste("Change in", variable_name, "(", unit_label, ")", sep = " ")
      axis_label_format <- scales::label_number(scale = 1, suffix = paste0(" ", unit_label))
      
      plot_x_limits <- range(combined_data$x_var, na.rm = TRUE)
      plot_x_limits <- c(plot_x_limits[1] * 1.1, plot_x_limits[2] * 1.1)  
      if (plot_x_limits[1] > 0) plot_x_limits[1] <- 0
      if (plot_x_limits[2] < 0) plot_x_limits[2] <- 0
      title_suffix <- "Overall Change"
    }
    
    if (nrow(combined_data) == 0) {
      p <- ggplot() +
        geom_blank() +
        theme_minimal() +
        labs(
          title = paste(variable_name, "Change (", start_year, "-", end_year, ")", sep = ""),
          x = "",
          y = "",
          fill = "Change"
        ) +
        annotate("text", x = 0.5, y = 0.5, label = paste("No data available for", variable_name))
      plots[[variable]] <- p
      next
    }
    
    combined_data <- combined_data %>%
      dplyr::arrange(x_var) %>%
      dplyr::mutate(Country = factor(Country, levels = Country))  
    
    p <- ggplot(combined_data, aes(x = x_var, y = Country, fill = (x_var > 0))) +
      geom_vline(xintercept = 0, color = "black", linetype = "dashed") +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = c(`TRUE` = "darkgreen", `FALSE` = "red")) +
      scale_x_continuous(labels = axis_label_format, limits = plot_x_limits) +  
      theme_minimal() +
      labs(
        title = paste(variable_name, title_suffix, "(", start_year, "-", end_year, ")", sep = " "),
        x = x_label,
        y = "",
        fill = "Change"
      ) +
      theme(
        legend.position = "top",
        axis.text.y = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 18, face = "bold")
      )
    
    plots[[variable]] <- p
  }
  
  # Combine all plots using patchwork
  patchwork::wrap_plots(plots, ncol = 1)
}




process_and_plot_line <- function(data_list, variables, year_range, countries) {
  plots <- list()
  
  for (variable in variables) {
    data <- data_list[[variable]]
    value_col <- if (variable == "gdp") "GDP" else if (variable == "population") "Population" else "CO2_Emissions"
    variable_name <- if (variable == "gdp") "GDP" else if (variable == "population") "Population" else "CO2 Emissions"
    
    data_filtered <- data %>%
      group_by(Country) %>%
      filter(!all(is.na(!!sym(value_col)))) %>%
      ungroup() %>%
      filter(Country %in% countries)
    
    top_countries <- data_filtered %>%
      group_by(Country) %>%
      summarize(MaxValue = max(!!sym(value_col), na.rm = TRUE)) %>%
      filter(!is.infinite(MaxValue)) %>%
      arrange(desc(MaxValue)) %>%
      slice_head(n = 5) %>%
      pull(Country)
    
    data_filtered <- data_filtered %>%
      filter(Country %in% top_countries)
    
    # Adjusted label data
    label_data <- data_filtered %>%
      group_by(Country) %>%
      filter(Year == max(Year)) %>%
      mutate(x_label_pos = max(Year) + 1) %>%  # Position labels outside the plot range
      ungroup()
    
    p <- ggplot(data_filtered, aes(x = Year, y = !!sym(value_col), color = Country)) +
      geom_line(size = 1.2, na.rm = TRUE) +
      scale_color_manual(values = shared_palette) +
      theme_minimal() +
      labs(
        title = paste(variable_name, "Over Time"),
        x = "Year",
        y = variable_name
      ) +
      scale_x_continuous(
        limits = c(year_range[1], year_range[2] + 5),
        breaks = seq(year_range[1], year_range[2], by = 5)
      ) +
      scale_y_continuous(
        labels = if (variable_name == "GDP") {
          scales::label_number(scale = 1e-12, suffix = " T")
        } else if (variable_name == "Population") {
          scales::label_number(scale = 1e-9, suffix = " B")
        } else {
          scales::label_number(scale = 1e-6, suffix = " M")
        }
      ) +
      geom_text_repel(
        data = label_data,
        aes(label = Country, x = x_label_pos, y = !!sym(value_col)),
        nudge_x = 0.5,
        hjust = 0,
        size = 5,  # Increased font size for labels
        box.padding = 0.3,
        point.padding = 0.1,
        show.legend = FALSE
      ) +
      theme(
        legend.position = "none",
        axis.text.x = element_text(size = 14),  # Increased font size for x-axis text
        axis.text.y = element_text(size = 14),  # Increased font size for y-axis text
        axis.title = element_text(size = 16),   # Increased font size for axis titles
        plot.title = element_text(size = 18, face = "bold")  # Increased font size for plot title
      )
    
    plots[[variable]] <- p
  }
  
  wrap_plots(plots, nrow = length(plots))
}
