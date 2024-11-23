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
         Country = recode(Country, "World" = "Global"))

pop_data <- read_csv("data/pop.csv", skip = 4, show_col_types = FALSE) %>%
  dplyr::select(-contains("...")) %>%
  dplyr::rename(Country = `Country Name`, Country_Code = `Country Code`) %>%
  pivot_longer(cols = matches("^\\d{4}$"), names_to = "Year", values_to = "Population") %>%
  mutate(Year = as.integer(Year),
         Country = recode(Country, "Russian Federation" = "Russia"),
         Country = recode(Country, "World" = "Global"))

co2_data <- read_csv("data/co2.csv", show_col_types = FALSE) %>%
  dplyr::select(-contains("...")) %>%
  dplyr::rename(
    Country = `Country`,
    CO2_Emissions = `Total`,
    Country_Code = `ISO 3166-1 alpha-3`
  ) %>%
  mutate(
    Year = as.integer(Year),
    Country = recode(Country, "USA" = "United States")
  )

common_countries <- Reduce(intersect, list(gdp_data$Country, pop_data$Country, co2_data$Country))

gdp_data <- gdp_data %>% filter(Country %in% common_countries)
pop_data <- pop_data %>% filter(Country %in% common_countries)
co2_data <- co2_data %>% filter(Country %in% common_countries)

common_years <- Reduce(intersect, list(unique(gdp_data$Year), unique(pop_data$Year), unique(co2_data$Year)))
start_year <- min(common_years)
end_year <- max(common_years)

process_and_plot_bar <- function(data_list, variables, year_range, global_limits, change_type, countries) {
  plots <- list()
  
  for (variable in variables) {
    data <- data_list[[variable]]
    value_col <- if (variable == "gdp") "GDP" else if (variable == "population") "Population" else "CO2_Emissions"
    variable_name <- if (variable == "gdp") "GDP" else if (variable == "population") "Population" else "CO2 Emissions"
    
    start_year <- as.integer(year_range[1])
    end_year <- as.integer(year_range[2])
    
    data_filtered <- data %>%
      filter(Year %in% c(start_year, end_year)) %>%
      filter(Country %in% countries) %>%
      dplyr::select(Country, Year, !!sym(value_col)) %>%
      pivot_wider(names_from = Year, values_from = !!sym(value_col)) %>%
      rename(
        Start_Value = !!sym(as.character(start_year)),
        End_Value = !!sym(as.character(end_year))
      ) %>%
      drop_na(c("Start_Value", "End_Value")) %>%
      mutate(
        Change = End_Value - Start_Value,
        Percentage_Change = ifelse(Start_Value == 0, NA, (Change / abs(Start_Value)) * 100)
      )
    
    if (change_type == "percentage") {
      data_filtered <- data_filtered %>%
        filter(is.finite(Percentage_Change)) %>%
        mutate(x_var = Percentage_Change)
      x_label <- "Percentage Change (%)"
      x_limits <- global_limits$percentage
      title_suffix <- "Percentage Change"
    } else {
      if (variable == "gdp") {
        scaling_factor <- 1e12  
        unit_label <- "Trillions"
      } else if (variable == "population") {
        scaling_factor <- 1e6   
        unit_label <- "Millions"
      } else {
        scaling_factor <- 1e6   
        unit_label <- "Million Metric Tons"
      }
      
      data_filtered <- data_filtered %>%
        filter(is.finite(Change)) %>%
        mutate(
          x_var = Change / scaling_factor
        )
      
      x_label <- paste("Change in", variable_name, "(", unit_label, ")", sep = " ")
      x_limits <- global_limits$absolute[[variable]]
      title_suffix <- "Absolute Change"
    }
    
    if (nrow(data_filtered) == 0) {
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
    
    data_filtered <- data_filtered %>%
      arrange(desc(x_var))
    
    data_filtered <- rbind(
      head(data_filtered, 10),
      tail(data_filtered, 10)
    )
    
    p <- ggplot(data_filtered, aes(x = x_var, y = reorder(Country, x_var), fill = (x_var > 0))) +
      geom_vline(xintercept = 0, color = "black", linetype = "dashed") +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = c(`TRUE` = "darkgreen", `FALSE` = "red")) +
      theme_minimal() +
      labs(
        title = paste(variable_name, title_suffix, "(", start_year, "-", end_year, ")", sep = " "),
        x = x_label,
        y = "",
        fill = "Change"
      ) +
      xlim(x_limits) +
      theme(
        legend.position = "top",
        axis.text.y = element_text(size = 6),
        axis.title = element_text(size = 10),
        plot.title = element_text(size = 12, face = "bold")
      )
    
    plots[[variable]] <- p
  }
  
  wrap_plots(plots, ncol = 1)
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
      filter(!is.infinite(MaxValue)) %>%  # Exclude -Inf values
      arrange(desc(MaxValue)) %>%
      slice_head(n = 5) %>%
      pull(Country)
    
    data_filtered <- data_filtered %>%
      filter(Country %in% top_countries)
    
    p <- ggplot(data_filtered, aes(x = Year, y = !!sym(value_col), color = Country)) +
      geom_line(size = 1.2, na.rm = TRUE) +
      scale_color_brewer(palette = "Set1") +
      theme_minimal() +
      labs(
        title = paste(variable_name, "Over Time"),
        x = "Year",
        y = variable_name
      ) +
      scale_x_continuous(
        limits = year_range,
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
      theme(
        legend.position = "none"
      ) +
      xlim(year_range[1], year_range[2] + (year_range[2] - year_range[1])/5) +
      geom_text_repel(aes(label = Country),
                      data = data_filtered %>% filter(Year == year_range[2]),
                      hjust = 1)
    
    plots[[variable]] <- p
  }
  
  wrap_plots(plots, ncol = length(plots))
}

