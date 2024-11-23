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

gdp_data <- read_csv("data/gdp.csv", skip = 4, show_col_types = FALSE) %>%
  select(-contains("...")) %>%
  rename(Country = `Country Name`, Country_Code = `Country Code`) %>%
  pivot_longer(cols = matches("^\\d{4}$"), names_to = "Year", values_to = "GDP") %>%
  mutate(Year = as.integer(Year))

pop_data <- read_csv("data/pop.csv", skip = 4, show_col_types = FALSE) %>%
  select(-contains("...")) %>%
  rename(Country = `Country Name`, Country_Code = `Country Code`) %>%
  pivot_longer(cols = matches("^\\d{4}$"), names_to = "Year", values_to = "Population") %>%
  mutate(Year = as.integer(Year))

co2_data <- read_csv("data/co2.csv", show_col_types = FALSE) %>% 
  select(-contains("...")) %>% 
  rename(
    Country = `Country`,
    CO2_Emissions = `Total`,
    Country_Code = `ISO 3166-1 alpha-3`
  ) %>% 
  mutate(
    Year = as.integer(Year),
    Country = recode(Country, "USA" = "United States")
  )

setdiff(pop_data$Country, gdp_data$Country)
setdiff(pop_data$Country, co2_data$Country)
setdiff(pop_data$Country, co2_data$Country)

common_countries <- Reduce(intersect, list(gdp_data$Country, pop_data$Country, co2_data$Country))

gdp_data <- gdp_data %>% filter(Country %in% common_countries)
pop_data <- pop_data %>% filter(Country %in% common_countries)
co2_data <- co2_data %>% filter(Country %in% common_countries)


common_years <- Reduce(intersect, list(unique(gdp_data$Year), unique(pop_data$Year), unique(co2_data$Year)))
start_year <- min(common_years)
end_year <- max(common_years)

process_and_plot_line <- function(data, value_col, variable_name, top_countries, year_range, map_countries) {
  data <- data %>% filter(Country_Code %in% map_countries)
  
  data$Color <- ifelse(data$Country %in% top_countries, global_color_mapping[data$Country], "grey80")
  
  ggplot(data, aes(x = Year, y = !!sym(value_col), group = Country)) +
    geom_line(data = data %>% filter(!Country %in% top_countries), color = "grey80", size = 0.8) +
    geom_line(data = data %>% filter(Country %in% top_countries), aes(color = Country), size = 1) +
    scale_color_manual(values = global_color_mapping) +
    ggtitle(paste(variable_name, "Over Time")) +
    ylab(variable_name) +
    xlab("Year") +
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
        scales::label_number(scale = 1e-3, suffix = " K")
      }
    ) +
    theme_minimal() +
    theme(
      legend.position = "right",
      plot.title = element_text(size = 14, face = "bold"),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10),
      legend.text = element_text(size = 10)
    )
}

all_countries <- unique(c(gdp_data$Country, pop_data$Country, co2_data$Country))
