library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(rnaturalearth)
library(sf)
library(patchwork)
library(scales)
library(zoo)
library(bslib)
library(tidyverse)
library(skimr)
library(gapminder)
library(readr)
library(shiny)
library(knitr)

gdp <- read_csv("gdp.csv", skip = 3)
  gdp <- subset(gdp, `Country Code` == 'USA' | `Country Code` == 'CHN' | `Country Code` == 'IND' | `Country Code` == 'WLD')
  gdp <- gdp %>% mutate(`Country Name` = str_replace(`Country Name`, "World", "Global"))
  gdp <- gdp %>% pivot_longer(cols=c('1960', '1961', '1962', '1963', '1964', '1965', '1966', '1967', '1968', '1969', '1970', '1971', '1972', '1973', '1974', '1975', '1976', '1977', '1978', '1979', '1980', '1981', '1982', '1983', '1984', '1985', '1986', '1987', '1988', '1989', '1990', '1991', '1992', '1993', '1994', '1995', '1996', '1997', '1998', '1999', '2000', '2001', '2002', '2003', '2004', '2005', '2006', '2007', '2008', '2009', '2010', '2011', '2012', '2013', '2014', '2015', '2016', '2017', '2018', '2019', '2020', '2021', '2022', '2023'), names_to = 'Year', values_to = 'GDP')
  gdp$Year <- as.numeric(gdp$Year)
  gdp$GDP <- gdp$GDP/1e12 
  
  co2 <- read.csv('co2.csv')
  co2_filtered <- reactive({
    co2 %>%
      filter(Year >= input$years[1] & Year <= input$years[2]) %>%
      filter(Country %in% c("USA", "India", "China", "Global"))
  })
  
  gdp_data <- read_csv("gdp.csv", skip = 4, show_col_types = FALSE) %>%
    select(-contains("...")) %>%
    dplyr::rename(
      Country = `Country Name`,
      Country_Code = `Country Code`
    ) %>%
    pivot_longer(
      cols = matches("^\\d{4}$"),
      names_to = "Year",
      values_to = "GDP"
    ) %>%
    mutate(Year = as.integer(Year))
  
  pop_data <- read_csv("pop.csv", show_col_types = FALSE) %>%
    select(-contains("...")) %>%
    dplyr::rename(
      Country = `Country Name`,
      Country_Code = `Country Code`
    ) %>%
    pivot_longer(
      cols = matches("^\\d{4}$"),
      names_to = "Year",
      values_to = "Population"
    ) %>%
    mutate(Year = as.integer(Year))
  
  co2_data <- read_csv("co2.csv", show_col_types = FALSE) %>%
    select(-contains("...")) %>%
    dplyr::rename(
      Country = `Country`,
      CO2_Emissions = `Total`,
      Country_Code = `ISO 3166-1 alpha-3`
    ) %>%
    mutate(Year = as.integer(Year))
  
  country_mapping <- c(
    "USA" = "United States",
    "United States of America" = "United States",
    "United Kingdom of Great Britain and Northern Ireland" = "United Kingdom",
    "Republic of Korea" = "South Korea",
    "Democratic People's Republic of Korea" = "North Korea",
    "Egypt, Arab Rep." = "Egypt",
    "Iran (Islamic Republic of)" = "Iran",
    "Venezuela (Bolivarian Republic of)" = "Venezuela",
    "Viet Nam" = "Vietnam",
    "Syrian Arab Republic" = "Syria",
    "Côte d'Ivoire" = "Cote d'Ivoire",
    "United Republic of Tanzania" = "Tanzania",
    "Lao People's Democratic Republic" = "Laos",
    "Bolivia (Plurinational State of)" = "Bolivia",
    "Congo, Dem. Rep." = "Democratic Republic of the Congo",
    "Congo, Rep." = "Republic of the Congo",
    "Gambia, The" = "Gambia",
    "Yemen, Rep." = "Yemen",
    "Slovak Republic" = "Slovakia",
    "Bahamas, The" = "Bahamas",
    "Brunei Darussalam" = "Brunei",
    "Czech Republic" = "Czechia",
    "Macedonia, FYR" = "North Macedonia",
    "Hong Kong SAR, China" = "Hong Kong",
    "Russian Federation" = "Russia"
  )
  
  standardize_countries <- function(data) {
    data %>%
      mutate(Country = recode(Country, !!!country_mapping))
  }
  
  gdp_data <- standardize_countries(gdp_data)
  pop_data <- standardize_countries(pop_data)
  co2_data <- standardize_countries(co2_data)
  
  world <- ne_countries(scale = "medium", returnclass = "sf")
  
  common_years <- Reduce(intersect, list(unique(gdp_data$Year), unique(pop_data$Year), unique(co2_data$Year)))
  
  start_year <- min(common_years)
  end_year <- max(common_years)
  
  process_and_plot <- function(data, value_col, variable_name, selected_year, world_data) {
    data_year <- data %>%
      filter(Year == selected_year)
    
    if (nrow(data_year) == 0) {
      stop(paste("No data available for", variable_name, "in the selected year:", selected_year))
    }
    
    data_year <- data_year %>%
      group_by(Country_Code) %>%
      mutate(
        Value = .data[[value_col]],
        Value = ifelse(is.na(Value), mean(Value, na.rm = TRUE), Value)
      ) %>%
      ungroup()
    
    map_data <- world_data %>%
      left_join(data_year, by = c("iso_a3" = "Country_Code"))
    
    max_value <- max(map_data$Value, na.rm = TRUE)
    if (!is.finite(max_value) || max_value <= 0) {
      max_value <- 1
    }
    scale_limits <- c(0, max_value)
    
    plot <- ggplot(map_data) +
      geom_sf(aes(fill = Value), color = "black", size = 0.1) +
      scale_fill_gradientn(
        colors = c("white", "lightcoral", "red", "darkred"),
        limits = scale_limits,
        na.value = "grey80",
        labels = function(x) {
          if (grepl("GDP", variable_name)) {
            scales::dollar(x, scale = 1e-9, suffix = " B")
          } else if (grepl("Population", variable_name)) {
            scales::number(x, scale = 1e-6, suffix = " M")
          } else {
            scales::number(x, scale = 1e-3, suffix = " K")
          }
        }
      ) +
      theme_void() +
      theme(
        legend.position = "right",
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 10)
      ) +
      labs(
        title = variable_name,
        fill = if (grepl("GDP", variable_name)) "$ GDP (Billions)" else variable_name
      )
    
    return(plot)
  }
  
pop <- read.csv("pop.csv")
    pop <- pop %>%
      pivot_longer(
        cols = starts_with("X"), 
        names_to = "year",          
        values_to = "pop"
      )
    pop$year <- substr(pop$year, 2, nchar(pop$year))
    pop$year <- as.numeric(pop$year)
    pop <- pop %>% filter(Indicator.Name == "Population, total")
    pop$Country.Name <- replace(pop$Country.Name, pop$Country.Name == "United States", "USA")
    pop$Country.Name <- replace(pop$Country.Name, pop$Country.Name == "World", "Global")
    pop$pop <- pop$pop/1e9

