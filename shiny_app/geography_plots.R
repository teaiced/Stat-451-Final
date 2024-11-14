
library(shiny)
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(rnaturalearth)
library(sf)
library(patchwork)
library(scales)
library(zoo)

setwd("~/Documents/Classes/Stat451")

gdp_data <- read_csv("data/gdp.csv", skip = 4, show_col_types = FALSE) %>%
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

pop_data <- read_csv("data/pop.csv", skip = 4, show_col_types = FALSE) %>%
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

co2_data <- read_csv("data/co2.csv", show_col_types = FALSE) %>%
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

ui <- fluidPage(
  titlePanel("Comparison of GDP, Population, and CO2 Emissions Between Two Years"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("year_range", "Select Years:",
                  min = start_year,
                  max = end_year,
                  value = c(1990, 2000),
                  step = 1,
                  sep = "",
                  animate = animationOptions(interval = 1000, loop = FALSE)),
      helpText("Use the slider to select two years for comparison.")
    ),
    mainPanel(
      plotOutput("finalPlot", height = "1200px")
    )
  )
)

server <- function(input, output) {
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
}

shinyApp(ui = ui, server = server)
