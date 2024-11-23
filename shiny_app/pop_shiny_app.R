library(shiny)
library(tidyverse)
library(ggplot2)
library(forcats)

gdp_data <- read_csv("data/gdp.csv", skip = 4, show_col_types = FALSE) %>%
  select(-contains("...")) %>%
  rename(Country = `Country Name`, Country_Code = `Country Code`) %>%
  pivot_longer(cols = matches("^\\d{4}$"), names_to = "Year", values_to = "GDP") %>%
  mutate(Year = as.integer(Year),
         Country = recode(Country, "Russian Federation" = "Russia"),
         Country = recode(Country, "World" = "Global"))

pop_data <- read_csv("data/pop.csv", skip = 4, show_col_types = FALSE) %>%
  select(-contains("...")) %>%
  rename(Country = `Country Name`, Country_Code = `Country Code`) %>%
  pivot_longer(cols = matches("^\\d{4}$"), names_to = "Year", values_to = "Population") %>%
  mutate(Year = as.integer(Year),
         Country = recode(Country, "Russian Federation" = "Russia"),
         Country = recode(Country, "World" = "Global"),
         Population = Population/1e9)

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

setdiff(pop_data$Country, gdp_data$Country)
setdiff(pop_data$Country, co2_data$Country)
setdiff(pop_data$Country, co2_data$Country)



pop <- read.csv("data/pop.csv", skip = 4)
pop <- pop %>%
  pivot_longer(
    cols = starts_with("X"), 
    names_to = "year",          
    values_to = "pop"
  )


ui <- fluidPage(
  
    sliderInput("years", "Choose the year range", min=1961, max=2023, value=c(1961, 2023), sep=""),
    selectInput("countries", "Select countries:", 
                choices = unique(common_countries),
                multiple = TRUE),
    
    plotOutput("pop_line")
  
)

server <- function(input, output, session) {
  
  filtered_pop <- reactive({
     pop_data %>% filter(Year >= input$years[1] & Year <= input$years[2])
  })
  
  output$pop_line <- renderPlot({
    ggplot(filtered_pop() %>% filter(Country %in% input$countries), 
           aes(x=Year, y=Population, color=Country)) +
      geom_line(aes(color=Country)) +
      scale_color_brewer(palette = "Set1") +
      
      ggtitle("How has population changed over time?") +
      labs(subtitle = "Population in billions of people",
           color = "") +
      xlab("Year") +
      ylab("") +
      scale_y_continuous(labels = scales::label_number(suffix = " billion", accuracy = 1.0))
  })
  
}

shinyApp(ui, server)

