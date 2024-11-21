library(shiny)
library(tidyverse)
library(ggplot2)
library(forcats)

ui <- fluidPage(
  
    sliderInput("years", "Choose the year range", min=1961, max=2023, value=c(1961, 2023), sep=""),
    selectInput("countries", "Select countries:", 
                choices = unique(pop$Country.Name),
                multiple = TRUE),
    
    plotOutput("pop_line")
  
)

server <- function(input, output, session) {
  
    pop <- read.csv("data/pop.csv", skip = 4)
  pop <- pop %>%
    pivot_longer(
      cols = starts_with("X"), 
      names_to = "year",          
      values_to = "pop"
    )
  pop$year <- substr(pop$year, 2, nchar(pop$year))
  pop$year <- as.numeric(pop$year)
  pop <- pop %>% filter(Indicator.Name == "Population, total")
  pop$Country.Name <- replace(pop$Country.Name, pop$Country.Name == "Russian Federation", "Russia")
  pop$Country.Name <- replace(pop$Country.Name, pop$Country.Name == "World", "Global")
  pop$pop <- pop$pop/1e9
  
  
  filtered_pop <- reactive({
     pop %>% filter(year >= input$years[1] & year <= input$years[2])
  })
  
  output$pop_line <- renderPlot({
    ggplot(filtered_pop() %>% filter(Country.Name %in% input$countries), 
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

shinyApp(ui, server)

