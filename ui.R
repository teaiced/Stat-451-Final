ui <- fluidPage(
  theme = bs_theme(
    bg = "#F0FFF0",
    fg = "#333333",
    primary = "#8B668B",
    base_font = font_google("Inter")
  ),
  titlePanel("Global Trends in GDP, Population, and CO2 Emissions"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("year_range", "Select Year Range:", min = start_year, max = end_year, value = c(1990, 2010), step = 1, sep = ""),
      checkboxGroupInput("variables", "Select Variables to Display:",
                         choices = c("GDP" = "gdp", "Population" = "population", "CO2 Emissions" = "co2"),
                         selected = c("gdp", "population", "co2")),
      selectInput("countries", "Select countries:", 
                  choices = unique(common_countries),
                  multiple = TRUE),
      radioButtons("change_type", "Select Change Type:",
                   choices = c("Percentage Change" = "percentage"),
                   selected = "percentage"),
      helpText("Use the slider to select year range and checkboxes to choose variables.")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Bar Plot", plotOutput("bar_plots", height = "800px")),
        tabPanel("Line Plot", plotOutput("line_plots", height = "400px"))
      )
    )
  )
)
