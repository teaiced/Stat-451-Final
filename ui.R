fluidPage(
  titlePanel("Global Trends in GDP, Population, and CO2 Emissions"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("year_range", "Select Year Range:", min = start_year, max = end_year, value = c(1990, 2010), step = 1, sep = ""),
      checkboxGroupInput("variables", "Select Variables to Display:", 
                         choices = c("GDP" = "gdp", "Population" = "population", "CO2 Emissions" = "co2"),
                         selected = c("gdp", "population", "co2")),
      helpText("Use the slider to select year range and checkboxes to choose variables.")
    ),
    mainPanel(
      plotOutput("maps", height = "800px"),      
      plotOutput("line_plots", height = "400px") 
    )
  )
)