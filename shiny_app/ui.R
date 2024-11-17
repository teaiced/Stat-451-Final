fluidPage(
  
  sliderInput(inputId="slider",
              label="Years",
              min=1961, max=2023, value=c(1961, 2023)),
  plotOutput("gdp_line"),

  titlePanel("CO2 Emissions Over Time"),
  sidebarLayout(
    sidebarPanel(
      sliderInput(
        inputId = "years", 
        label = "Select year range:", 
        min = 1961, 
        max = 2023, 
        value = c(1961, 2023)
      )
    ),
    mainPanel(
      plotOutput(outputId = "co2_line_plot")
    )
  ),
  
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
  ),
  
  sliderInput("years", "Choose the year range", min=1961, max=2023, value=c(1961, 2023), sep=""),
  plotOutput("pop_line")
)