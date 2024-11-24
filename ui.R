ui <- fluidPage(
  theme = bs_theme(
    bg = "white",
    fg = "#333333",
    primary = "#8B668B",
    base_font = font_google("Inter")
  ),
  titlePanel("Global Trends in GDP, Population, and CO2 Emissions"),
  sidebarLayout(
    sidebarPanel(
      sliderInput(
        "year_range",
        "Select Year Range:",
        min = start_year,
        max = end_year,
        value = c(1990, 2010),
        step = 1,
        sep = ""
      ),
      checkboxGroupInput(
        "variables",
        "Select Variables to Display:",
        choices = c("GDP" = "gdp", "Population" = "population", "CO2 Emissions" = "co2"),
        selected = c("gdp", "population", "co2")
      ),
      selectizeInput(
        "countries",
        "Select up to 5 countries:",
        choices = unique(common_countries),
        selected = "Global",
        multiple = TRUE,
        options = list(maxItems = 5)
      ),
      # Conditional "Change Type" input
      conditionalPanel(
        condition = "input['tabs'] == 'Bar Plot'",
        radioButtons(
          "change_type",
          "Select Change Type:",
          choices = c("Percentage Change" = "percentage", "Overall Chnage" = "absolute"),
          selected = "percentage"
        )
      ),
      # Conditional scatter plot inputs
      conditionalPanel(
        condition = "input['tabs'] == 'Scatter Plot'",
        selectInput(
          "scatter_x",
          "Select X-axis Variable for Scatter Plot:",
          choices = c("GDP" = "gdp", "Population" = "population", "CO2 Emissions" = "co2")
        ),
        selectInput(
          "scatter_y",
          "Select Y-axis Variable for Scatter Plot:",
          choices = c("GDP" = "gdp", "Population" = "population", "CO2 Emissions" = "co2"),
          selected = "population"
        )
      ),
      helpText("Use the slider to select year range and checkboxes to choose variables.")
    ),
    mainPanel(
      tabsetPanel(
        id = "tabs",  # Add an ID to the tabsetPanel for referencing
        tabPanel("Bar Plot", plotOutput("bar_plots", height = "800px")),
        tabPanel("Line Plot", plotOutput("line_plots", height = "800px")),
        tabPanel("Scatter Plot", plotOutput("scatter_plot", height = "800px"))
      )
    )
  )
)
