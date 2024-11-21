setwd("~/Documents/Classes/Stat451")

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

setdiff(pop_data$Country, gdp_data$Country)
setdiff(pop_data$Country, co2_data$Country)
setdiff(pop_data$Country, co2_data$Country)