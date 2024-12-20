# Stat-451-Final
Members: Ben Tanenbaum, Josephine Czeskleba, Oliver Brown, Thy Luong

## Plan
We will analyze CO₂ emissions, population, and GDP by country. Our data is from the following sources.

https://zenodo.org/records/10065794 

https://data.worldbank.org/indicator/NY.GDP.MKTP.CD

https://data.worldbank.org/indicator/SP.POP.TOTL

Our primary focus is tracking CO₂ emissions over time, both globally and in specific countries. We will use line plots to illustrate trends over time and bar plots to display percent change or overall change during an interval of years. Our goal is to utilize Shiny to make these plots interactive; the user will be able to select specific countries and specific ranges of years. To compare variables with each other, we will use a scatterplot with the option to choose one of CO₂ emissions, population, and GDP on the x and y-axes.

In addition, we aim to analyze the relationships between CO₂ emissions and other country-specific factors, population, and GDP. Specifically, we will examine the correlation between these variables and CO₂ emissions, assessing whether these correlations have increased or decreased over time.

For consistency between datasets, we are choosing to analyze only countries that currently exist. For countries that renamed, we will be using the current name. Similarly, we will only be presenting data from 1960 to 2023 so each country and dataset has values for every year.

## Preliminary Visualizations 

## Plot 1 
The plot displays annual CO₂ emissions in metric tonnes from the last 100 years. It focuses on the top three countries with the current highest carbon emissions and the global total. The y-axis ticks are labeled with their units to increase comprehension. In our final project, we plan to create a similar visualization- however, the year range and the countries displayed will be selectable to the viewer. 

## Plot 2
The plot displays annual population in billions of people from the last 80 years. It focuses on the top three countries with the current highest carbon emissions and the global total. The y-axis ticks are labeled in billions due to the high population of people in the world. The final version will be accompanied with selectable options to decide country and year range specified. The main purpose of this visualization to accompany analysis of CO₂ emissions since this will be one of the factors examined alongside CO₂ emissions.

## Plot 3

The plot presents global GDP, population, and CO₂ emissions for 2020 in a color-coded format, allowing for direct comparison across three separate maps. Each metric is represented on a gradient scale from blue to red, with red indicating higher values. The GDP map, scaled in billions of dollars, highlights major economies like the United States and China, while the population map, scaled to millions, emphasizes demographic giants such as China and India. The CO₂ emissions map, in thousands of metric tons, showcases high-emission regions, primarily industrialized nations. Together, these maps effectively illustrate the global landscape of economic power, population distribution, and environmental impact.

## Plot 4
The plot shows GDP since 1960 for the world and the three countries with the current highest carbon emissions. We plan to make the countries and year range selectable for the user in our Shiny application. You can conclude that global GDP is increasing exponentially while the US's and India's GDPs are increasing linearly. China's GDP has been increasing exponentially since 1980 and appears on track to surpass the US soon. The y-axis is measured in 2015 USD to provide a constant point of comparison while the x-axis displays the year to track the variables over time. 

