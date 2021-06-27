library('magrittr')

world_covid_data = read.csv('https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/latest/owid-covid-latest.csv')

#world_covid_data = world_data

# X<-world_covid_data[,-grep("B|D",colnames(X))]

world_cases = world_covid_data$total_cases[is.na(world_covid_data$total_cases) == FALSE]
world_deaths = world_covid_data$total_deaths[is.na(world_covid_data$total_deaths) == FALSE]


df_world_stats <- data.frame (
        cases = format(world_covid_data[world_covid_data$location == 'World', ]$total_cases, decimal.mark = ",", big.mark=".", digits = 7, nsmall = 0),
        deaths = format(world_covid_data[world_covid_data$location == 'World', ]$total_deaths, decimal.mark = ",", big.mark=".", digits = 2, nsmall = 0),
        recovered = NA
)
df_world_stats