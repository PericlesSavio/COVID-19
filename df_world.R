world_covid <- read.csv('https://covid.ourworldindata.org/data/owid-covid-data.csv', sep = ",", header = TRUE, encoding = "UTF-8", stringsAsFactors=FALSE)[,1:16]


df_world_countries <- function() {
        # Source: world_covid
        last_date <- as.Date(max(world_covid$date))
        last_date = last_date - 1
        
        # Removing "World"
        world_covid2 <- filter(world_covid, world_covid$date == last_date)
        world_covid2 <- filter(world_covid2, world_covid2$location != "World")
        
        # Fixind Spain's bug
        last_date1 <- as.Date(max(world_covid$date))-1
        world_covid_spain <- filter(world_covid, world_covid$location == "Spain")
        world_covid_spain <- filter(world_covid_spain, world_covid_spain$date == last_date1)
        
        # Merging the two tables
        world_covid_data <- rbind(world_covid2, world_covid_spain)
        world_covid_data <- arrange(world_covid_data, desc(total_cases))
        world_covid_data
}

world_cases = df_world_countries()$total_cases[is.na(df_world_countries()$total_cases) == FALSE]
world_deaths = df_world_countries()$total_deaths[is.na(df_world_countries()$total_deaths) == FALSE]


df_world_stats <- data.frame (
        cases = format(sum(world_cases), decimal.mark = ",", big.mark=".", digits = 7, nsmall = 0),
        deaths = format(sum(world_deaths), decimal.mark = ",", big.mark=".", digits = 2, nsmall = 0),
        recovered = NA
)
df_world_stats
