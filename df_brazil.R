# Brazil
brazil_pop <- read.csv('./brazil_pop.csv', sep = ",", header = TRUE, encoding = "UTF-8", stringsAsFactors=FALSE)
brazil_states <- read.csv('https://raw.githubusercontent.com/wcota/covid19br/master/cases-brazil-total.csv', sep = ",", header = TRUE, encoding = "UTF-8", stringsAsFactors=FALSE)
brazil_states_time <- read.csv('https://raw.githubusercontent.com/wcota/covid19br/master/cases-brazil-states.csv', sep = ",", header = TRUE, encoding = "UTF-8", stringsAsFactors=FALSE)
brazil_cities <- read.csv('https://raw.githubusercontent.com/wcota/covid19br/master/cases-brazil-cities.csv', sep = ",", header = TRUE, encoding = "UTF-8", stringsAsFactors=FALSE)
brazil_cities <- filter(brazil_cities, brazil_cities$totalCases > 0)

df_brazil_states <- function() {
        
        #merge cases/deaths... with population
        brazil_states_pop <- merge(brazil_states, brazil_pop, by.x = "state", by.y = "Sigla", all = FALSE)
        
        df_states <- data.frame(
                "Estados" = brazil_states_pop$state,
                "Casos" = brazil_states_pop$totalCasesMS,
                "Óbitos" = brazil_states_pop$deathsMS,
                "Letalidade" = brazil_states_pop$deaths_by_totalCases*100,
                "Recuperados" = brazil_states_pop$recovered,
                "Testes" = brazil_states_pop$tests,
                "Casos/100k" = brazil_states_pop$totalCasesMS/brazil_states_pop$População*100000,
                "Mortes/100k" = brazil_states_pop$deathsMS/brazil_states_pop$População*100000,
                "Recuperados/100k" = brazil_states_pop$recovered/brazil_states_pop$População*100000,
                "Testes/100k" = brazil_states_pop$tests/brazil_states_pop$População*100000
        )
        
        brazil_states_pop[is.na(brazil_states_pop)] <- 0
        brazil <- aggregate(brazil_states_pop[,c(4,7,12,14)], by = data.frame(brazil_states_pop$country), FUN = sum)
        brazil$brazil_states_pop.country = "Brasil"
        
        df_brazil <- data.frame(
                "Estados" = brazil$brazil_states_pop.country,
                "Casos" = brazil$totalCasesMS,
                "Óbitos" = brazil$deathsMS,
                "Letalidade" = brazil$deathsMS/brazil$totalCasesMS*100,
                "Recuperados" = brazil$recovered,
                "Testes" = brazil$tests,
                "Casos/100k" = brazil$totalCasesMS/sum(brazil_states_pop$População)*100000,
                "Mortes/100k" = brazil$deathsMS/sum(brazil_states_pop$População)*100000,
                "Recuperados/100k" = brazil$recovered/sum(brazil_states_pop$População)*100000,
                "Testes/100k" = brazil$tests/sum(brazil_states_pop$População)*100000
        )
        
        df_brazil_covid <- rbind(df_brazil, df_states) # df means 'dataframe'
        #df_brazil_covid <- df_brazil_covid[order(-df_brazil_covid$Casos, -df_brazil_covid$Óbitos),]
        
        df_brazil_formated <- data.frame(
                "Estados" = df_brazil_covid$Estados,
                "Casos" = format(df_brazil_covid$Casos, decimal.mark = ",", big.mark="."),
                "Óbitos" = format(df_brazil_covid$Óbitos, decimal.mark = ",", big.mark="."),
                "Letalidade" = paste(format(df_brazil_covid$Letalidade, decimal.mark = ",", big.mark=".", digits = 2, nsmall = 2),"%", sep = ""),
                "Recuperados" = format(df_brazil_covid$Recuperados, decimal.mark = ",", big.mark="."),
                "Testes" = format(df_brazil_covid$Testes, decimal.mark = ",", big.mark="."),
                "Casos_por_100k" = format(df_brazil_covid$Casos.100k, decimal.mark = ",", big.mark=".", digits = 2, nsmall = 2),
                "Mortes_por_100k" = format(df_brazil_covid$Mortes.100k, decimal.mark = ",", big.mark=".", digits = 2, nsmall = 2),
                "Recuperados_por_100k" = format(df_brazil_covid$Recuperados.100k, decimal.mark = ",", big.mark=".", digits = 2, nsmall = 2),
                "Testes_por_100k" = format(df_brazil_covid$Testes.100k, decimal.mark = ",", big.mark=".", digits = 2, nsmall = 2)
        )
        
        df_brazil_formated <- arrange(df_brazil_formated, desc(Casos))
        df_brazil_formated
}

df_brazil_cities <- function() {
        df_cities_br <- data.frame(
                "Cidade" = brazil_cities$city,
                "Estado" = brazil_cities$state,
                "Casos" = format(brazil_cities$totalCases, decimal.mark = ",", big.mark="."),
                "Óbitos" = format(brazil_cities$deaths, decimal.mark = ",", big.mark="."),
                "Letalidade" = paste(format(brazil_cities$deaths_by_totalCases*100, decimal.mark = ",", big.mark=".", digits = 2, nsmall = 2),"%", sep = ""),
                "Casos_por_100k" = format(brazil_cities$totalCases_per_100k_inhabitants, decimal.mark = ",", big.mark=".", digits = 2, nsmall = 2),
                "Óbitos_por_100k" = format(brazil_cities$deaths_per_100k_inhabitants, decimal.mark = ",", big.mark=".", digits = 2, nsmall = 2)
        )
        df_cities_br <- arrange(df_cities_br, desc(Casos))
        df_cities_br
}

# Cities statistics

cities_cases <- filter(brazil_cities, brazil_cities$totalCases > 0 & brazil_cities$ibgeID > 1000000); #remove cities with zero cases / remove unknown locations
number_cities_cases <- nrow(cities_cases)
        
cities_deaths <- filter(brazil_cities, brazil_cities$deaths > 0 & brazil_cities$ibgeID > 1000000);
number_cities_deaths <- nrow(cities_deaths)
        
total_cities <- 5570
        
cities_percentage <- paste(format(number_cities_cases/total_cities*100, decimal.mark = ",", big.mark=".", digits = 2, nsmall = 2),"%", sep = "")
        
        

