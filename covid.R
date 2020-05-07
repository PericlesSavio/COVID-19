#Fontes dos dados: https://github.com/wcota/covid19br

#cases per states
#download.file('https://raw.githubusercontent.com/wcota/covid19br/master/cases-brazil-states.csv', './cases-brazil-states-time.csv')
cases_brazil_states_time <- read.csv('https://raw.githubusercontent.com/wcota/covid19br/master/cases-brazil-states.csv', sep = ",", header = TRUE, encoding = "UTF-8", stringsAsFactors=FALSE)
#cases_brazil_states_time <- read.csv('./cases-brazil-states-time.csv', sep = ",", header = TRUE, encoding = "UTF-8", stringsAsFactors=FALSE)

#cases per states
#download.file('https://raw.githubusercontent.com/wcota/covid19br/master/cases-brazil-total.csv', './cases-brazil-states.csv')
cases_brazil_states <- read.csv('https://raw.githubusercontent.com/wcota/covid19br/master/cases-brazil-total.csv', sep = ",", header = TRUE, encoding = "UTF-8", stringsAsFactors=FALSE)
#cases_brazil_states <- read.csv('./cases-brazil-states.csv', sep = ",", header = TRUE, encoding = "UTF-8", stringsAsFactors=FALSE)

#GPS coord.
#download.file('https://raw.githubusercontent.com/wcota/covid19br/master/gps_cities.csv', './csv/gps_cities.csv')
gps_cities <- read.csv('https://raw.githubusercontent.com/wcota/covid19br/master/gps_cities.csv', sep = ",", header = TRUE, encoding = "UTF-8", stringsAsFactors=FALSE)
#gps_cities <- read.csv('./csv/gps_cities.csv', sep = ",", header = TRUE, encoding = "UTF-8", stringsAsFactors=FALSE)

#cases per cities
#download.file('https://raw.githubusercontent.com/wcota/covid19br/master/cases-brazil-cities.csv', './csv/cases-brazil-cities.csv')
cases_brazil_cities <- read.csv('https://raw.githubusercontent.com/wcota/covid19br/master/cases-brazil-cities.csv', sep = ",", header = TRUE, encoding = "UTF-8", stringsAsFactors=FALSE)
#cases_brazil_cities <- read.csv('./csv/cases-brazil-cities.csv', sep = ",", header = TRUE, encoding = "UTF-8", stringsAsFactors=FALSE)


#============================#
#tabelas e variáveis de apoio
#============================#
cases_brazil_cities_gps <- merge(cases_brazil_cities, gps_cities, by="ibgeID", all.x = TRUE) # fundir tabela pelo codigo IBGE das cidades
cases_brazil_cities_gps <- filter(cases_brazil_cities_gps, cases_brazil_cities_gps$totalCases > 0) #filtrar cidades sem casos
cases_brazil_cities_gps_deaths <- filter(cases_brazil_cities_gps, cases_brazil_cities_gps$deaths > 0) #filtrar cidades sem óbitos

atualizacao <- as.Date(max(cases_brazil_states_time$date))
atualizacao_format <- format(atualizacao, "%d %b %Y")


#============================#
#funções
#============================#

#plotar mapa com casos ou mortes
mapa <- function(tipo) {

          #plotar mapa com casos
          #plot(map_brazil$geom) #com detalhes de estados, pesado 12mb
          par(mar=c(1,1,1,1))
          map("worldHires","Brazil") #library(maps) library(mapdata)
          
          if (tipo == "casos") {
                    points(cases_brazil_cities_gps$lon, cases_brazil_cities_gps$lat, col = "red", pch = 21, bg = "red",  cex = 0.4)
          } else if (tipo == "mortes") {
                    points(cases_brazil_cities_gps_deaths$lon, cases_brazil_cities_gps_deaths$lat, col = "red", pch = 21, bg = "red",  cex = 0.1)
          }
}


# Dataframe
DT_cases_brazil_states <- data.frame(
          "Estado" = cases_brazil_states$state,
          "Casos" = cases_brazil_states$totalCases,
          "Óbitos" = cases_brazil_states$deaths,
          "Casos (100 mil)" = cases_brazil_states$totalCases_per_100k_inhabitants,
          "Óbitos (100 mil)" = cases_brazil_states$deaths_per_100k_inhabitants,
          "Óbitos por Casos" = cases_brazil_states$deaths_by_totalCases,
          "Testes" = cases_brazil_states$tests,
          "Testes (100 mil)" = cases_brazil_states$tests_per_100k_inhabitants,
          "Recuperados" = cases_brazil_states$recovered
)


# mapa leaflet
mapa_interativo <- function(N) {
          
          #remover as localizações em coordenadas
          cases_brazil_cities_gps <- filter(cases_brazil_cities_gps, cases_brazil_cities_gps$lat != 'is.na') 
          
          #data frame de apoio
          covid_dados <- data.frame(
                    city = cases_brazil_cities_gps$city,
                    lat = cases_brazil_cities_gps$lat,
                    long = cases_brazil_cities_gps$lon,
                    cases = cases_brazil_cities_gps$totalCases,
                    deaths = cases_brazil_cities_gps$deaths,
                    cases_100k = cases_brazil_cities_gps$totalCases_per_100k_inhabitants,
                    deaths_100k = cases_brazil_cities_gps$deaths_per_100k_inhabitants,
                    death_ratio = cases_brazil_cities_gps$deaths_by_totalCases,
                    factor = NA
          )
          
          # apoio leaflet (escala, lagendas)
          leaflet_menu <- data.frame(
                    "legenda" = c("log10(CASOS)","log10(MORTES)"),
                    "escala_i" = c(0, 0),
                    "escala_f" = c(5, 5),
                    "escala_by" = c(1, 1)
          )
          
          #  switch: // 1 = log10 casos, 2 = log10 mortes
          if (N == 1) {
                    covid_dados$factor <- log10(covid_dados$cases)
                    tamanho <- ((covid_dados$factor^2)*1.2)+2
          } else if (N == 2) {
                    covid_dados <- filter(covid_dados, covid_dados$deaths>0)
                    covid_dados$factor <- log10(covid_dados$deaths)
                    tamanho <- ((covid_dados$factor^2)*1.2)+2
          }
          
          #paleta de cores para legenda
          palette <- colorBin(palette=c("#00FF00","#FF0000"), domain=NULL, na.color="transparent",
                                bins=seq(leaflet_menu$escala_i[N], leaflet_menu$escala_f[N], leaflet_menu$escala_by[N]))
          
          #mural com info
          label_mapa <- paste(
                    "<h3>", covid_dados$city, "</h3>",
                    "<b>CASOS: </b>", covid_dados$cases, "<br/>",
                    round(covid_dados$cases_100k, 2), " casos/100 mil hab.<br/><br/>",
                    "<b>ÓBITOS: </b>", covid_dados$deaths, "<br/>",
                    round(covid_dados$deaths_100k, 2), " óbitos/100 mil hab.<br/><br/>",
                    "<b>FATALIDADE</b><br>", round(covid_dados$death_ratio*100, 2), "%<br/>",
                    covid_dados$factor,
                    sep = ""
          )
          
          lapply(label_mapa, htmltools::HTML)
          
          #plotar o mapa
          m <- leaflet()
          m <- addTiles(m)
          m <- addProviderTiles(m, providers$OpenStreetMap.Mapnik)
          m <- setView(m, lat=-15, lng=-56 , zoom=4)
          m <- addCircleMarkers(m, covid_dados$long, covid_dados$lat,
                                fillColor = palette(covid_dados$factor), fillOpacity = 1, color="black", radius=tamanho, stroke=FALSE,
                                label = label_mapa,
                                labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                                            textsize = "13px", direction = "auto")
                                
          )
          m <- addLegend(m, pal=palette, values=covid_dados$factor, opacity=1, title = leaflet_menu$legenda[N], position = "bottomright" )
          m
}