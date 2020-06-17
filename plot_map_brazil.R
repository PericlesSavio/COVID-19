# mapa leaflet
brazil_covid_cities <- function(N) {
        #library(leaflet)
        
        cities_gps <- read.csv("./brazil_gps.csv")
        
        cases_brazil_cities_gps <- merge(brazil_cities, cities_gps, by = "ibgeID")
        
        #remover as localizações em coordenadas
        #cases_brazil_cities_gps <- filter(cases_brazil_cities_gps, cases_brazil_cities_gps$lat != 'is.na') 
        
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
                covid_dados <- filter(covid_dados, covid_dados$cases>0)
                covid_dados$factor <- log10(covid_dados$cases)
                tamanho <- ((covid_dados$factor^1.2)*1.0)+1
        } else if (N == 2) {
                covid_dados <- filter(covid_dados, covid_dados$deaths>0)
                covid_dados$factor <- log10(covid_dados$deaths)
                tamanho <- ((covid_dados$factor^1.2)*1.0)+1
        }
        
        #paleta de cores para legenda
        palette <- colorBin(palette=c("#00FF00","#FF0000"), domain=NULL, na.color="transparent",
                            bins=seq(leaflet_menu$escala_i[N], leaflet_menu$escala_f[N], leaflet_menu$escala_by[N]))
        
        #mural com info
        mytext <- paste(
                "<h3>", covid_dados$city, "</h3>",
                "<b>CASOS: </b>", covid_dados$cases, "<br/>",
                round(covid_dados$cases_100k, 2), " casos/100 mil hab.<br/><br/>",
                "<b>ÓBITOS: </b>", covid_dados$deaths, "<br/>",
                round(covid_dados$deaths_100k, 2), " óbitos/100 mil hab.<br/><br/>",
                "<b>FATALIDADE</b><br>", round(covid_dados$death_ratio*100, 2),"%",
                sep = ""
        ) %>%
                lapply(htmltools::HTML)
        
        #plotar o mapa
        m <- leaflet(height="100%", width="100%")
        m <- addTiles(m)
        m <- addProviderTiles(m, providers$Stamen.TonerLite)
        m <- setView(m, lat=-15, lng=-56 , zoom=4)
        m <- addCircleMarkers(m, covid_dados$long, covid_dados$lat,
                              fillColor = palette(covid_dados$factor), fillOpacity = 1, color="black", radius=tamanho, stroke=FALSE,
                              label = mytext,
                              labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                                          textsize = "13px", direction = "auto")
                              
        )
        m <- addLegend(m, pal=palette, values=covid_dados$factor, opacity=1, title = leaflet_menu$legenda[N], position = "bottomright" )
        m
}