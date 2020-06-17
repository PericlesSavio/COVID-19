# # World COVID19 CSV
# world_covid <- read.csv('https://covid.ourworldindata.org/data/owid-covid-data.csv',
#                         sep = ",", header = TRUE, encoding = "UTF-8", stringsAsFactors=FALSE)[,1:16]
# 
# # Aggregated dataframe
# world_covid_sum <- function() {
#         # Source: world_covid
#         last_date <- as.Date(max(world_covid$date))
#         
#         # Removing "World"
#         world_covid2 <- filter(world_covid, world_covid$date == last_date)
#         world_covid2 <- filter(world_covid2, world_covid2$location != "World")
#         
#         # Fixind Spain's bug
#         last_date1 <- as.Date(max(world_covid$date))-1
#         world_covid_spain <- filter(world_covid, world_covid$location == "Spain")
#         world_covid_spain <- filter(world_covid_spain, world_covid_spain$date == last_date1)
#         
#         # Merging the two tables
#         world_covid_data <- rbind(world_covid2, world_covid_spain)
#         world_covid_data <- arrange(world_covid_data, desc(total_cases))
#         world_covid_data
# }

plot_curves_top10_countries <- function(N) {
        # Excluding Brazil / Selecting the top9 countries in cases
        country <- unique(filter(df_world_countries(), df_world_countries()$location != "Brazil")$location, incomparables = FALSE)[1:9]
        pandemic_date <- NA
        selected_countries <- filter(world_covid, world_covid$location %in% country)
        
        # Switch
        switch(N,
               {
                       selected_countries$serie <- selected_countries$new_cases ; serie_N <- 30 ; text_ <- "casos" ;
                       title_ <- 'Casos diários por COVID-19' ; dia_ <- "Dias a partir do 1º dia com 30 ou mais casos" ;
                       brazil <- filter(world_covid, world_covid$location == "Brazil" & world_covid$date >= "2020-03-16") ;
                       brazil$serie <- brazil$new_cases
               },
               {
                       selected_countries$serie <- selected_countries$new_deaths ; serie_N <- 5 ; text_ <- "óbitos" ;
                       title_ <- 'Óbitos diários por COVID-19' ; dia_ <- "Dias a partir do 1º dia com 5 ou mais óbitos" ;
                       brazil <- filter(world_covid, world_covid$location == "Brazil" & world_covid$date >= "2020-03-21") ;
                       brazil$serie <- brazil$new_deaths
               },
               {
                       selected_countries$serie <- selected_countries$total_cases ; serie_N <- 100 ; text_ <- "casos" ;
                       title_ <- 'Casos diários por COVID-19' ; dia_ <- "Dias a partir do 100º caso confirmado" ;
                       brazil <- filter(world_covid, world_covid$location == "Brazil" & world_covid$date >= "2020-03-15") ;
                       brazil$serie <- brazil$total_cases
               },
               {
                       selected_countries$serie <- selected_countries$total_deaths ; serie_N <- 5 ; text_ <- "óbitos" ;
                       title_ <- 'Óbitos diários por COVID-19' ; dia_ <- "Dias a partir do 5º óbito confirmado" ;
                       brazil <- filter(world_covid, world_covid$location == "Brazil" & world_covid$date >= "2020-03-21") ;
                       brazil$serie <- brazil$total_deaths
               }
        )
        
        # Generating a list/dataframe with first day since confirmed cases first reached 30 per day
        for (i in 1:9) {
                pandemic_date[i] <- filter(selected_countries,
                                           selected_countries$location == country[i] & selected_countries$serie > serie_N)[1,4]
        }
        country_list <- cbind(country, pandemic_date)
        country_list <- as.data.frame(country_list)
        country_list$pandemic_date <- as.Date(country_list$pandemic_date)
        
        # Generating a dataframe with necessary data to plot
        world_data_plot <- as.data.frame(matrix(nrow = 0, ncol = 0))
        for (i in 1:9) {
                world_data_plot <- rbind(world_data_plot, filter(selected_countries,
                                selected_countries$location == country_list[i,1] & selected_countries$date >= country_list[i,2]))
        }

        # Removing negative and zero value
        world_data_plot <- filter(world_data_plot, world_data_plot$serie >0)
        
        # Plot
        fig <- plot_ly(x = 1:nrow(brazil), y = brazil$serie, name = "Brazil", type = 'scatter', mode = 'lines', height=620,
                       text = format(brazil$serie, decimal.mark = ",", big.mark="."),
                       hovertemplate = paste("<b>Brazil</b><br>%{text} casos<br>em ",
                                             format(as.Date(brazil$date[1:nrow(brazil)]), "%d-%b-%Y"),
                                             "<extra></extra>", sep = ""))
        # 1# is visible / 2:9 is legend only
        for (i in 1:1) {
                n = length(filter(world_data_plot, world_data_plot$location == country[i])$serie)
                
                fig <- fig %>% add_trace(x = 1:n,
                                         y = filter(world_data_plot, world_data_plot$location == country[i])$serie,
                                         name = country[i],
                                         mode = 'lines',
                                         visible = TRUE,
                                         text = format(filter(world_data_plot, world_data_plot$location == country[i])$serie,
                                                       decimal.mark = ",", big.mark="."),
                                         hovertemplate = paste("<b>",country[i],"</b><br>%{text} ",text_,"<br>em ",
                                                               format(as.Date(filter(world_data_plot,
                                                                   world_data_plot$location == country[i])$date[1:n]), "%d-%b-%Y"),
                                                               "<extra></extra>", sep = "")
                )
        }
        for (i in 2:9) {
                n = length(filter(world_data_plot, world_data_plot$location == country[i])$serie)
                
                fig <- fig %>% add_trace(x = 1:n,
                                         y = filter(world_data_plot, world_data_plot$location == country[i])$serie,
                                         name = country[i],
                                         mode = 'lines',
                                         visible = "legendonly",
                                         text = format(filter(world_data_plot, world_data_plot$location == country[i])$serie, decimal.mark = ",", big.mark="."),
                                         hovertemplate = paste("<b>",country[i],"</b><br>%{text} ",text_,"<br>em ",
                                                               format(as.Date(filter(world_data_plot, world_data_plot$location == country[i])$date[1:n]), "%d-%b-%Y"),
                                                               "<extra></extra>", sep = "")
                )
        }
        fig <- fig %>% layout(xaxis = list(title = dia_),
                              yaxis = list(title = title_, type = "log"),
                              #legend = list(y = 0.9),
                              legend=list(y = 1, title=list(text='<b>Países</b>')),
                              updatemenus = list(
                                      list(#type = "buttons",
                                           y = 1,
                                           x = 0.15,
                                           buttons = list(
                                                   list(method = "update",
                                                        args = list(list(visible = c()), list(yaxis = list(type = 'log'))
                                                        ),
                                                        label = "Log"),
                                                   
                                                   list(method = "update",
                                                        args = list(list(visible = c()), list(yaxis = list(type = 'linear'))
                                                        ),
                                                        label = "Linear")
                                           )
                                      )
                              )
        )
        fig
}
#plot_curves_top10_countries(3)
