

# df_newcases = read.csv('https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/jhu/new_cases.csv')
# df_newdeaths = read.csv('https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/jhu/new_deaths.csv')
df_totalcases = read.csv('https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/jhu/total_cases.csv')
df_totaldeaths = read.csv('https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/jhu/total_deaths.csv')

plot_curves_top10_countries <- function(N) {
        # # Excluding Brazil / Selecting the top9 countries in cases
        # country <- unique(filter(df_world_countries(), df_world_countries()$location != "Brazil")$location, incomparables = FALSE)[1:9]
        # pandemic_date <- NA
        # selected_countries <- filter(world_covid, world_covid$location %in% country)
        
        # Switch
        switch(N,
               {
                       text_ <- "casos" ;
                       title_ <- 'Casos diários por COVID-19' ; dia_ <- "Data";
                       x_ = df_newcases$date; y_ = df_newcases$Brazil;
                       df = df_newcases
               },
               {
                       text_ <- "óbitos" ;
                       title_ <- 'Óbitos diários por COVID-19' ; dia_ <- "Data";
                       x_ = df_newdeaths$date; y_ = df_newdeaths$Brazil;
                       df = df_newdeaths
                       
               },
               {
                       text_ <- "casos" ;
                       title_ <- 'Casos diários por COVID-19' ; dia_ <- "Data";
                       x_ = df_totalcases$date; y_ = df_totalcases$Brazil;
                       df = df_totalcases
               },
               {
                       text_ <- "óbitos" ;
                       title_ <- 'Óbitos diários por COVID-19' ; dia_ <- "Data";
                       x_ = df_totaldeaths$date; y_ = df_totaldeaths$Brazil;
                       df = df_totaldeaths
               }
        )
        
        # Generating a list/dataframe with first day since confirmed cases first reached 30 per day
        # for (i in 1:9) {
        #         pandemic_date[i] <- filter(selected_countries,
        #                                    selected_countries$location == country[i] & selected_countries$serie > serie_N)[1,4]
        # }
        # country_list <- cbind(country, pandemic_date)
        # country_list <- as.data.frame(country_list)
        # country_list$pandemic_date <- as.Date(country_list$pandemic_date)
        # 
        # # Generating a dataframe with necessary data to plot
        # world_data_plot <- as.data.frame(matrix(nrow = 0, ncol = 0))
        # for (i in 1:9) {
        #         world_data_plot <- rbind(world_data_plot, filter(selected_countries,
        #                         selected_countries$location == country_list[i,1] & selected_countries$date >= country_list[i,2]))
        # }
        # 
        # # Removing negative and zero value
        # world_data_plot <- filter(world_data_plot, world_data_plot$serie >0)
        
        
        
        # Plot
        fig <- plot_ly(x = x_, y = y_, name = "Brazil", type = 'scatter', mode = 'lines', height=620,
                       text = format(y_, decimal.mark = ",", big.mark="."),
                       hovertemplate = paste("<b>Brazil</b><br>%{text} casos<br>em ",
                                             format(as.Date(max(x_)), "%d-%b-%Y"),
                                             "<extra></extra>", sep = ""))
        
        fig <- fig %>% add_trace(x = x_,
                                 y = df$Argentina,
                                 name = 'Argentina',
                                 mode = 'lines',
                                 visible = TRUE,
                                 #text = format(filter(world_data_plot, world_data_plot$location == country[i])$serie,
                                 #              decimal.mark = ",", big.mark="."),
                                 hovertemplate = paste("<b>",'Argentina',"</b><br>%{y} ",text_,"<br>em ",
                                                       format(as.Date(max(x_)), "%d-%b-%Y"),
                                                       "<extra></extra>", sep = "")
        )
        
        fig <- fig %>% add_trace(x = x_,
                                 y = df$Peru,
                                 name = 'Peru',
                                 mode = 'lines',
                                 visible = TRUE,
                                 #text = format(filter(world_data_plot, world_data_plot$location == country[i])$serie,
                                 #              decimal.mark = ",", big.mark="."),
                                 hovertemplate = paste("<b>",'Peru',"</b><br>%{y} ",text_,"<br>em ",
                                                       format(as.Date(max(x_)), "%d-%b-%Y"),
                                                       "<extra></extra>", sep = "")
        )
        
        fig <- fig %>% add_trace(x = x_,
                                 y = df$'United States',
                                 name = 'Estados Unidos',
                                 mode = 'lines',
                                 visible = TRUE,
                                 #text = format(filter(world_data_plot, world_data_plot$location == country[i])$serie,
                                 #              decimal.mark = ",", big.mark="."),
                                 hovertemplate = paste("<b>",'Estados Unidos',"</b><br>%{y} ",text_,"<br>em ",
                                                       format(as.Date(max(x_)), "%d-%b-%Y"),
                                                       "<extra></extra>", sep = "")
        )
        
        fig <- fig %>% add_trace(x = x_,
                                 y = df$'United Kingdom',
                                 name = 'Reino Unido',
                                 mode = 'lines',
                                 visible = TRUE,
                                 #text = format(filter(world_data_plot, world_data_plot$location == country[i])$serie,
                                 #              decimal.mark = ",", big.mark="."),
                                 hovertemplate = paste("<b>",'Reino Unido',"</b><br>%{y} ",text_,"<br>em ",
                                                       format(as.Date(max(x_)), "%d-%b-%Y"),
                                                       "<extra></extra>", sep = "")
        )
        
        fig <- fig %>% add_trace(x = x_,
                                 y = df$India,
                                 name = 'India',
                                 mode = 'lines',
                                 visible = TRUE,
                                 #text = format(filter(world_data_plot, world_data_plot$location == country[i])$serie,
                                 #              decimal.mark = ",", big.mark="."),
                                 hovertemplate = paste("<b>",'India',"</b><br>%{y} ",text_,"<br>em ",
                                                       format(as.Date(max(x_)), "%d-%b-%Y"),
                                                       "<extra></extra>", sep = "")
        )

                
        fig <- fig %>% add_trace(x = x_,
                                 y = df$Russia,
                                 name = 'Russia',
                                 mode = 'lines',
                                 visible = TRUE,
                                 #text = format(filter(world_data_plot, world_data_plot$location == country[i])$serie,
                                 #              decimal.mark = ",", big.mark="."),
                                 hovertemplate = paste("<b>",'Russia',"</b><br>%{y} ",text_,"<br>em ",
                                                       format(as.Date(max(x_)), "%d-%b-%Y"),
                                                       "<extra></extra>", sep = "")
        )
        
        fig <- fig %>% add_trace(x = x_,
                                 y = df$Mexico,
                                 name = 'México',
                                 mode = 'lines',
                                 visible = TRUE,
                                 #text = format(filter(world_data_plot, world_data_plot$location == country[i])$serie,
                                 #              decimal.mark = ",", big.mark="."),
                                 hovertemplate = paste("<b>",'Mexico',"</b><br>%{y} ",text_,"<br>em ",
                                                       format(as.Date(max(x_)), "%d-%b-%Y"),
                                                       "<extra></extra>", sep = "")
        )
        
        fig <- fig %>% add_trace(x = x_,
                                 y = df$Italy,
                                 name = 'Itália',
                                 mode = 'lines',
                                 visible = TRUE,
                                 #text = format(filter(world_data_plot, world_data_plot$location == country[i])$serie,
                                 #              decimal.mark = ",", big.mark="."),
                                 hovertemplate = paste("<b>",'Itália',"</b><br>%{y} ",text_,"<br>em ",
                                                       format(as.Date(max(x_)), "%d-%b-%Y"),
                                                       "<extra></extra>", sep = "")
        )
        
        fig <- fig %>% add_trace(x = x_,
                                 y = df$France,
                                 name = 'França',
                                 mode = 'lines',
                                 visible = TRUE,
                                 #text = format(filter(world_data_plot, world_data_plot$location == country[i])$serie,
                                 #              decimal.mark = ",", big.mark="."),
                                 hovertemplate = paste("<b>",'França',"</b><br>%{y} ",text_,"<br>em ",
                                                       format(as.Date(max(x_)), "%d-%b-%Y"),
                                                       "<extra></extra>", sep = "")
        )
        
        fig <- fig %>% add_trace(x = x_,
                                 y = df$Colômbia,
                                 name = 'Colômbia',
                                 mode = 'lines',
                                 visible = TRUE,
                                 #text = format(filter(world_data_plot, world_data_plot$location == country[i])$serie,
                                 #              decimal.mark = ",", big.mark="."),
                                 hovertemplate = paste("<b>",'Colômbia',"</b><br>%{y} ",text_,"<br>em ",
                                                       format(as.Date(max(x_)), "%d-%b-%Y"),
                                                       "<extra></extra>", sep = "")
        )
        
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