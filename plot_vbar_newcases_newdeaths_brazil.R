plot_vbar_newcases_newdeaths_brazil <- function(N) {
        
        data <- brazil_states_time
        data$date <- as.Date(data$date)
        data$epiweek <- strftime(data$date, format = "%V")
        
        data_total <- filter(data, data$state == "TOTAL")
        data_epiweek <- aggregate(list(data_total$newCases, data_total$newDeaths), by = data.frame(data_total$epiweek), FUN = sum)
        names(data_epiweek) <- c("week", "newCases", "newDeaths")
        
        switch (N,
                {x_ <- data_total$date; y_ <- data_total$newCases; X_axis = "Data da notificação"; Y_axis = "Casos notificados"; color_ = "rgb(240, 150, 0)"},
                {x_ <- data_total$date; y_ <- data_total$newDeaths; X_axis = "Data da notificação"; Y_axis = "Óbitos notificados"; color_ = "rgb(180, 0, 0)"},
                {x_ <- data_epiweek$week; y_ <- data_epiweek$newCases; X_axis = "Semana epidemiológica"; Y_axis = "Casos notificados"; color_ = "rgb(240, 150, 0)"},
                {x_ <- data_epiweek$week; y_ <- data_epiweek$newDeaths; X_axis = "Semana epidemiológica"; Y_axis = "Óbitos notificados"; color_ = "rgb(180, 0, 0)"}
        )
        
        #
        updatemenus = list(
                list(type = "buttons",
                     y = 0.6,
                     buttons = list(
                             list(method = "update",
                                  args = list(list(x = data_total$newDeaths),
                                              list(y = data_total$date),
                                              list(title = "Casos"),
                                              list(xaxis = list(title = "X_axis")),
                                              list(marker = list(color = "rgb(0, 150, 0)"))
                                  ),
                                  colorbar = list(title = "Typing Ratrfrrre"),
                                  label = "Casos/Dia"),
                             
                             list(method = "update",
                                  args = list(list(x = list(data_total$newCases, data_total$newDeaths)),
                                              list(x = list(data_total$date, data_epiweek$week)),
                                              list(title = "Casdffdfdos"),
                                              list(colorscale = list("Reds", "Blues")),
                                              list(zmin = list(10, 10000)),
                                              list(colorbar = list(title = "Typing Ratrfrrre"))
                                              
                                  ),
                                  colorbar = list(title = "Typinsdfg Ratrfrrre"),
                                  label = "Óbitos")
                     )
                )
        )
        #
        
        fig <- plot_ly(
                x = x_,
                y = y_,
                name = "SF Zoo",
                type = "bar",
                height=420,
                marker = list(color = color_)
        )
        
        fig <- fig %>% layout(xaxis = list(title = X_axis),
                              yaxis = list(title = Y_axis)
                              #updatemenus = updatemenus
        )
        fig
}