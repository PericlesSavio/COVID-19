dia_hj = as.Date(max(world_covid_data$last_updated_date))

world_map <- function(N) {
        
        # world_covid_data <- df_world_countries()
        # world_covid_data$date <- as.Date(world_covid_data$date)
        last_date <- as.Date(max(world_covid_data$last_updated_date))
        
        switch (N, #1 = total_cases / 2 = total_deaths / 3 = new_cases / 4 = new_deaths
                {#1
                        z_max <- sort(world_covid_data$total_cases, decreasing = T)[6];
                        legend_text = "Casos (Total)";
                        data = world_covid_data$total_cases;
                        hover_info = paste("<b>",world_covid_data$location,"</b><br><br>",
                                           "Casos:<br>", format(world_covid_data$total_cases, decimal.mark = ",", big.mark="."), "<br>",
                                           "(", format(dia_hj, "%d de %B de %Y"), ")", sep = ""
                        )
                },
                {#2
                        z_max <- sort(world_covid_data$total_deaths, decreasing = T)[7];
                        legend_text = "Óbitos (Total)";
                        data = world_covid_data$total_deaths;
                        hover_info = paste("<b>",world_covid_data$location,"</b><br><br>",
                                           "Óbitos:<br>", format(world_covid_data$total_deaths, decimal.mark = ",", big.mark="."), "<br>",
                                           "(", format(dia_hj, "%d de %B de %Y"), ")", sep = ""
                        )
                },
                {#3
                        z_max <- sort(world_covid_data$new_cases, decreasing = T)[1];
                        legend_text = paste("Casos", format(last_date, "(%d de %B)"));
                        data = world_covid_data$new_cases;
                        hover_info = paste("<b>",world_covid_data$location,"</b><br><br>",
                                           "Casos no último dia:<br>", format(world_covid_data$new_cases, decimal.mark = ",", big.mark="."), "<br>",
                                           "(", format(dia_hj, "%d de %B de %Y"), ")", sep = ""
                        )
                },
                {#4
                        z_max <- sort(world_covid_data$new_deaths, decreasing = T)[1];
                        legend_text = paste("Óbitos", format(last_date, "(%d de %B)"));
                        data = world_covid_data$new_deaths;
                        hover_info = paste("<b>",world_covid_data$location,"</b><br><br>",
                                           "Óbitos no último dia:<br>", format(world_covid_data$new_deaths, decimal.mark = ",", big.mark="."), "<br>",
                                           "(", format(dia_hj, "%d de %B de %Y"), ")", sep = ""
                        )
                },
                {#5
                        z_max <- sort(world_covid_data$total_cases_per_million, decreasing = T)[6];
                        legend_text = "Casos por milhão";
                        data = world_covid_data$total_cases_per_million;
                        hover_info = paste("<b>",world_covid_data$location,"</b><br><br>",
                                           "Casos por milhão:<br>", format(round(world_covid_data$total_cases_per_million, digits = 2), decimal.mark = ",", big.mark=".", digits = 5, nsmall = 2), "<br>",
                                           "(", format(dia_hj, "%d de %B de %Y"), ")", sep = ""
                        )
                },
                {#6
                        z_max <- sort(world_covid_data$total_deaths_per_million, decreasing = T)[3];
                        legend_text = "Óbitos  por milhão";
                        data = world_covid_data$total_deaths_per_million;
                        hover_info = paste("<b>",world_covid_data$location,"</b><br><br>",
                                           "Óbitos por milhão:<br>", format((world_covid_data$total_deaths_per_million), decimal.mark = ",", big.mark=".", digits = 0, nsmall = 2), "<br>",
                                           "(", format(dia_hj, "%d de %B de %Y"), ")", sep = ""
                        )
                },
                {#7
                        z_max <- sort(world_covid_data$new_cases_per_million, decreasing = T)[3];
                        legend_text = "Novos casos por milhão";
                        data = world_covid_data$new_cases_per_million;
                        hover_info = paste("<b>",world_covid_data$location,"</b><br><br>",
                                           "Novos casos por milhão:<br>", format((data), decimal.mark = ",", big.mark=".", digits = 0, nsmall = 2), "<br>",
                                           "(", format(dia_hj, "%d de %B de %Y"), ")", sep = ""
                        )
                },
                {#8
                        z_max <- sort(world_covid_data$new_deaths_per_million, decreasing = T)[1];
                        legend_text = "Novos óbitos  por milhão";
                        data = world_covid_data$new_deaths_per_million;
                        hover_info = paste("<b>",world_covid_data$location,"</b><br><br>",
                                           "Novos óbitos por milhão:<br>", format((data), decimal.mark = ",", big.mark=".", digits = 0, nsmall = 2), "<br>",
                                           "(", format(dia_hj, "%d de %B de %Y"), ")", sep = ""
                        )
                }
        )
        
        fig <- plot_ly(world_covid_data, type='choropleth', locations=world_covid_data$iso_code, z=data, colorscale="Reds",
                       name = "<extra></extra>", zmin=0, zmax=z_max, hovertemplate = hover_info
        )
        fig <- fig %>% colorbar(title = legend_text, x = 1, y = 0.97)
        fig <- fig %>% layout(geo = list(showframe = FALSE))
        fig
        
}
#world_map(5)
