plot_hbar_cases_deaths_states <- function (n) {
        
        df <- arrange(brazil_states[2:28,], desc(totalCasesMS))
        df <- merge(df, brazil_pop, by.x = "state", by.y = "Sigla", all = FALSE)
        
        switch(n,
               {x1 = df$totalCasesMS ; xaxis_ = "Nº de casos" ; x2 = format(x1, decimal.mark = ",", big.mark=".") ; x2_text = ' casos' ; color1 = 'orange'},
               {x1 = df$deathsMS ; xaxis_ = "Nº de óbitos" ; x2 = format(x1, decimal.mark = ",", big.mark=".") ; x2_text = ' óbitos' ; color1 = '#990000'},
               {x1 = df$totalCases_per_100k_inhabitants ; xaxis_ = "Casos por 100 mil hab." ; x2 = format(x1, decimal.mark = ",", big.mark=".", digits = 2, nsmall = 2) ; x2_text = ' casos por 100 mil hab.' ; color1 = 'orange'},
               {x1 = df$deaths_per_100k_inhabitants ; xaxis_ = "Óbitos por 100 mil hab." ; x2 = format(x1, decimal.mark = ",", big.mark=".", digits = 2, nsmall = 2) ; x2_text = ' óbitos por 100 mil hab.' ; color1 = '#990000'},
               {x1 = df$deaths_by_totalCases*100 ; xaxis_ = "Taxa de letalidade em % (mortes por nº de casos)" ; x2 = format(x1, decimal.mark = ",", big.mark=".", digits = 2, nsmall = 2) ; x2_text = ' % de letalidade' ; color1 = 'purple'},
               {x1 = df$tests_per_100k_inhabitants ; xaxis_ = "Testes por 100 mil hab." ; x2 = format(x1, decimal.mark = ",", big.mark=".", digits = 2, nsmall = 2) ; x2_text = ' testes por 100 mil hab.' ; color1 = 'green'}
        )
        
        fig <- plot_ly(x = x1, y = ~reorder(df$state, df$totalCasesMS), hovertemplate = paste(df$UF,'<br>',x2, x2_text, sep =""), name = "", marker = list(color=color1), type = 'bar', orientation = 'h', height = 600,

        )
        
        fig <- fig %>% layout(
                xaxis = list(title = xaxis_),
                yaxis = list(title = 'Estados')
                )
        
        fig
}