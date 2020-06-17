library(shiny)

shinyServer(function(input, output) {
        
        output$world_covid_df <- DT::renderDataTable({
                df_world_countries()[,c(3,5:16)]
        }, options = list(pageLength = 7, lengthMenu = c(7,14,21,28)), class = "display nowrap compact", filter = "top", rownames= FALSE)
        
        # Brazil
        output$df_brazil_cities <- DT::renderDataTable({
                df_brazil_cities()
        }, options = list(pageLength = 10, lengthMenu = c(10,20,30)), class = "display nowrap compact", filter = "top", rownames= FALSE)
        
        output$df_brazil_states <- DT::renderDataTable({
                df_brazil_states()
        }, options = list(pageLength = 7, lengthMenu = c(7,14,21,28)), class = "display nowrap compact", filter = "top", rownames= FALSE)

        output$brazil_covid_cases <- renderLeaflet(brazil_covid_cities(1))
        output$brazil_covid_deaths <- renderLeaflet(brazil_covid_cities(2))
})