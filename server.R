server <- function(input, output) {
          
          output$mapa_casos <- renderPlot({ 
                    mapa("casos")
          }, height = 440)
          
          
          
          output$mapa_mortes <- renderPlot({ 
                    mapa("mortes")
          }, height = 440)
          
          
          
          output$mapa_interativo_menu <- renderPlot({
                    opcao <- switch(input$mapa_interativo_radiobtn, "casos_log" = 1, "mortes_log" = 2)
                    print(opcao)
          })
          
          
          
          output$tabela_casos_mortes = DT::renderDataTable({
                    names(DT_cases_brazil_states) <- c("Estados", "Casos", "Óbitos", "Casos/100 mil", "Óbitos/100 mil",
                                                       "Óbitos/Casos", "Testes", "Testes/100 mil", "Recuperados")
                    rownames(DT_cases_brazil_states) <- NULL
                    DT_cases_brazil_states
                    
          }, options = list(pageLength = 28, lengthMenu = c(28)), class = "display nowrap compact", filter = "top", rownames= FALSE)
          
          
          
          output$mapa_interativo <- renderLeaflet({
                    opcao <- switch(input$mapa_interativo_radiobtn,
                                    "casos_log" = 1,
                                    "mortes_log" = 2)
                    mapa_interativo(opcao)})
}