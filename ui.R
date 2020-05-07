

header = dashboardHeader(
          title = "Covid-19 Dashboard",
          titleWidth = '300px'

)

sidebar = dashboardSidebar(
          width = '300px',
          sidebarMenu(
                    menuItem("Casos em cidades e estados", tabName = "casos", icon = icon("table")),
                    menuItem("Mapa interativo", tabName = "mapa_interativo", icon = icon("map")),
                    menuItem("Sobre o site", tabName = "sobre", icon = icon("exclamation-triangle")),
                    h1(""),
                    infoBox("Atualização", h3(atualizacao_format), icon = icon("clock"), fill = TRUE, width = 12)
          )
)

body = dashboardBody(
          tabItems(
                    tabItem("sobre",
                            box(title = "Contatos", status = "primary", width = 3, height = 200, solidHeader = TRUE,
                                helpText(a("pericles.marques@outlook.com", href="mailto:pericles.marques@outlook.com?Subject=S%E9rie%20B")),
                                helpText(a("linkedin.com/in/periclessavio/", target="_blank", href="https://www.linkedin.com/in/periclessavio/")),
                                helpText(a("twitter.com/Pericles_SGM", target="_blank", href="https://twitter.com/Pericles_SGM"))
                            ),
                            box(title = "Atualização", status = "primary", width = 9, height = 200, solidHeader = TRUE,
                                p("Projeto em desenvolvimento.")
                            )
                    ),
                    
                    tabItem("casos",
                            box(title = paste("Tabela com casos, óbitos, testes e recuperados."), 
                                width = 8,
                                height = "auto",
                                status="primary",
                                solidHeader = TRUE, 
                                DT::dataTableOutput("tabela_casos_mortes")
                            ),
                            
                            fluidRow(
                                      box(title = "Cidades com casos por Covid-19.", 
                                          width = 4,
                                          height = 498,
                                          status="primary",
                                          solidHeader = TRUE, 
                                          plotOutput("mapa_casos")
                                      ),
                                      box(title = "Cidades com óbitos por Covid-19.", 
                                          width = 4,
                                          height = 498,
                                          status="primary",
                                          solidHeader = TRUE, 
                                          plotOutput("mapa_mortes")
                                      )
                            ) 
                    ),
                    
                    tabItem("mapa_interativo",
                            box(title = "Mapa Interativo", width = 10, height = "auto", status = "primary", solidHeader = TRUE, 
                                leafletOutput("mapa_interativo", height = 790)
                            ),
                            box(title = "Visualização", width = 2, height = "400", status = "primary", solidHeader = TRUE, 
                                radioButtons("mapa_interativo_radiobtn", label = NULL, 
                                             c("log10 CASOS" = "casos_log",
                                               "log10 MORTES" = "mortes_log")
                                             )
                            )
                    )
          )
)


ui = dashboardPage(header, sidebar, body)