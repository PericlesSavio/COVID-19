# libraries
library("shiny")
library("shinydashboard") #advanced dashboard
library("dplyr") #to manipulate dataframes
library("DT") #thousand separator
library("plotly") #plotly graphs
library("leaflet")

# R files
#source("data.R", encoding = "UTF-8")
source("df_world.R", encoding = "UTF-8")
source("df_brazil.R", encoding = "UTF-8")

source("plot_curves_world.R", encoding = "UTF-8") # countries log curve and world dataframe
source("plot_vbar_newcases_newdeaths_brazil.R", encoding = "UTF-8")
source("plot_hbar_cases_deaths_states.R", encoding = "UTF-8")
source("plot_map_world.R", encoding = "UTF-8")
source("plot_map_brazil.R", encoding = "UTF-8")
source("plot_curves_brazil_states.R", encoding = "UTF-8")


header = dashboardHeader(
        title = "Covid-19 Dashboard",
        titleWidth = '300px'
)

sidebar = dashboardSidebar(width = '300px', disable = TRUE, sidebarMenu(menuItem("Menu", tabName = "nacional")))

body = dashboardBody(
        tags$head(
                tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
        ),
        
        fluidRow(
                box(title = "Brasil", width = 9, height = "auto", status="primary", solidHeader = TRUE,
                    box(align="center", width = 2, background = "red",    h4("Casos"),                h2(df_brazil_states()[1,2]), h4(df_brazil_states()[1,7],"casos por 100 mil hab.")),
                    box(align="center", width = 2, background = "black",  h4("Óbitos"),               h2(df_brazil_states()[1,3]), h4(df_brazil_states()[1,8],"óbitos por 100 mil hab.")),
                    box(align="center", width = 2, background = "green",  h4("Recuperados"),          h2(df_brazil_states()[1,5]), h4(df_brazil_states()[1,9],"óbitos por 100 mil hab.")),
                    box(align="center", width = 2, background = "olive",  h4("Testes"),               h2(df_brazil_states()[1,6]), h4(df_brazil_states()[1,8],"óbitos por 100 mil hab.")),
                    box(align="center", width = 2, background = "purple", h4("Municípios com casos"), h2(number_cities_cases), h4(cities_percentage, " dos municípios")),
                    box(align="center", width = 2, background = "maroon", h4("Letalidade"),           h2(df_brazil_states()[1,4]), h4("(* nº de óbitos/nº de casos)"))
                ),
                
                box(title = "Mundo", width = 3, height = "auto", status="primary", solidHeader = TRUE,
                    box(align="center", width = 6, background = "red",    h4("Casos"),       h2(df_world_stats$cases)),
                    box(align="center", width = 6, background = "black",  h4("Óbitos"),      h2(df_world_stats$deaths))
                ),
                
                
                tabBox(id = "World_Info", width = 12, height = "auto",
                       
                       tabPanel("Mundo",
                                
                                fluidRow(
                                        box(title = "Dados por país", 
                                            width = 12,
                                            height = "auto",
                                            status="primary",
                                            solidHeader = TRUE, 
                                            DT::dataTableOutput("world_covid_df")
                                        ),
                                        
                                        box(width = 6, height = "700", status="primary", solidHeader = TRUE,
                                            title = "Casos notificados por data de notificação (depois do 30º caso diário)",
                                            plot_curves_top10_countries(1)
                                        ),
                                        box(width = 6, height = "700", status="primary", solidHeader = TRUE,
                                            title = "Óbitos notificados por data de notificação (depois do 5º óbito diário)",
                                            plot_curves_top10_countries(2)
                                        ),
                                        box(width = 6, height = "700", status="primary", solidHeader = TRUE,
                                            title = "Casos acumuladoso por dia (depois do 100º caso)",
                                            plot_curves_top10_countries(3)
                                        ),
                                        box(width = 6, height = "700", status="primary", solidHeader = TRUE,
                                            title = "Óbitos acumulados por dia (depois do 5º óbito)",
                                            plot_curves_top10_countries(4)
                                        ),
                                        box(title = "Casos (Total)", width = 6, status="primary", solidHeader = TRUE, 
                                            world_map(1)
                                        ),
                                        box(title = "Óbitos (Total)", width = 6, status="primary", solidHeader = TRUE, 
                                            world_map(2)
                                        ),
                                        box(title = "Casos (Último dia)", width = 6, status="primary", solidHeader = TRUE, 
                                            world_map(3)
                                        ),
                                        box(title = "Óbitos (Último dia)", width = 6, status="primary", solidHeader = TRUE, 
                                            world_map(4)
                                        ),
                                        box(title = "Casos por milhão", width = 6, status="primary", solidHeader = TRUE, 
                                            world_map(5)
                                        ),
                                        box(title = "Óbitos por milhão", width = 6, status="primary", solidHeader = TRUE, 
                                            world_map(6)
                                        ),
                                        box(title = "Novos casos por milhão", width = 6, status="primary", solidHeader = TRUE, 
                                            world_map(7)
                                        ),
                                        box(title = "Novos óbitos por milhão", width = 6, status="primary", solidHeader = TRUE, 
                                            world_map(8)
                                        )
                                ),
                                
                       ),
                       
                       tabPanel("Brasil",
                                fluidRow(
                                        box(width = 6, height = "480", status="primary", solidHeader = TRUE,
                                            title = "Casos notificados por data de notificação",
                                            plot_vbar_newcases_newdeaths_brazil(1)
                                        ),
                                        
                                        box(width = 6, height = "480", status="primary", solidHeader = TRUE,
                                            title = "Óbitos notificados por data de notificação",
                                            plot_vbar_newcases_newdeaths_brazil(2)
                                        ),
                                        
                                        box(width = 6, height = "480", status="primary", solidHeader = TRUE,
                                            title = "Casos notificados por semana epidemiológica",
                                            plot_vbar_newcases_newdeaths_brazil(3)
                                        ),
                                        
                                        box(width = 6, height = "480", status="primary", solidHeader = TRUE,
                                            title = "Óbitos notificados por semana epidemiológica",
                                            plot_vbar_newcases_newdeaths_brazil(4)
                                        ),
                                        
                                )
                       ),
                       
                       tabPanel("Estados",
                                fluidRow(
                                        box(title = "Dados por Estado", 
                                            width = 12,
                                            height = "auto",
                                            status="primary",
                                            solidHeader = TRUE, 
                                            DT::dataTableOutput("df_brazil_states")
                                        ),
                                        
                                        box(title = "Casos por Estados", 
                                            width = 6,
                                            height = "680",
                                            status="primary",
                                            solidHeader = TRUE, 
                                            brazil_state_curves(1)
                                        ),
                                        
                                        box(title = "Óbitos por Estados", 
                                            width = 6,
                                            height = "680",
                                            status="primary",
                                            solidHeader = TRUE, 
                                            brazil_state_curves(2)
                                        ),
                                        
                                        ### Horizontal bar charts
                                        box(width = 3, height = "680", status="primary", solidHeader = TRUE,
                                            title = "Casos por estado",
                                            plot_hbar_cases_deaths_states(1)
                                        ),
                                        
                                        box(width = 3, height = "680", status="primary", solidHeader = TRUE,
                                            title = "Óbitos por estado",
                                            plot_hbar_cases_deaths_states(2)
                                        ),
                                        
                                        box(width = 3, height = "680", status="primary", solidHeader = TRUE,
                                            title = "Casos por 100 mil hab. por estado",
                                            plot_hbar_cases_deaths_states(3)
                                        ),
                                        
                                        box(width = 3, height = "680", status="primary", solidHeader = TRUE,
                                            title = "Óbitos por 100 mil hab. por estado",
                                            plot_hbar_cases_deaths_states(4)
                                        ),
                                        
                                        box(width = 3, height = "680", status="primary", solidHeader = TRUE,
                                            title = "Letalidade por estado",
                                            plot_hbar_cases_deaths_states(5)
                                        ),
                                        
                                        box(width = 3, height = "680", status="primary", solidHeader = TRUE,
                                            title = "Testes por 100 mil hab. por estado",
                                            plot_hbar_cases_deaths_states(6)
                                        ),
                                        
                                )
                       ),
                       
                       tabPanel("Cidades",
                                fluidRow(
                                        box(title = "Dados por cidade",
                                            width = 12,
                                            height = "auto",
                                            status="primary",
                                            solidHeader = TRUE, 
                                            DT::dataTableOutput("df_brazil_cities")
                                        ),
                                        
                                        box(title = "Cidades com casos confirmados", width = 6, status="primary", solidHeader = TRUE,
                                            leafletOutput("brazil_covid_cases", height = 600)
                                        ),
                                        
                                        box(title = "Cidades com óbitos confirmados", width = 6, status="primary", solidHeader = TRUE,
                                            leafletOutput("brazil_covid_deaths", height = 600)
                                        )
                                )
                       ),
                       
                       tabPanel("Sobre",
                                fluidRow(
                                        box(title = "Sobre o dashboard", width = 12, height = "auto", status="primary", solidHeader = TRUE,
                                            p("Meu nome é Péricles, sou um estudante de Data Science e criei meu este projeto para treinar minhas habilidades em R (linguagem de programação voltada à manipulação, análise e visualização de dados) e Shiny (pacote R que facilita a criação de aplicativos web interativos)."),
                                            hr(),
                                            h4("Contato:"),
                                            helpText(a("pericles.marques@outlook.com", href="mailto:pericles.marques@outlook.com?Subject=S%E9rie%20B")),
                                            helpText(a("linkedin.com/in/periclessavio/", target="_blank", href="https://www.linkedin.com/in/periclessavio/")),
                                            helpText(a("twitter.com/Pericles_SGM", target="_blank", href="https://twitter.com/Pericles_SGM"))
                                            
                                        )
                                )
                       )
                       
                       
                )
        )
)

ui = dashboardPage(header, sidebar, body)