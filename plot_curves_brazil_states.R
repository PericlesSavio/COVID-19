brazil_state_curves <- function (N) {
        
        #library(plotly)
        #library(dplyr)
        
        data <- brazil_states_time
        data$date <- as.Date(data$date)
        
        switch(N,
               {data$serie <- data$totalCasesMS ; eixo_X = 'Dia a partir do 1º caso'; eixo_Y <- 'Casos Acumulados'},
               {
                       {data$serie <- data$deathsMS ; eixo_X = 'Dia a partir do 1º óbito'; eixo_Y <- 'Óbitos Acumulados'}
                       {data <- filter(data, data$deaths >0)}
               },
               {data$serie <- data$newCases ; eixo_X = 'Dia a partir do 1º caso'; eixo_Y <- 'Casos Diários'},
               {
                       {data$serie <- data$newDeaths ; eixo_X = 'Dia a partir do 1º óbito'; eixo_Y <- 'Óbitos Diários'}
                       {data <- filter(data, data$deaths >0)}
               }
               
        )
        
        #data$serie <- data$totalCases; eixo_X = 'Dia a partir do 1º caso'; eixo_Y <- 'Casos Acumulados'
        siglas <- read.csv("./sigla_estado.csv", sep = ",", header = TRUE, encoding = "UTF-8", stringsAsFactors=FALSE)
        
        for (i in 1:28) {
                assign(paste("data", siglas$Sigla[i], sep = ""), filter(data, data$state == siglas$Sigla[i]))
        }
        
        fig <- plot_ly(x = 1:nrow(dataTOTAL), y= ~dataTOTAL$serie, type = 'scatter', mode = 'lines', line=list(color='rgb(0,0,0)'), name = 'Brasil', height=620, visible = TRUE)
        
        # this code doesn't work
        # for (i in 2:2) {
        #         fig <- fig %>% add_trace(x = 1:length(filter(data, data$state == siglas$Sigla[i])$serie), y = ~filter(data, data$state == siglas$Sigla[i])$serie, mode = 'lines', visible = "legendonly", name = siglas$Estado_Min[i])
        # }
        
        # new code instead of the last code
        fig <- fig %>% add_trace(x = 1:nrow(dataAC), y = ~dataAC$serie, mode = 'lines', visible = "legendonly", line=list(color='rgb(0,255,0)'), name = 'Acre')
        fig <- fig %>% add_trace(x = 1:nrow(dataAP), y = ~dataAP$serie, mode = 'lines', visible = "legendonly", line=list(color='rgb(0,205,0)'), name = 'Amapá')
        fig <- fig %>% add_trace(x = 1:nrow(dataAM), y = ~dataAM$serie, mode = 'lines', visible = "legendonly", line=list(color='rgb(0,145,0)'), name = 'Amazonas')
        fig <- fig %>% add_trace(x = 1:nrow(dataPA), y = ~dataPA$serie, mode = 'lines', visible = "legendonly", line=list(color='rgb(0,95,0)'), name = 'Pará')
        fig <- fig %>% add_trace(x = 1:nrow(dataRO), y = ~dataRO$serie, mode = 'lines', visible = "legendonly", line=list(color='rgb(150,255,0)'), name = 'Rondônia')
        fig <- fig %>% add_trace(x = 1:nrow(dataRR), y = ~dataRR$serie, mode = 'lines', visible = "legendonly", line=list(color='rgb(150,185,0)'), name = 'Roraima')
        fig <- fig %>% add_trace(x = 1:nrow(dataTO), y = ~dataTO$serie, mode = 'lines', visible = "legendonly", line=list(color='rgb(150,95,0)'), name = 'Tocantins')
        
        fig <- fig %>% add_trace(x = 1:nrow(dataAL), y = ~dataAL$serie, mode = 'lines', visible = "legendonly", line=list(color='rgb(0,0,255)'), name = 'Alagoas')
        fig <- fig %>% add_trace(x = 1:nrow(dataBA), y = ~dataBA$serie, mode = 'lines', visible = "legendonly", line=list(color='rgb(0,0,185)'), name = 'Bahia')
        fig <- fig %>% add_trace(x = 1:nrow(dataCE), y = ~dataCE$serie, mode = 'lines', visible = "legendonly", line=list(color='rgb(0,0,95)'), name = 'Ceará')
        fig <- fig %>% add_trace(x = 1:nrow(dataMA), y = ~dataMA$serie, mode = 'lines', visible = "legendonly", line=list(color='rgb(128,229,255)'), name = 'Maranhão')
        fig <- fig %>% add_trace(x = 1:nrow(dataPB), y = ~dataPB$serie, mode = 'lines', visible = "legendonly", line=list(color='rgb(0,204,255)'), name = 'Paraíba')
        fig <- fig %>% add_trace(x = 1:nrow(dataPE), y = ~dataPE$serie, mode = 'lines', visible = "legendonly", line=list(color='rgb(0, 163, 204)'), name = 'Pernambuco')
        fig <- fig %>% add_trace(x = 1:nrow(dataPI), y = ~dataPI$serie, mode = 'lines', visible = "legendonly", line=list(color='rgb(153, 204, 255)'), name = 'Piauí')
        fig <- fig %>% add_trace(x = 1:nrow(dataRN), y = ~dataRN$serie, mode = 'lines', visible = "legendonly", line=list(color='rgb(51, 153, 255)'), name = 'Rio Grande do Norte')
        fig <- fig %>% add_trace(x = 1:nrow(dataSE), y = ~dataSE$serie, mode = 'lines', visible = "legendonly", line=list(color='rgb(0, 89, 179)'), name = 'Sergipe')
        
        fig <- fig %>% add_trace(x = 1:nrow(dataES), y = ~dataES$serie, mode = 'lines', visible = "legendonly", line=list(color='rgb(204, 204, 204)'), name = 'Espírito Santo')
        fig <- fig %>% add_trace(x = 1:nrow(dataMG), y = ~dataMG$serie, mode = 'lines', visible = "legendonly", line=list(color='rgb(115, 115, 115)'), name = 'Minas Gerais')
        fig <- fig %>% add_trace(x = 1:nrow(dataRJ), y = ~dataRJ$serie, mode = 'lines', visible = "legendonly", line=list(color='rgb(230, 230, 0)'), name = 'Rio de Janeiro')
        fig <- fig %>% add_trace(x = 1:nrow(dataSP), y = ~dataSP$serie, mode = 'lines', visible = "legendonly", line=list(color='rgb(128, 128, 0)'), name = 'São Paulo')
        
        fig <- fig %>% add_trace(x = 1:nrow(dataDF), y = ~dataDF$serie, mode = 'lines', visible = "legendonly", line=list(color='rgb(255, 179, 179)'), name = 'Distrito Federal')
        fig <- fig %>% add_trace(x = 1:nrow(dataGO), y = ~dataGO$serie, mode = 'lines', visible = "legendonly", line=list(color='rgb(255, 102, 102)'), name = 'Goias')
        fig <- fig %>% add_trace(x = 1:nrow(dataMT), y = ~dataMT$serie, mode = 'lines', visible = "legendonly", line=list(color='rgb(255, 0, 0)'), name = 'Mato Grosso')
        fig <- fig %>% add_trace(x = 1:nrow(dataMS), y = ~dataMS$serie, mode = 'lines', visible = "legendonly", line=list(color='rgb(125,0,0)'), name = 'Mato Grosso do Sul')
        
        fig <- fig %>% add_trace(x = 1:nrow(dataPR), y = ~dataPR$serie, mode = 'lines', visible = "legendonly", line=list(color='rgb(255,0,255)'), name = 'Paraná')
        fig <- fig %>% add_trace(x = 1:nrow(dataRS), y = ~dataRS$serie, mode = 'lines', visible = "legendonly", line=list(color='rgb(180,0,180)'), name = 'Rio Grande do Sul')
        fig <- fig %>% add_trace(x = 1:nrow(dataSC), y = ~dataSC$serie, mode = 'lines', visible = "legendonly", line=list(color='rgb(105,0,125)'), name = 'Santa Catarina')
        
        fig <- fig %>% layout(xaxis = list(title = eixo_X),
                              yaxis = list(title = eixo_Y, type = "log"))
        fig
}

