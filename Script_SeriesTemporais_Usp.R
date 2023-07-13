
# Script Usp/Esalq

# Pacotes -----------------------------------------------------------------
pacotes <- c("readxl","plotly","tidyverse","gridExtra","forecast","TTR",
             "smooth","tidyverse", "tsibble", "fable","tsibbledata", "fpp3",
             "urca")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T)
} else {
  sapply(pacotes, require, character = T)
}



# Dados -------------------------------------------------------------------
basepetr4 <- read_excel("raw_datas_exemplos/basepetr4.xlsx")
View(basepetr4)

# Explorar
head(basepetr4)



# Tibble vs tsibble ------------------------------------------------------------------
# 

# Base 1:
# Note que é uma tibble de 812 x 6
basepetr4


# Base 2:
# Note que é um tsibble: logo permite o armazenamento e manipulação de múltiplas
# séries temporais no R.

# Ele contêm:
# - 1 index (informação de tempo, normalmente o período de tempo)
# - Variáveis medidas
# - Variáveis chave --  coluna que contém a variável chave *ou as chaves 
#   (identificadores únicos opcionais para cada série)

# Note que tem mais de uma série temporal. Tem uma série atrás da outra. Uma
# variável para vários países. 
global_economy



# Avisar ao R sobre a Série Temporal -------------------------------------

# Olha o R não entende que temos uma série temporal
petr4 = basepetr4[3]
head(petr4)
plot(petr4)
class(petr4)

# Criar uma tsibble
# Avisar ao R que temos uma série temporal - comando times series (ts)
petr4 = ts(petr4)
class(petr4)

# Quantas cotações tenho?
length(petr4)


# Gráfico Série Temporal --------------------------------------------------
# Agora sim, o gráfico como série de tempo
ggplotly(
  basepetr4 %>%
    mutate(Data = as.Date(Data)) %>%
    ggplot() +
    geom_line(aes(x = Data, y = Fechamento, color = "série")) +
    scale_color_viridis_d() +
    scale_x_date(date_labels = "%m-%Y", date_breaks = "1 month") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.4),
          panel.background = element_rect(fill = "white", color = "black"),
          panel.grid = element_line(color = "grey90"),
          panel.border = element_rect(color = "black", fill = NA),
          legend.position = "none")
)


# Ordenar a série  ---------------------------------------------------------
# Observe que tenho 812 observações; posso informar
petr4 = ts (petr4, start=1, end=812)

# fazer o gráfico
plot(petr4, main="Cotações da PETR4 - jan/2020 a abr/2023",
     xlab="Tempo em dias (02/01/20 a 04/04/23)", ylab="R$/ação" )

head(petr4)

# Multiplas séries ----------------------------------------------
# Essa montagem da matriz permite fazer todas as séries ao mesmo tempo
# empilhadas
# Considerando as demais informações da base de dados basepetr4

volume = ts (basepetr4[2])  # petr4: fechamento
mínimo = ts (basepetr4[5])  #
abertura = ts(basepetr4[4])

# Colocando os dados em forma de uma matriz com todos os dados no conjunto dados1
dados1 = ts(matrix(1,812,4)) 
dados1[,1] = petr4
dados1[,2] = volume
dados1[,3] = mínimo
dados1[,4] = abertura

# Trocando o nome da coluna
colnames(dados1)[1]='Fechamento R$'
colnames(dados1)[2]='Volume Financ. Neg'
colnames(dados1)[3]='Mínimo R$'
colnames(dados1)[4]='Abertura R$'

head(dados1)
# Gráfico Multiplas Séries ------------------------------------------------
plot(dados1, main="Informações da ação PETR4",
     xlab="Tempo em dias (02/01/20 a 04/04/23) -
     Fonte: Google Finance")



# Construindo uma janela com 4 gráficos -----------------------------------
par(mfrow=c(2,2))
plot(petr4, main="Cotação de Fechamento PETR4 - jan/20 a abr/23",
     xlab="tempo em dias - 02/01/20 a 04/04/23", ylab="R$")
#options(scipen = 999)
plot(mínimo, main="Cotação Mínima no dia - PETR4 - jan/20 a abr/23",
     xlab="tempo em dias - 02/01/20 a 04/04/23", ylab="Núm.Negócios")
#options(scipen = 999)
plot(volume, main="Volume Financeiro Negociado PETR4 - jan/20 a abr/23",
     xlab="tempo em dias - 02/01/20 a 04/04/23", ylab="R$")
plot(abertura, main="Cotação de Abertura PETR4 - jan/20 a abr/23",
     xlab="tempo em dias - 02/01/20 a 04/04/23", ylab="R$")

# preciso encerrar o comparticionamento dos gráficos
par(mfrow=c(1,1))




# x -----------------------------------------------------------------------
# Exemplo 2 ---------------------------------------------------------------
# Dados -------------------------------------------------------------------
passageiros <- read_excel("raw_datas_exemplos/passageiros.xlsx")
View(passageiros)
head(passageiros)


# Separar a coluna de interesse -------------------------------------------
# A base de dados tem duas colunas, e os passageiros transportados estao 
# na coluna 2
passag = passageiros[2]
head(passag)


# Definindo a série temporal ----------------------------------------------
# definindo o conjunto de dados passageiros como uma série temporal, pelo
# comando time series (ts). Aqui é como se tivesse em cada observação
# t1, t2, t3, t4 ....tx
passag = ts(passag)



# Gráfico -----------------------------------------------------------------
plot(passag, main="Total de Passageiros no Transporte Aéreo BR",
     xlab="Jan/2011 a Fev/2023", ylab="Total de Passageiros Mensal" )


# Gráfico como Série Temporal ---------------------------------------------
# Agora sim um gráfico de série temporal, como o tempo definido. E não apenas
# t1, t2 ... como no gráfico anterior que perdemos a medida do tempo
ggplotly(
  passageiros %>%
    mutate(Data = as.Date(Data)) %>%
    ggplot() +
    geom_line(aes(x = Data, y = passageiros, color = "Total Passageiros Transportados")) +
    scale_color_viridis_d() +
    scale_x_date(date_labels = "%m-%Y", date_breaks = "3 month") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.4),
          panel.background = element_rect(fill = "white", color = "black"),
          panel.grid = element_line(color = "grey90"),
          panel.border = element_rect(color = "black", fill = NA),
          legend.position = "none")
)



# x -----------------------------------------------------------------------
# Exemplo 3 ---------------------------------------------------------------
# Trabalhando com uma Série Temporal da Receita Trimestral da AMBEV


# Dados -------------------------------------------------------------------
ambev <- read_excel("raw_datas_exemplos/ambev.xlsx")
View(ambev)



# Ajustar dados -----------------------------------------------------------
# Ajustando a base de dados trimestrais acumulados da Receita Líquida 
# Operacional da AMBEV
# frequency = 4 : avisando que são 4 dados por ano
# start = c(2000,1): avisando que começa 2020 no periodo 1
# end = c(2022,4): avisando que termina em 2022 no periodo 4
receita = ts (ambev[2], start = c(2000,1), end = c(2022,4), frequency = 4)
head(receita)


# Gráfico -----------------------------------------------------------------
# options(scipen = 999)
plot(receita, main="Faturamento Trimestral - Acumulado da AMBEV SA - 1T/2000 ao 4T/2022",
     xlab="Trimestres - jan/2000 a dez/2022", ylab="R$ mil")


# x -----------------------------------------------------------------------
# Exemplo 4 ---------------------------------------------------------------
# Trabalhando com a base de dados das Manchas Solares


# Dados -------------------------------------------------------------------
manchas <- read_excel("raw_datas_exemplos/manchas.xlsx")


# Definindo série Temporal ---------------------------------------------
# start=c(1749,1): avisar q começa em 1749 no período 1
# end=c(2023,3): avisar que termina em 2023 no período 3. Termina no periodo 3
#               pq o ano de 2023 não está completo, vai até o mes 3, se tivesse
#               terminaria no 12 assim como os demais anos.
# frequency = 12: se repete 12 vezes - 12 meses
sol = ts(manchas$manchas, start=c(1749,1), end=c(2023,3), frequency = 12)
View(manchas)

plot(sol, main="Número Médio Mensal de Manchas Solares",
     xlab="Mensal - jan/1749 a mar/2023")


# Medidas Descritivas: Séries de Temporais --------------------------------
summary(sol)
sd(sol)
length(sol) # quantidade de dados distribuídos ao longo do período de tempo



# Definir Janela de Tempo -------------------------------------------------
# Definir uma janela de tempo da Série Temporal caso queira observar somente
# um trecho da base de dados
sol1 = window (sol,c(1749,1), c(1990,12))
plot(sol1)


# x -----------------------------------------------------------------------
# Exemplo 5 ---------------------------------------------------------------
# Número Aleatório --------------------------------------------------------
# Trabalhando com uma Série Temporal de Números Aleatórios

# gerando números aleatórios
aleat=ts(rnorm(500))
View(aleat)

plot(aleat, main="Série Temporal de Números Aleatórios com Distribuição Normal Padrão",
     xlab="quantidade de números aleatórios")

# estatistica descritiva
mean(aleat)
sd(aleat)

# gerar uma série de números aleatórios com média 2 e dp = 0.1
aleat1 = ts (rnorm (500, 2,0.1))
plot (aleat1)
mean (aleat1)
sd (aleat1)


# x -----------------------------------------------------------------------
# Exemplo 6 ---------------------------------------------------------------
# Passeio Aleatório -------------------------------------------------------
# Trabalhando com uma Série Temporal de um Passeio Aleatório
# Passeio Aleatório significa poder seguir para esquerda ou para direita com 
# a mesma probabilidade
passeio = cumsum (aleat)
plot (passeio, type='l', main="Passeio Aleatório", xlab = "Núm. Observações")



# x -----------------------------------------------------------------------
# Comparando Série Temporal com Passeio Aleatorio -------------------------
# Comparando a Série PETR4(jan/2020 a abr/2023) com o Passeio Aleatório


# Criar Passeio Aleatório com tamanho da série temp ---------------------
# criando a série petropasseio com mesmo tamanho da petr4, inicialmente todos os
# valores com zero
length(petr4)
petropasseio = ts (0, start=1, end=812) # 812 é o tamanho do petr4

# séries começam no mesmo ponto
petropasseio[1] = petr4[1]

# gerando um passeio aleatório
for(i in 2:812){petropasseio[i] = petropasseio[i-1]+rnorm(1)}


# Gráfico 2 séries --------------------------------------------------------
# plotando no mesmo gráfico as duas séries
plot (petr4,
     main="Cotação Original de Fechamento e Random Walk",
     xlab="tempo em dias - jan/20 a abr/23",
     ylab="R$",
     ylim=c(min(petropasseio,petr4),max(petropasseio,petr4)))

par (new=TRUE)

lines (petropasseio,
       type="l",
       lty=2,
       ylim=c(min(petr4),
              max(petropasseio)))
legend ("bottomright", c('Cotação Original','Random Walk'), lty = 1:2, bty='n')



# x -----------------------------------------------------------------------
# Decomposição Série Temporal ---------------------------------------------

# Decomposição possui três componentes:
# - Tendencia
# - Sazonalidade
# - Aleatório (ciclico)



# Média Móvel -------------------------------------------------------------
# Analisando a tendencia através de médias móveis



# Dados Covid -------------------------------------------------------------
# lendo a base de dados: Fonte: https://covid.saude.gov.br/
covid <- read_excel("raw_datas_exemplos/covid.xlsx")


# Visualizando média móvel ------------------------------------------------
# Série com as médias móveis
ggplotly(
  covid %>%
    mutate(Data = as.Date(Data),
           media_movel = ma(por_dia, order=14)) %>%
    ggplot() +
    geom_line(aes(x = Data, y = por_dia, color = "Por Dia")) +
    geom_line(aes(x = Data, y = media_movel, color = "Média Móvel"), size = 1) +
    labs(color = "Legenda:",
         x = "Data",
         y = "Comportamento da Covid-19") +
    scale_color_viridis_d() +
    scale_x_date(date_labels = "%m-%Y", date_breaks = "1 month") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.4),
          panel.background = element_rect(fill = "white", color = "black"),
          panel.grid = element_line(color = "grey90"),
          panel.border = element_rect(color = "black", fill = NA),
          legend.position = "bottom")
) %>% layout(showlegend = TRUE,
             legend = list(orientation = "h"))

# Podemos notar a presença de outliers. Pois temos dados muito discrepantes, 
# completamente fora da curva


# Boxplot - outlier -------------------------------------------------------
# Gráfico Box-Plot para verificar se temos outliers
covid %>%
  mutate(Data = as.Date(Data),
         media_movel = ma(por_dia, order=14)) %>%
  ggplot() +
  geom_boxplot(aes(x = Data, y = por_dia, color = "Por Dia")) +
  labs(x = "Data",
       y = "Comportamento da Covid-19") +
  scale_color_viridis_d() +
  scale_x_date(date_labels = "%m-%Y", date_breaks = "1 month") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.4),
        panel.background = element_rect(fill = "white", color = "black"),
        panel.grid = element_line(color = "grey90"),
        panel.border = element_rect(color = "black", fill = NA),
        legend.position = "none")


# Limpar Dados ------------------------------------------------------------
# Comando tsclean() para limpar os dados outlier em séries temporais e 
# substituir por um valor interpolado entre o valor anterior e o valor seguinte
covid %>%
  mutate(Data = as.Date(Data),
         covid_suave = tsclean(por_dia),
         media_movel = ma(por_dia, order=14)) %>%
  ggplot() +
  geom_line(aes(x = Data, y = por_dia, color = "Série Original"), size = 1) +
  geom_line(aes(x = Data, y = covid_suave, color = "Série Suavizada")) +
  geom_line(aes(x = Data, y = media_movel, color = "Média Móvel"), size = 1) +
  labs(color = "Legenda:",
       x = "Data",
       y = "Comportamento da Covid-19") +
  scale_color_viridis_d() +
  scale_x_date(date_labels = "%m-%Y", date_breaks = "1 month") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.4),
        panel.background = element_rect(fill = "white", color = "black"),
        panel.grid = element_line(color = "grey90"),
        panel.border = element_rect(color = "black", fill = NA),
        legend.position = "bottom")

#> Note que o gráfico amarelo removeu os outliers (que estavam na série verde, que
#> era a série original). Então, vemos que a série amarela é mais suavizada.


# Quais dados são outliers? ----------------------------------------------------
# Para saber quais pontos foram removidos e quais entraram no lugar
# $index: é o ID da observação, seria a observação 484
# $replacements: ele informa por qual valor trocou a observação indicada
tsoutliers(covid$por_dia)



# Média Móvel não-centralizada --------------------------------------------
# Faz a média dos 7 dias anteriores (ou outro período escolhido). Podemos fazer
# também a média móvel centralizada que faz 3 dias antes e 3 dias depois (ou
# qualquer outro período escolhido)
ggplotly(
  covid %>%
    mutate(Data = as.Date(Data),
           media_movel_nao_centralizada = SMA(por_dia, 14)) %>%
    ggplot() +
    geom_line(aes(x = Data, y = por_dia, color = "Por Dia")) +
    geom_line(aes(x = Data, y = media_movel_nao_centralizada,
                  color = "Média Móvel Não Centralizada"), size = 1) +
    labs(color = "Legenda:",
         x = "Data",
         y = "Comportamento da Covid-19") +
    scale_color_viridis_d() +
    scale_x_date(date_labels = "%m-%Y", date_breaks = "1 month") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.4),
          panel.background = element_rect(fill = "white", color = "black"),
          panel.grid = element_line(color = "grey90"),
          panel.border = element_rect(color = "black", fill = NA),
          legend.position = "bottom")
) %>% layout(showlegend = TRUE,
             legend = list(orientation = "h"))


# Decomposição simples ----------------------------------------------------
# Vamos decompor uma serie temporal em suas componentes:
# Tedência, Sazonal e Erros

# Assumindo inicialmente uma série temporal simples - vide excel
s = ts(c(10,14,8,25,16,22,14,35,15,27,18,40,28,40,25,65), start=c(2019,1),
     end=c(2022,4), frequency = 4)
plot(s)


# Decomposição pelo modelo aditivo ----------------------------------------
# Modelo indicado para quando a série temporal tem efeitos sazonais que 
# permanecem aproximadamente do mesmo tamanho ao longo do tempo

# Decomposição pelo modelo ADITIVO
decompa = decompose (s, type = "additive")
plot(decompa)

decompa$trend
decompa$seasonal
decompa$random

# Decomposição pelo modelo Multiplicativo ----------------------------------------
# Modelo indicado para o tamanho dos efeitos sazonais aumentam com o tempo
decompm = decompose (s, type = "multiplicative")
plot(decompm)

decompm$trend
decompm$seasonal
decompm$random


# Exemplo Aplicação -------------------------------------------------------
# Aplicando a decomposição das séries temporais usando o pacote viridis
# decomposição da série temporal do PIB Mensal BR
pib <- read_excel("raw_datas_exemplos/pib_mensal.xlsx")

# Transformando a base de dados em um objeto de classe ts
pib_ts <- ts(data = pib[, 2],
             start = c(2004, 1),
             end = c(2023, 2),
             frequency = 12)

# Fazendo a plotagem da série temporal
plot(pib_ts, main="Produto Interno Bruto - PIB",
     xlab="jan/2004 a fev/2023",
     ylab="R$ milhões")


# decompondo o PIB pelo modelo aditivo
decpib <- decompose(x = pib_ts,
                    type = "additive")

plot(decpib)


# Transformando o objeto decpib num data frame
decpib_df <- data.frame(tempo = pib$Data,
                        serie = unlist(decpib$x),
                        tendencia = unlist(decpib$trend),
                        sazonalidade = unlist(decpib$seasonal),
                        dessazonalizada = pib_ts - decpib$seasonal,
                        erro = unlist(decpib$random)) %>%
  rename(tempo = 1,
         serie = 2,
         tendencia = 3,
         sazonalidade = 4,
         dessazonalizada = 5,
         erro = 6)

# decompondo o PIB pelo modelo multiplicativo
decpib = decompose(pib_ts, type = "multiplicative")
plot(decpib)



# x -----------------------------------------------------------------------
# Previsões ---------------------------------------------------------------
# Modelagem ---------------------------------------------------------------


# Esse é um método de armotecimento (suavização) ideal para tendências 
# e inclui variação sazonal.

# Podem ser três modelos:
# - Suavização Simples (SES): é ideal para séries que não apresentam tendencia
#                             e sazonalidade.
# - Suavização Exponencial de Holt (SEH): ideal para série COM tendencia mas
#                                         SEM sazonalidade.
# - Suavização Exponencial de Holt-Winters: ideal para séries com tendencia
#                                           e sazonalidade


# Aditivo: para variação sazonal constante
# Multiplicativo: Variação sazonal varia na série


# x -----------------------------------------------------------------------
# x -----------------------------------------------------------------------
# SES - Suavização Exponencial Simples ------------------------------------
# Esse modelo dá peso maior para as observações mais recentes, captando 
# melhor as mudanças de comportamento.
# A previsão é igual ao último valor exponencial suavizado.
# O alfa será escolhido de acordo com o erro. O alfa que dá a menor medida
# de erro será escolhido.Pois me dará a melhor qualidade de previsão



# Dados -------------------------------------------------------------------
base = ts(c(3,5,9,20,12,17,22,23,51,41,56,75,60,75,88))
autoplot(base)


# Modelo de suavização simples --------------------------------------------
# Criando o modelo de suavização exponencial com previsão de 3 passos (h)
# a frente. Previsão para 3 meses (ou outro marcador de tempo).
modeloses = ses(base, h=3)

# valores previstos
# valores previstos com os intervalos de confiança
modeloses

# modelo gerado
# Mostra o alpha ideal para esta previsão (parâmetro de amortização.
# Quanto mais próximo de 1. Mais peso as observações recentes
# possuem para fazer a previsão
modeloses$model

# valores estimados (previstos)
modeloses$fitted

# visualização dos dados e das previões com intervalos de confiança
# intervalo mais claro (maior previsibilidade - é o de 95%). O intervalo
# mais escuro é o de 80%.
autoplot(modeloses)

#> Esse modelo é indicado para uma série mais estável. Esta nossa série
#> parece que está subindo, então não seria muito ideal. Talvez seja melhor 
#> rodar um modelo de Holt, que é um modelo designado para séries com 
#> tendencias, e esta parece ter tendencia a aumentar.
 

# x -----------------------------------------------------------------------
# x -----------------------------------------------------------------------
# Modelo de Holt com Tendência --------------------------------------------

#> Como esse modelo é aplicado para dados com tendencias, ele terá não
#> apenas o alpha mas também o beta para a tendencia. Vamos estimar então
#> dois parametros nesse modelo.

#> Esse modelo traça uma tendencia que vai para infinito. Signfica que
#> esses dados crescem sempre, indefinidamente. Só que isso nem sempre
#> ocorre as vezes cresce até um momento q a tendencia é amortecida. 
#> ou seja, atinge um platô. O modelo de holt acrescentando o amortecimento
#> para atingir o platô é o modelo seguinte. 



# Dados -------------------------------------------------------------------
base=ts(c(10,14,8,25,16,22,14,35,15,27,18,40,28,40,25,65), start = c(2019,1),
        end = c(2022,4), frequency = 4)

# Visualização
autoplot(base)

# Modelo - prever tres passos a frente
modeloholt = holt(base, h=3)


# valores previstos
modeloholt

# modelo gerado
# temos informação sobre o alpha e beta
modeloholt$model

# valores estimados
modeloholt$fitted

# visualização dos dados e das previses com intervalos de confiança
autoplot(modeloholt)


# x -----------------------------------------------------------------------
# x -----------------------------------------------------------------------
# Modelo de Holt com Tendência e Amortecimento (damped) -------------------

#> Esse modelo traça uma tendencia que vai para infinito. Signfica que
#> esses dados crescem sempre, indefinidamente. Só que isso nem sempre
#> ocorre as vezes cresce até um momento q a tendencia é amortecida. 
#> ou seja, atinge um platô. O modelo de holt acrescentando o amortecimento
#> para atingir o platô.

#> Neste caso, temos que estimar além do alpha e beta, o parâmetro phi
#> que é o parâmetro de amortecimento. 

# modelo
modholtamort = holt( base, damped = T, phi = 0.9, h=3)

# valores previstos
modholtamort

# modelo gerado
modholtamort$model

# visualização dos dados e das previ?es com intervalos de confiança
autoplot(modholtamort)
# note que o modelo aparece um pouco mais amortecido, sem explodir com
# valores irreais.



# x -----------------------------------------------------------------------
# x -----------------------------------------------------------------------
# Modelo de Holt com Tendência e Sazonalidade -----------------------------

#> Quando alé do comportamento de tendencia, temos também o comportamento
#> de sazonalidade. Temos esse modelo hw que é mais completo. Esse modelo
#> consegue captar além da tendencia, a sazonalidade.

#> Podemos ter sazonalidade multiplicativa ou aditiva. Depende do tamanho
#> da sazonalidade. Se for curta, rodamos o aditivo. Se for maior, mais longa
#> rodamos o modelo multiplicativo.

# ######
# Modelo Aditivo:
modholtsazonalad= hw (base, h=3, seasonal="additive")

# valores previstos
modholtsazonalad

# modelo gerado
modholtsazonalad$model

# visualização dos dados e das previsões com intervalos de confiança
autoplot(modholtsazonalad)

# ######
# Modelo Multiplicativo:
modholtsazonalm=hw(base,h=3,seasonal="multiplicative")

# valores previstos
modholtsazonalm

# modelo gerado
modholtsazonalm$model

# visualização dos dados e das previ?es com intervalos de confiança
autoplot(modholtsazonalm)



# x -----------------------------------------------------------------------
# Comparando os modelos de previsão ---------------------------------------


# Dados -------------------------------------------------------------------
ambev <- read_excel("raw_datas_exemplos/ambev.xlsx")

# serie de tempo
ambev = ambev[2]
ambev = ts(ambev,start=c(2000,1), end=c(2022,4), frequency=4)
ambev

# total de observações
length(ambev)


# Separar Treino e Teste --------------------------------------------------
# Separar a base de dados em uma janela para criar o modelo 
# outra para prever e avaliar a qualidade da previsão


# base para rodar o modelo
# Até 2000 vamos gerar o modelo - treino
bambev = window (ambev, start= c(2000, 1), end=c(2020,4))
plot(bambev)

# a partir de 2021, vamos prever o modelo - teste
reais=window(ambev,start=c(2021,1), end= c(2022,4))
plot(reais)
length(reais)


# Métricas ----------------------------------------------------------------
# Fazendo as previsões e calculando a estatística MAPE de qualidade
# das previsões


# xxxxxxxx
# Modelo 1: Previsão pelo alisamento exponencial simples (SES)
ses = ses (bambev, h=8)
prevses = ses$mean
prevses #valores previstos para os 4 trimestres de 2021 e 2022

# Transformar a previsão em uma série temporal
pses = ts(prevses, start = c(2021,1), end = c(2022,4), frequency = 4)

# Métricas de erro
qualises=forecast::accuracy(pses,reais)
qualises
qualises=forecast::accuracy(pses,reais)[5] # pegando o MAPE
qualises

# xxxxxxxx
# Modelo 2: previsão pelo Holt-Winters com Tendência
holttend = holt(bambev, h=8)
prevholttend = holttend$mean

# Transformar a previsão em uma série temporal
pholt=ts(prevholttend, start = c(2021,1), end = c(2022,3),frequency=4)
qualiholt=forecast::accuracy(pholt,reais)[5]
qualiholt


obs <- c(57.47, 111.52,96.77,164.33,127.41)
esp <- c(67.94,91.92,94.59,132.96,129.90)
sqrt(mean((obs - esp)^2))

# xxxxxxxx
# Modelo 3: Previsão pelo Holt-Winters Sazonal Aditivo
hwaditivo = hw (bambev, h=8, seasonal = "additive")
hwaditivo
phwaditivo=hwaditivo$mean
phwaditivo
# Transformar a previsão em uma série temporal
pha = ts(phwaditivo,start = c(2021,1), end = c(2022,4), frequency = 4)
qualihwa=forecast::accuracy(pha,reais)[5]
qualihwa


# xxxxxxxx
# Modelo 4: Previsão pelo Holt-Winters Sazonal Multiplicativo
hwmult = hw(bambev, h=8, seasonal = "multiplicative")
hwmult
phwmult=hwmult$mean
phwmult
# Transformar a previsão em uma série temporal
phm = ts(phwmult, start = c(2021,1), end = c(2022,4), frequency = 4)
qualihwm=forecast::accuracy(phm,reais)[5]
qualihwm


# Comparando: Analisando as acurácias das previsões ---------------------
modelos = c ("SES","HOLT_T", "HW_ADIT", "HW_MULT")
mape = c(qualises, qualiholt, qualihwa, qualihwm)
tabela = data.frame (modelos, mape)
tabela


# x -----------------------------------------------------------------------
# x -----------------------------------------------------------------------
# Modelo ETL --------------------------------------------------------------
# EXPONENTIAL SMOOTHING
# Esse modelo possibilita a escolha automática do modelo pelo R.
# Se deixarmos N = none, A = additive, M = multiplicative e Z = automatic
# Entao temos três campos para designar:
# E (Error) T (Trend) S (Season)

# Erro: aditivo (A) ou multiplicativo (M)

# Tendência: nenhuma (N), aditiva (A),multiplicativa (M) 
#            ou amortecida (Ad ou Md)

# Sazonalidade: nenhuma (N), aditiva (A) ou multiplicativa (M)

# z - automático


### Fazer o Modelo:
# Treino
bambev = window(ambev, start = c(2000,1), end = c(2020,4))
# Teste
reais = window(ambev, start = c(2021,1), end = c(2022,4))
# Grafico
autoplot(ambev)+
  autolayer(bambev,series="Treino") +
  autolayer(reais,series = "Reais") +
  scale_color_viridis_d() +
  scale_y_continuous(labels = scales::comma) +
  theme_bw()
# Holt-Winters Ambev
ambev.hw <- forecast::hw(bambev, h = 8, seasonal = "additive")
summary(ambev.hw)
# Grafico
autoplot(ambev) +
  forecast::autolayer(ambev.hw,
                      series = "Holt-Winters adit",
                      PI = FALSE) +
  xlab("Ano") +
  ylab("Receita Líquida") +
  ggtitle("Forecasts para AMBEV") +
  guides(colour = guide_legend(title = "Forecast")) +
  scale_color_viridis_d(option = "cividis") +
  scale_y_continuous(labels = scales::comma) +
  theme_bw()
# Métricas
forecast::accuracy(ambev.hw$mean,reais)


# Usando ETS --------------------------------------------------------------
# N = none, A = additive, M = multiplicative e Z = automatic

# Modelo
# model = "ZZZ" - ETS: pedindo para r escolher o melhor modelo para os três
#                 fatores
# Com o summary podemos ver qual o modelo escolhido para cada fator do ETS
# ETS(M,A,M) : Dizendo que calculou multiplicativo para Error e Sazonalidade
#              e aditivo para tendencia
ambev.ets <- ets(bambev, model = "ZZZ")
summary(ambev.ets)

# Previsoes
ambev.ets.forecasts <- forecast.ets(ambev.ets, h = 8)
summary(ambev.ets.forecasts)
# Métricas
forecast::accuracy(ambev.ets.forecasts$mean,reais)


# Analisando resíduos -----------------------------------------------------
# Analisando os resíduos (erros) das previsões
# Condições:
# não podem ser correlacionados; se forem correlacionados ficaram informações
# nos resíduos que deveriam estar no modelo
# devem possuir média zero e variancia constante,
# caso não seja então as previsões são enviesadas
autoplot(ambev.ets$residuals)
acf(ambev.ets$residuals)

#> Interpretação do ACF:
# As barras indicam que não houve nenhuma correlação dos resíduos, pois
# em todos os lags, as barras pretas estão dentro do pontilhado azul.
# apenas no Lag0, que representa a autocorrelação dos resíduos com eles mesmo
# está ultrapassando os pontilhados azuis.


# Teste de Ljung-box ------------------------------------------------------
# Teste de autocorrelação dos resíduos
# H0: os resíduos são independente e homogeneamente distribuídos
#     ou seja, o modelo não exibe falhas de ajustes
# H1: os resíduos não são independente e homogeneamente distribuídos
#     ou seja, o modelo exibe falhas de ajustes

# Não quero rejeitar H0 (quero um pvalor grande)
Box.test(ambev.ets$residuals, lag=1,type=c("Ljung-Box"))

# Box-Ljung test
# data:  ambev.ets$residuals
# X-squared = 0.10756, df = 1, p-value = 0.7429

#> Conclusão: Não rejeito Ho. Ou seja, os resíduos são independentes.
#> Logo, esse modelo representa bem esses dados, captando os padrões.






# x -----------------------------------------------------------------------
# Exemplo Real ------------------------------------------------------------
# Caso Prático: Previsão Consumo Energia Elétrica na região sudeste (Gwh)
# Fonte: http://www.ipeadata.gov.br/Default.aspx


# Dados -------------------------------------------------------------------
energia <- read_excel("raw_datas_exemplos/energia.xlsx")


# Criar série Temporal ----------------------------------------------------
energia = energia[2]
energia = ts (energia,start = c(1979,1), end = c(2023,1), frequency = 12)
energia


# Total de Observações ----------------------------------------------------
length(energia)


# Separar base de Dados ---------------------------------------------------
# Treino vs Teste

# Treino: base para rodar o modelo
benergia = window (energia, start = c (1979,1), end = c (2020,12))
plot(benergia)

# Teste: base para fazer previsões
reaisenergia = window (energia, start = c(2021,1), end = c(2023,1))
plot(reaisenergia)
length(reaisenergia)


# Modelo ETS --------------------------------------------------------------
energia.ets <- ets(benergia)
summary(energia.ets)

# ETS (Multiplicativo , Aditivo Amortecido, Multiplicativo)


# Previsão ----------------------------------------------------------------
# energia.ets: usando o modelo
# h = 25: fazer previsão de 25 passos a frente
energia.ets.forecasts <- forecast.ets(energia.ets, h = 25)
summary(energia.ets.forecasts)


# Grafico -----------------------------------------------------------------
autoplot(energia.ets.forecasts)


# Métricas de Erro --------------------------------------------------------
# Mape:2.172648 - menos de 2% de erro, muito baixo
forecast::accuracy(energia.ets.forecasts$mean,reaisenergia)


# Teste dos Resíduos ------------------------------------------------------
autoplot(energia.ets$residuals)
# algumas autocorrelações romperam as barras azuis.
acf(energia.ets$residuals) 
# Logo, melhor seria fazer o teste para me certificar.
Box.test(energia.ets$residuals, lag=1,type=c("Ljung-Box"))


# x -----------------------------------------------------------------------
# x -----------------------------------------------------------------------
# x -----------------------------------------------------------------------


# Pacotes -----------------------------------------------------------------
# Pacotes apara o teste arima
pacotes <- c("readr","readxl","plotly","tidyverse","gridExtra","forecast","TTR",
             "smooth", "tsibble", "fable","tsibbledata", "fpp3","lubridate",
             "urca", "dygraphs", "quantmod","BETS","tseries","FinTS","feasts",
             "gridExtra", "scales", "caret","xtable", "tsutils","GetBCBData", 
             "quantmod","dgof","seasonal","devtools","transformr","gganimate")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T)
} else {
  sapply(pacotes, require, character = T)
}


# Modelos ARIMA - (Métodos de Box - Jenkins) -------------------------------
# São modelos autoregressivos de médias móveis. Esses modelos são robustos
# para serem aplicados em praticamente qualquer tipo de série temporal. Embora
# funcione melhor com dados estáveis, com poucos outliers (embora possamos
# remove-los)

#> Requer dados estacionários:
#> - Pode ser transformada usando diferenciação: remove tendencias
#> - Diferenciação: subtrai a observação da anterior
#> - Diferenciação pode ser feita 1x: diferenciação de primeira ordem
#> - Diferenciação 2x: diferenciação de segunda ordem (mais rara)
#> No máximo 2 difenrenciações, já conseguimos estacionar esta série.

#> Temos basicamente 2 classes:
#> Modelos não sazonais - ARIMA
#> Modelos sazonais - SARIMA

#> Embora a gnt fale sempre ARIMA, podemos ter modelos:
#>                                                     
#> Modelos auto-regressivos (AR): avalia a relação entre períodos (lags):  
#>                                autocorrelação extrai a influência. Nestes
#>                                modelos os valores correntes da série Yt,
#>                                dependem apenas de seus valores passados 
#>                                e dos erros aleatórios;                  
#>                                AR(p): p é o número de defasagens        
#>                                                                         
#> Modelos Integrados (I): Aplicado a diferenciação quando necessário      
#>                                                                         
#> Modelos de Médias Móveis (MA): Avalia erros entre períodos e extrai esses
#>                                erros.                                   
#>                                                                         
#> Modelos auto-regressivos e de média móveis (ARMA)                       
#>                                                                         
#> Modelos auto-regressivos integrados e de média móveis (ARIMA)           
#>                                                                         

# Dados -------------------------------------------------------------------
# Simular dados
pontos = 500
set.seed(1)


# Modelo AR(1) ------------------------------------------------------------
# Modelo Auto-regressivo de primeira ordem (= o modelo de hoje é explicado 
# pelo dado anterior).
# Simulando um modelo AR(1)
modelo_ar = list(ar=0.8)
dp = 1
# Gerar modelo simulado aima.sim()
serie_ar = arima.sim (n = pontos, model = modelo_ar, sd = dp)
plot(serie_ar, ylab=" Modelo AR(1)", main="Modelo AR(1) X(t)=0.8.X(t-1) + erro(t)")
autoplot(serie_ar)

#> Esse modelo gerado, já é estacionaria. Não tem tendencia nessa série.

# Modelo MA(1) ------------------------------------------------------------
# Modelo de médias móveis
# Simulando um modelo MA(1)
modelo_ma = list(ma=-0.3)
dp = 1
# Gerar modelo simulado
serie_ma = arima.sim(n=pontos,model=modelo_ma,sd=dp)
plot(serie_ma, ylab=" Modelo MA(1)", main="Modelo MA(1) X(t)=-0.3erro(t-1) + erro(t)")


# Modelo ARMA(1,1) --------------------------------------------------------
# Modelo Auto-regressivo de ordem 1 e médias móveis de ordem 1
# Simulando um modelo ARMA(1,1)
modelo_arma=list(ar=0.8, ma=-0.3)
dp=1
# Gerar modelo simulado
serie_arma=arima.sim(n=pontos,model=modelo_arma,sd=dp)
plot(serie_arma, ylab=" Modelo ARMA(1,1)", main="Modelo ARMA(1,1)")


# Modelo ARIMA(1,1,1) --------------------------------------------------------
# Modelo Autoregressivo de ordem 1, Integrado (ou seja, com tendencia), e 
# médias móveis de ordem 1.
# Simulando um modelo ARIMA(1,1,1)
modelo_arima=list(order=c(1,1,1),ar=0.8,ma=-0.3)
dp=1
serie_arima=arima.sim(n=pontos,model=modelo_arima, sd=dp)
plot(serie_arima, ylab=" Modelo ARIMA(1,1,1)", main="Modelo ARIMA(1,1,1)")



# Testes de Estacionaridade -----------------------------------------------
# Para analisar as séries Autoregressivas, começamos analisando a 
# estacionaridade. Podemos usar o pacote URCA-Unit Root and Cointegration Test

# XXXXXXXX
# OPÇÃO 1: 
# Teste de Dickey-Fuller (mais usado)
# H0: A série Não é Estacionária
# H1: A série é Estacionária

# Devemos olhar o p-valor do z.lag.1
# Coefficients:
#           Estimate    Std. Error   t value   Pr(>|t|)    
# z.lag.1    -0.001675   0.001584  -1.058    0.291 
# z.diff.lag  0.635263   0.034622  18.349   <2e-16 ***


# TESTAR AS SÉRIES SIMULADAS
testeDF_ar = ur.df(serie_ar)
testeDF_ar
summary (testeDF_ar)
# Conclusão: p-value 4.28e-13 *** < 0.01 (99% confiança) - REJEITO H0,
# portanto a série é estacionária

testeDF_ma=ur.df(serie_ma)
summary(testeDF_ma)
# Conclusão: p-value 2.e-16*** < 0.01 (99% confiança) - REJEITO H0,
# portanto a série é estacionária

testeDF_arma=ur.df(serie_arma)
summary(testeDF_arma)
# Conclusão: p-value 7.52e-12*** < 0.01 (99% confiança) - REJEITO H0,
# portanto a série é estacionária


# ATENÇÃO: vamos rodar para a série ARIMA, tem o I = 1 (ou seja, ela foi
# criada para ser integrado, ter tendencias, ou seja, não ser estacionaria)
testeDF_arima=ur.df(serie_arima)
summary(testeDF_arima)
# Conclusão: p-value 0.537 > 0.01 (99% confiança) - ACEITO H0,
# portanto a série NÃO é estacionária


# XXXXXXXX
# OPÇÃO 2:
# Teste de KPSS
# H0: A série é Estacionária
# H1: A série NÃO é Estacionária

# TESTAR DADOS SIMULADOS

testeKPSS_ar=ur.kpss(serie_ar)
summary(testeKPSS_ar)
# Conclusão: t-test= 0.2297 < 0.739 (ponto crítico para 99% confiança)
# - ACEITO H0, portanto a série é estacionária

testeKPSS_ma=ur.kpss(serie_ma)
summary(testeKPSS_ma)
# Conclusão: t-test= 0.0685 < 0.739 (ponto crítico para 99% confiança)
# - ACEITO H0, portanto a série é estacionária

testeKPSS_arma=ur.kpss(serie_arma)
summary(testeKPSS_arma)
# Conclusão: t-test= 0.2429 < 0.739 (ponto crítico para 99% confiança)
# - ACEITO H0, portanto a série é estacionária

testeKPSS_arima=ur.kpss(serie_arima)
summary(testeKPSS_arima)
# Conclusão: t-test= 3.0369 > 0.739 (ponto crético para 99% confiança) 
# - REJEITO H0, portanto a série NÃO é estacionária



# Estimar Modelo ARIMA ----------------------------------------------------
# Devemos escolher os parâmetros: p | q | d
# Vamos então tentar descobrir quais são os parâmetros autoregressivos, 
# de integração e de médias móveis.

# • p: ordem da parte autorregressiva: PACF
# • d: grau de diferenciação – Teste de Estacionariedade
# • q: ordem da média móvel: ACF


# Autocorrelação ----------------------------------------------------------
# Para isso vamos avaliar a função de Autocorreção e da Autocorrelação Parcial
# Função de autocorrelação:
#> - Coeficiente de correlação de Pearson (r)
#> - Coeficiente de Determinação (R²)
#> - Autocorrelação:
#>   Vai medir se existe uma relação matemática entre os intervalos da série
#>   temporal; 
#>   Deve estar entre -1 e +1, sendo zero a ausencia de autocorrelação
#>   Medida em intervalos (lags): 1 intervalo - mede como os valores de 1
#>                                              período (vizinhos) distantes
#>                                              estão relacionados
#>                                2 intervalos - mede com os valores de 2
#>                                               períodos distantes estão 
#>                                               relacionados

# Para estudar a autocorrelação, estudamos os gráficos da função: correlogramas
# FAC (ACF): Função de Autocorrelação
#>           Mostra as autocorrelações em uma série temporal
#>           Linhas mostram a significancia (intervalo de confiança)
#>           A 1º autocorrelação é igua a 1. Cada traço do gráfico mostra
#>           uma defasagem e uma correlação (autocorrelação)

#> FACP (PACF): Função de Autocorrelação Parcial
#>              Mede a autocorrelação não entre lags mas entre diferentes
#>              intervalos

#>  Temos que olhar os padrões de comportamento. Nos slides, para saber 
#>  os tipor de gráficos
ggtsdisplay(serie_ar)
acf(serie_ar)
pacf(serie_ar, lag.max = 5)

# Série AR(1) não precisa de ser diferenciada
# Quantas diferenciações seriam necessárias para tornar a série estacionária

ndiffs(serie_ar)
# Resposta = 0

ndiffs(serie_arima)
# Resposta = 1 (a série precisa de 1 dierenciação para se tornar estacionária)

ndiffs(serie_ma)
ndiffs(serie_arma)
# Comando Geral para se estimar um modelo ARIMA, quando eu não sei a ordem
# da série, não conheço os valores de p, d e q


# Automatizar: Escolha dos Parâmetros -------------------------------------
# auto.arima()
# Lembrando: simulanos um AR(1) de coeficiente 0.8
estima = auto.arima(serie_ar)
estima

# Series: serie_ar 
# ARIMA(1,0,0) with zero mean 
# 
# Coefficients:
#       ar1
#       0.7766   # coeficiente
# s.e.  0.0280

# Lembrando: simulamos um MA(1) de coeficiente -0.3
estima1=auto.arima(serie_ma)
estima1

# Exemplo 2: SIMULANDO
# Simulando agora modelos de ordens maiores 
# por exemplo, simulando um AR(2)
modelo1 = list(ar=c(0.8,0.1))  # AR(2): ar1=0.8, ar2=0.1
serie1 = arima.sim(n=pontos, model=modelo1, sd=dp)
plot(serie1, ylab="modelo AR(2)")

# Teste de Estacionariedade - Teste Dickey-Fuller
teste1=ur.df(serie1)
summary(teste1)

# Conclusão: z.lag1 tem p-valor < 0.01 - rejeita-se H0
# portanto a ST é estacionária

# Olhando as funções de ACF e a PACF
acf(serie1)
pacf(serie1)

# Exemplo 3: SIMULANDO
modelo2=list(ar=c(0.5,0.1,0.3))  # AR(3)
serie2=arima.sim(n=pontos, model=modelo2, sd=dp)
plot(serie2)
acf(serie2)
# Interpretação: esse decaimento no ACF é caracteristica de um modelo 
# autoregressivo
pacf(serie2, lag.max = 5)
# Interpretação: três barras estão fora do tracejado. Significa que 3 lags deram
# significativo ou seja, temos um modelo de autoregressivo de ordem 3. As barras
# são positivas,

# Exemplo 4: SIMULANDO
modelo3=list(ar=c(0.5,-0.1,-0.3))  # AR(3)
serie3=arima.sim(n=pontos, model=modelo3, sd=dp)
plot(serie3)
acf(serie3)
pacf(serie3, lag.max=5)

# IDENTIFICANDO OS MODELOS
estima1=arima(serie1,order=c(2,0,0))
estima1

estima2=arima(serie2, order=c(3,0,0))
estima2

estima3=arima(serie3,order=c(3,0,0))
estima3
# 
# Call:
#   arima(x = serie2, order = c(3, 0, 0))
# 
# Coefficients:
#          ar1     ar2     ar3  intercept
#       0.5469  0.0532  0.2448    -0.2630 #coeficientes de auregressao na ordem 1, 2 e 3
# s.e.  0.0433  0.0497  0.0433     0.3011



# Exemplo 5: SIMULANDO
# partindo para a identificação do modelo SEM conhecer a ordem
# auto.arima ()
# testa vários modelos para ver qual modelo se ajusta mais
estima31 = auto.arima(serie3, trace=T)
estima31

# Estimar ARMA(2,2)
modelo4 = list(ar=c(0.8,0.1), ma=c(0.4, -0.3))
serie4 = arima.sim(n=pontos, model=modelo4, sd=dp)
plot(serie4)

# Passo1: Estacionariedade
teste4 = ur.df(serie4)
summary(teste4)
#> Conclusão: a série4 é estacionária com p-valor z.lag.1 <0.01. rejeitamos
#> a hipótese nula.

# Passo2: Autocorrelaçao
acf(serie4) # mostra que é autoregressivo
pacf(serie4) #mostra que é de ordem 4 ou 5

# Passo3: Estimando os valores dos parâmetros para o modelo
estima4 = arima(serie4, order=c(2,0,2))
estima4
# ar1: parametro autogressivo
# ma1: média móvel

# Ou podíamos usar o auto.arima() para o R verificar o melhor modelo
estima4 = auto.arima(serie4, trace = T)


### Caso encontre ARIMA(0,0,0) - não foi possível encontrar memória
## autoregressiva significativa



# Modelo SARIMA -----------------------------------------------------------
# Modelo ARIMA com sazonalidade se chama SARIMA.
# Possui os parâmetros P | D | Q sazonais
#                      p | d | q

# Então o  SARIMA (p,d,q)(P,D,Q)



# Dados -------------------------------------------------------------------
# Buscando a série do Índice de Volume de Vendas de SP pelo pacote BETS - 
# Brazilian Economic Time Series ou GetBCBData

# Download da série
# código da série que queremos: 1475
# Alternativa: pacote: GetBCBData
varejo2 = gbcbd_get_series (1475,first.date='2000-01-01')
varejo2 = ts(varejo2[2], start = c(2000,1), end = c(2022,12), frequency = 12)
plot(varejo2)


# Treino/Teste ------------------------------------------------------------
# Divisão de janelas
# Até 2020 para treinar
varejotreino = window(varejo2, start=c(2000,1), end=c(2020,12))
length(varejotreino)

# Jan 2021 até Dez 2022 para previsão / teste
varejoteste=window(varejo2,start=c(2021,1), end=c(2022,12))
length(varejoteste)


# Gráfico -----------------------------------------------------------------
dygraph(varejo2)

# Plotando as séries Treino e Teste juntas para checagem
autoplot(varejo2) +
  autolayer(varejotreino, series="Treino") +
  autolayer(varejoteste, series="Teste") +
  scale_color_viridis_d() +
  theme_bw()


# Análise da Série --------------------------------------------------------
# Faz a função de autocorrelação e autocorrelação parcial
ggtsdisplay(varejotreino)



# Passos ------------------------------------------------------------------
# Com base nos gráficos anteriores, a série não parece estar estacionária.
# Caso não esteja precisaremos estacionar ela, e para isso precisa ser 
# diferenciada.

####
# 1. Testar se a Série é estacionária
# Parece que existe sazonalidade

# Teste de Estacionariedade
testevarejo = ur.df(varejotreino)
summary(testevarejo)

# p-valor de z.lag.1 > 0.05: A série NÃO é estacionária - precisa ser 
# diferenciada

####
# 2. Diferenciar a série - caso não seja estacionária ela será diferenciada.
# Está série é uma série integrada, com tendencia.
ndiffs(varejotreino)
#> Resultado = 1. Logo precisamos de apenas 1 diferenciação. O ponto 2 menos 
#> o ponto 1.

# Diferenciando a série - função diff()
# daí teremos uma série diferenciada
difvarejotreino = diff(varejotreino)
ggtsdisplay(difvarejotreino)

# Agora temos uma série sem tendencia. Que já foi diferenciada. Vamos olhar 
# agora para ACF e PACF. E parece que temos uma sazonalidade na série.
# Pois a cada 12 meses temos um lag significativo.


####
# 3. Identificar o melhor modelo para a série diferenciada

# Testar novamente a estacionaridade da serie apos ter sido diferenciada
testevarejodif=ur.df(difvarejotreino)
summary(testevarejodif)
# p-valor de z.lag.1 < 0.05: A série É estacionária 


# Qual é o melhor modelo?
# note: usando os dados originais da série separado para treino
# antes de diferenciar. Poderíamos ter feito isso antes de explorar
# com os passos anteriores para saber sobre como a série se comporta
arimavarejo = auto.arima(varejotreino, trace=T)

#> Resultado:
#> Best model: ARIMA(1,1,2)(0,1,1)[12] 
#> Mostra que temos um modelo ARIMA com sazonalidade de 12 meses.
#> Modelo ARIMA para a parte estacionaria de série é:
#>              autogressivo de ordem 1,
#>              integrado de ordem 1
#>              médias móveis de ordem 2
#> Para a parte sazonal é:
#>              autogressivo de ordem 0,
#>              integrado de ordem 1
#>              médias móveis de ordem 1
#>              Com período de sazonalidade 12
 

# Validação ---------------------------------------------------------------
# Validação e diagnóstico: Checar os resíduos do modelo.
# O modelo escolhido na etapa anterior :
# arimavarejo = auto.arima(varejotreino, trace=T)
checkresiduals(arimavarejo)

#> Resíduos distribuídos ao redor de zero. A função ACF não tem nenhum 
#> lag significativo, mostrando que o modelo é adequado para esses dados


# 1. Testar correlção dos resíduos:
#    Teste de Ljung-Box p-value = 0.9174>0.01, aceitamos H0, resíduos
#    não são correlacionados
checkresiduals(arimavarejo)

# Ljung-Box test
# data:  Residuals from ARIMA(1,1,2)(0,1,1)[12]
# Q* = 11.96, df = 20, p-value = 0.9174
# Model df: 4.   Total lags used: 24



# 2. Testar Normalidade dos resíduos (Kolmogorov-Smirnov)
ks.test(arimavarejo$residuals, "pnorm", mean(arimavarejo$residuals),
        sd(arimavarejo$residuals))
# p-valor = 0.04891 > 0,01 - Aceita H0, ou seja, resíduos normais

#> CONCLUSÃO: confirmada a não existência de autocorrelação serial e
#> normalidade dos resíduos. 
#> Se ainda tivesse autocorrelação e os resíduos fossem heterocedásticos
#> teríamos que testar outros modelos mais complicados da família ARCH (
#> modelos de redes neurais, por exemplo)

 
#> Podemos verificar a estacionariedade de
#> variância verificar se existe efeitos ARCH
ArchTest(arimavarejo$residuals)

# p-valor 0.1362 > 0,01, aceita-se H0, não se rejeita a H0, garante a não
# existência de efeitos ARCH



# Previsão ----------------------------------------------------------------
# Previsao para a série de varejo SP
# Previsão para 24 meses
prevvarejo=forecast::forecast(arimavarejo, h=24)

# Valores
prevvarejo

# Grafico
autoplot(prevvarejo) +
  theme_bw()


# Métricas de Acurácia ----------------------------------------------------
# Dados previstos e reais (varejo teste)
forecast::accuracy(prevvarejo, varejoteste)
# olhar o "test set"

# Grafico
ggplotly(
  autoplot(varejotreino)+
    autolayer(varejoteste,serie="Valores Reais")+
    autolayer(prevvarejo$mean, serie="Forecast")+
    scale_colour_viridis_d()+
    scale_y_continuous(labels=scales::comma)+
    theme_bw()
)


# Modelo ETS --------------------------------------------------------------
# Testando um modelo ETS (ERRO, TENDENCIA E SAZONALIDADE)

ETS = forecast(ets(varejotreino),h=24)
summary(ETS)

#> ETS(M,Ad,M) : Neste caso, gerou um modelo Multiplicativo para erro, 
#> Aditivo Amortecido (Ad) para a tendencia e Multiplicativo para a 
#> sazonalidade

ggplotly(
  autoplot(varejotreino)+
    autolayer(varejoteste,serie="Valores Reais")+
    autolayer(ETS$mean, serie="Previstos")+
    scale_colour_viridis_d()+
    scale_y_continuous(labels=scales::comma)+
    theme_bw()
)
autoplot(ETS) +
  theme_bw()

# Métricas
forecast::accuracy(ETS$mean, varejoteste)


# Teste de Estacionaridade: Teste de Dickey-Fuller 
# H0: A série Não é Estacionária
# H1: A série é Estacionária
testeDF_ar=ur.df(varejotreino)
testeDF_ar
summary(testeDF_ar)


# Transformação BoxCox ----------------------------------------------------
#> Como a série não é estacionaria (p valor = 0.978). Poderíamos 
#> fazer uma transformação de Boxcox na serie original.


# Fazendo uma transformação de Box-Cox : tentativa de suavizar a série
# melhorar o seu comportamento
l = BoxCox.lambda(varejo2)
# Predição: passando os valores transformados por lambda
ETS_transf = forecast (ets (varejotreino, lambda = l), h=24)
summary(ETS_transf)
# Gráfico
autoplot(forecast(ETS_transf,h=24)) +
  xlab("Ano") +
  ylab("Índice Base 100") +
  ggtitle("Índice de volume de vendas no varejo total de SP BCB ") +
  theme_bw()
# Métrica
forecast::accuracy(ETS_transf,varejoteste)["Test set","MAPE"]
# Comparar sem transformação
forecast::accuracy(ETS$mean, varejoteste)

#> CONCLUSÂO: Após transformação de boxCox o MAPE ficou menor. Ou seja, 
#> melhorou o modelo.




# x -----------------------------------------------------------------------
# Exemplo2: Inflação ------------------------------------------------------
# Esse exemplo o modelo ARIMA não funciona para gerar boas previsões

# Dados -------------------------------------------------------------------
# Prevendo a Inflação - IPCA - BACEN
# Dados online
ipca1 = gbcbd_get_series(433,first.date='2007-01-01')
# Serie temporal
ipca = ts(ipca1[2], start = c(2007,1), end=c(2023,3), frequency = 12)
# Grafico
autoplot(ipca) +
  theme_bw()

# Analisando os dados mês a mês -------------------------------------------
# A linha traceja é a média de cada mês
monthplot(ipca, col.base=1,lty.base = 2, col = par("col"))

# Separando Treino e Teste ------------------------------------------------
sipca=window(ipca, start=c(2007,1), end=c(2022,3))
plot(sipca)
teste=window(ipca, start=c(2022,4), end=c(2023,3))
plot(teste)
length(teste)


# Analisando a série para estimação ---------------------------------------
ggtsdisplay(sipca)
acf(sipca)
pacf(sipca)


# Estimando um modelo inicial ---------------------------------------------
mod=Arima(sipca,order = c(1,0,0), seasonal=c(0,0,1))
mod


# Estimando pelo auto.arima() ---------------------------------------------
modelo=auto.arima(sipca,trace = T)

# Fazendo a previsão do modelo COM sazonalidade -------------------------
pipca<-forecast::forecast(mod,h=12)


# Grafico -----------------------------------------------------------------
autoplot(pipca) +
  theme_bw()


# Metrica -----------------------------------------------------------------
forecast::accuracy(pipca,teste)


# Fazendo a previsão do modelo COM sazonalidade -------------------------
psipca<-forecast::forecast(modelo,h=12)


# Grafico -----------------------------------------------------------------
autoplot(psipca) +
  theme_bw()


# Previsao ----------------------------------------------------------------
forecast::accuracy(psipca,teste)



# Checar Pressupostos --------------------------------------------------------
## agora que temos um modelo definido precisamos saber se o modelo capturou
## toda a estrutura do processo
## Significa que devemos checar se os resíduos do modelo estão limpos
## quer dizer, devemos ter resíduos não autocorrelacionados e normalmente
## distribuídos

# 1. Teste se os resíduos são não autocorrelacionados
# Teste de Ljung-Box
# H0: independência da ST, isto é, resíduos não correlacionados no tempo
# H1: dependência da ST, isto é, resíduos correlacionados, indicando que o 
# modelo não capturou alguma estrutura que indica um erro sistemático
checkresiduals(pipca)

#> CONCLUSÃO: O Teste de Ljung-Box, o p-valor > 0,05 - Não Rejeição de H0,
#> ou seja, resíduos não são correlacionados no tempo



# 2. Teste de Normalidade dos Resíduos
# Teste de Kolmogorv-Smirnov
# H0: Resíduos com comportamento normal
# H1: Resíduos sem normalidade

# Teste de Shapiro-Wilk (n<30), KS>30
ks.test(pipca$residuals, "pnorm", mean(pipca$residuals), 
        sd(pipca$residuals))
# CONCLUSAÕ: p-value >0,01 - Aceita H0, resíduos são normais



# 3. Testar a estacionariedade da variância
## testar se existe efeitos ARCH
# H0: Não Existe Efeitos ARCH
# H1: Existe Efeitos ARCH

ArchTest(pipca$residuals)

#> CONCLUSÃO: o p-value < 0,05 Rejeita-se a H0, a série apresenta efeitos ARCH.
#> Isso significa que o modelo ARIMA não é um bom modelo para fazer essas 
#> previsões, os erros foram muito grandes. Outro modelo mais complexo deve ser
#> explorado.



# x -----------------------------------------------------------------------
# Previsão com Pacote FABLE  -------------------------------------------------
# O pacote fable nos permite trabalhar com um conjunto de séries temporais,
# com mais de uma série temporal de uma vez.Podemos trabalhar com várias 
# séries empilhadas.


# Dados -------------------------------------------------------------------
# lendo um arquivo csv direto de um endereço eletrônico e convertendo para
# tsibble
# vamos usar série do covid19
# Script: github.com/paulamacaira

covid = read_csv("https://raw.githubusercontent.com/wcota/covid19br/master/cases-brazil-states.csv")
covid
covid %>% View()

covid = read_csv("https://raw.githubusercontent.com/wcota/covid19br/master/cases-brazil-states.csv") %>%
  select(date, state, newDeaths, newCases)%>%
  as_tsibble(
    index = date,
    key = state
  ) %>%
  filter(date < lubridate::today()) %>%
  group_by(state) %>%
  mutate(MM_mortes = zoo::rollmean(newDeaths, k = 7, fill = NA, align = "right"),
         MM_casos = zoo::rollmean(newCases, k = 7, fill = NA, align = "right"))
covid


# Gráfico -----------------------------------------------------------------
covid %>%
  filter(state == "TOTAL") %>%
  autoplot(newDeaths, color = "#DCE319FF") +
  geom_line(aes(y = MM_mortes), color = "#440154FF") +
  labs(x="Dia",
       y="Mortes", title="Média Móvel (7 dias) do número de mortes por COVID-19 no Brasil") +
  theme_bw()

# Gráficos juntos de todos os estados
covid %>%
  filter(state != "TOTAL") %>%
  autoplot(MM_mortes) +
  scale_color_viridis_d() +
  scale_x_date(date_labels = "%m-%Y", date_breaks = "1 month") +
  labs(x="Dia",y="Mortes (MM 7 dias)") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.4, size = 10),
        panel.background = element_rect(fill = "white", color = "black"),
        panel.grid = element_line(color = "grey90"),
        panel.border = element_rect(color = "black", fill = NA),
        legend.text = element_text(size = 7),
        legend.position = "bottom")


# Pacote gganimate --------------------------------------------------------
#Gráfico de Mortes (MM 7 dias) ao longo do tempo por Estado separadamente
#(funções transition_states e animate do pacote gganimate)
covid2 <- covid %>% filter(state != "TOTAL")

ggplot(covid2, aes(x=date, y=MM_mortes, color=state)) +
  geom_line() +
  transition_states(state, transition_length = 1, state_length = 2) + 
  enter_fade() + 
  exit_shrink() +
  ease_aes('linear') + 
  labs(x = "Dias",
       y = "Mortes (MM 7 dias)") +
  scale_color_viridis_d() +
  ggtitle("Mortes (MM 7 dias) por Estado", subtitle = "Estado: {closest_state}") +
  theme_minimal() -> p

### Atenção: vai demorar alguns minutos. Aparecerá um contador decrescente de 
# tempo. 
animate(p, nframes = 300, fps = 20)
#anim_save("output_file.gif", animation = p, ani.width = 800, ani.height = 600, nframes = 300, fps = 20)

# Graficos separados ------------------------------------------------------
covid %>%
  filter(state != "TOTAL") %>%
  autoplot(MM_mortes) +
  scale_color_viridis_d() +
  facet_wrap(~state, scales = "free") +
  labs(x="Dia",y="Mortes (MM 7 dias)") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.4, size = 5),
        panel.background = element_rect(fill = "white", color = "black"),
        panel.grid = element_line(color = "grey90"),
        panel.border = element_rect(color = "black", fill = NA),
        legend.position = "none")


# Modelar: varias séries simultaneas ----------------------------------------
# Rodar uma série de modelos padrões de séries temporais (MEAN, NAIVE, SNAIVE
# e RW) para todas as séries ao mesmo tempo


# Usamos a função model() gerar o modelo
# está configurado para gerar somente o modelo para a base total = Brasil
table(covid$state)
covid_fit = covid %>%
  filter(state == "TOTAL") %>%
  model(
    Seasonal_naive = SNAIVE(newCases),
    Naive = NAIVE(newCases),
    Drift = RW(newCases ~ drift()),
    Mean = MEAN(newCases)
  )
covid_fit


# se desejarmos mais estados além do total:
covid_fit3 = covid %>%
  filter(state %in% c("TOTAL", "RJ", "SP")) %>%
  model(
    Seasonal_naive = SNAIVE(newCases),
    Naive = NAIVE(newCases),
    Drift = RW(newCases ~ drift()),
    Mean = MEAN(newCases)
  )
covid_fit3


# Previsão ----------------------------------------------------------------
# para produzir as previsões use a função forecast()

covid_fc = covid_fit %>%
  forecast(h = 12)
covid_fc %>% View()

covid_fc3 = covid_fit3 %>%
  forecast(h = 12)
covid_fc3 %>% View()

# fable é uma tabela de previsão com previsões pontuais e distribuições
# se quiser ver somente as previsões
covid_fc %>%
  autoplot() +
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  scale_y_continuous(labels = scales::comma) +
  theme_bw()

covid_fc3 %>%
  autoplot() +
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  scale_y_continuous(labels = scales::comma) +
  theme_bw()

# se quiser ver somente as previsões
covid_fc %>%
  autoplot(covid) +
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  scale_y_continuous(labels = scales::comma) +
  theme_bw()

covid_fc3 %>%
  autoplot(covid) +
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  scale_y_continuous(labels = scales::comma) +
  theme_bw()

covid_fc %>%
  autoplot(covid, level = NULL) +
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  scale_y_continuous(labels = scales::comma) +
  theme_bw()

covid_fc3 %>%
  autoplot(covid, level = NULL) +
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  scale_y_continuous(labels = scales::comma) +
  theme_bw()

# visualização a partir de 01/01/2022
covid_fc %>%
  autoplot(covid %>% filter(date >= "2022-01-01"), level = NULL) +
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  scale_y_continuous(labels = scales::comma) +
  theme_bw()

covid_fc3 %>%
  autoplot(covid %>% filter(date >= "2022-01-01"), level = NULL) +
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  scale_y_continuous(labels = scales::comma) +
  theme_bw()


# ETS: Exponencial Smoothing ----------------------------------------------
#> E (Error) T (Trend) S (Season)
#> Erro: aditivo (A) ou multiplicativo (M)
#> Tendência: nenhuma (N),
#>            aditiva (A),
#>            multiplicativa (M) 
#>            amortecida (Ad ou Md)
# Sazonalidade: nenhuma (N), aditiva (A) ou multiplicativa (M)

# Somente dados do RJ | somente novos casos
covid_RJ = covid %>%
  filter(state == "RJ") %>%
  select(newDeaths)  

# Grafico
covid_RJ %>%
  autoplot() +
  theme_bw()

# Modelo ETS para o RJ
fit = covid_RJ %>%
  model(ets = ETS(newDeaths))
fit
#> Resultado: Identificou modelo Aditivo para o Erro | Nenhum para tendencia
#>            e Aditivo para Sazonalidade
#  Key:     state [1]
# state          ets
# <chr>      <model>
# 1 RJ    <ETS(A,N,A)>

# dados
components(fit)

# Decompondo os dados  do modelo
components(fit) %>%
  autoplot() +
  theme_bw()

# Previsão
fit %>%
  forecast(h = 14) %>%
  autoplot(covid_RJ %>% filter(date >= "2021-08-01")) +
  theme_bw()

# posso forçar uma tendência
covid_RJ %>%
  model(ets = ETS(newDeaths ~ trend("Ad"))) %>%
  forecast(h = 14) %>%
  autoplot(covid_RJ %>% filter(date >= "2021-08-01")) +
  theme_bw()

# Prevendo para 3 regiões juntas
covid_3 = covid %>%
  filter(state %in% c("TOTAL", "RJ", "SP")) %>%
  select(newDeaths)  
covid_3 %>%
  autoplot() +
  theme_bw()

# Modelo ETS
fit = covid_3 %>%
  model(ets = ETS(newDeaths))
fit
components(fit)

# GRÁFICO
components(fit) %>%
  autoplot() +
  theme_bw()

# Previsão
fit %>%
  forecast(h = 14) %>%
  autoplot(covid_3 %>% filter(date >= "2021-08-01")) +
  theme_bw()



# Parei aqui --------------------------------------------------------------



# ARIMA

# AR: autoregressivo (observações defasadas como input)
#  I: integrado (diferenciação para tornar a série estacionária)
# MA: média móvel (erros defasados como input)


fit_ARIMA = covid_RJ %>%
  model(arima = fable::ARIMA(newDeaths))

fit_ARIMA
fabletools::report(fit_ARIMA)

fit_ARIMA %>%
  forecast(h = 14) %>%
  autoplot(covid_RJ %>% filter(date >= "2021-08-01")) +
  theme_bw()

covid_RJ %>%
  model(arima = fable::ARIMA(newDeaths ~ pdq(1,1,1)+PDQ(1,1,1))) %>%
  forecast(h = 14) %>%
  autoplot(covid_RJ %>% filter(date >= "2021-08-01")) +
  theme_bw()

covid_RJ %>%
  filter(date >= "2021-08-01") %>%
  model(ets = fable::ETS(newDeaths),
        arima = fable::ARIMA(newDeaths)) %>%
  forecast(h = 14) %>%
  autoplot(covid_RJ %>%
             filter(date >= "2021-08-01")) +
  theme_bw()


# prevendo para 3 regiões - SP, RJ e BRASIL (TOTAL)

fit_ARIMA3 = covid_3 %>%
  model(arima = fable::ARIMA(newDeaths))

fit_ARIMA3

fit_ARIMA3 %>%
  forecast(h = 14) %>%
  autoplot(covid_3 %>% filter(date >= "2021-08-01")) +
  theme_bw()

covid_3 %>%
  model(arima = fable::ARIMA(newDeaths ~ pdq(1,1,1)+PDQ(1,1,1))) %>%
  forecast(h = 14) %>%
  autoplot(covid_3 %>% filter(date >= "2021-08-01")) +
  theme_bw()

covid_3 %>%
  filter(date >= "2021-08-01") %>%
  model(ets = fable::ETS(newDeaths),
        arima = fable::ARIMA(newDeaths)) %>%
  forecast(h = 14) %>%
  autoplot(covid_3 %>%
             filter(date >= "2021-08-01")) +
  theme_bw()

# ----------------------------------------------------------------------------
# Previsão para a série de Consumo de Energia Elétrica GWh
# Ipeadata para Regiões S, SE, CO, NE e N
# Período de jan/1979 a mar/23
###############################################################################

load("consumo_brasil.RData")

View(consumo_brasil)
consumo_brasil
# Para gerar um gráfico estático:
consumo_brasil %>% 
  mutate(Data = as.Date(paste0(gsub("\\.", "-", Data),"-","01"))) %>% 
  ggplot(aes(x = Data, y = Consumo, color = regiao, group = regiao)) +
  geom_line() +
  scale_y_continuous(labels = scales::comma) +
  scale_x_date(limits = as.Date(c("1979-01-01","2023-01-01")),
               date_labels = "%Y",
               date_breaks = "1 years") +
  scale_color_viridis_d("Região") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.4, size = 8),
        panel.background = element_rect(fill = "white", color = "black"),
        panel.grid = element_line(color = "grey95"),
        panel.border = element_rect(color = "black", fill = NA),
        legend.position = "bottom")

# Para gerar um gráfico interativo:
ggplotly(
  consumo_brasil %>% 
    mutate(Data = as.Date(paste0(gsub("\\.", "-", Data),"-","01"))) %>% 
    ggplot(aes(x = Data, y = Consumo, color = regiao, group = regiao)) +
    geom_line() +
    scale_y_continuous(labels = scales::comma) +
    scale_x_date(limits = as.Date(c("1979-01-01","2023-01-01")),
                 date_labels = "%Y",
                 date_breaks = "1 years") +
    scale_color_viridis_d("Região") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.4, size = 8),
          panel.background = element_rect(fill = "white", color = "black"),
          panel.grid = element_line(color = "grey95"),
          panel.border = element_rect(color = "black", fill = NA),
          legend.position = "bottom")
) %>% layout(showlegend = TRUE,
             legend = list(orientation = "h"))

# Convertendo para TSIBBLE

consumo <- as_tsibble(consumo_brasil %>% 
                        mutate(Data = gsub("\\.", "-", Data) %>% 
                                 yearmonth()), 
                      index = Data, 
                      key = regiao)

# Observando os dados
glimpse(consumo)
consumo

#------------------------------------------------------------------------------
# separando somente a série do consumo da região SE
cons_SE = consumo %>%
  filter(regiao == "SE") %>%
  select(Consumo)

consumo %>%
  filter(regiao == "SE") %>%
  autoplot(Consumo, color = "#DCE319FF") +
  geom_line(aes(y = Consumo), color = "#440154FF") +
  labs(x="Meses",
       y="Consumo em GWh", title="Consumo em GWh - Região Sudeste") +
  theme_bw()

# Usamos a função model() gerar o modelo
# está configurado para gerar somente o modelo para a base SE

fit_SE = cons_SE %>% 
  model(ets = ETS(Consumo))

fit_SE

# -- separando somente SE de um segundo modo
consumo_fit_SE=consumo %>%
  filter(regiao == "SE") %>%
  model(
    energia.ets <- ETS(Consumo)
  )

consumo_fit_SE

# -- previsão somente para SE
consumo_fc_SE = consumo_fit_SE %>%
  forecast(h = 12)

# Observando as previsões

consumo_fc_SE %>% View()

# se quiser ver somente as previsões somente para SE
consumo_fc_SE %>%
  autoplot() +
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  scale_y_continuous(labels = scales::comma) +
  theme_bw()

# se quiser ver somente as previsões para SE em conjunto com os dados
consumo_fc_SE %>%
  autoplot(consumo) +
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  scale_y_continuous(labels = scales::comma) +
  theme_bw()

# Análise dos resíduos: Não devem ser correlacionados
# caso sejam correlacionados, dignifica que ficaram informações nos resíduos
# que deveriam estar no modelo
# os resíduos devem possuir média zero, caso não seja então as previsões são viesadas

augment(consumo_fit_SE)

# olhando apenas os resíduos do modelo ETS
augment(consumo_fit_SE) %>%
  filter(.model == "energia.ets <- ETS(Consumo)") %>%
  autoplot(.resid) +
  scale_color_viridis_d() +
  scale_y_continuous(labels = scales::comma) +
  theme_bw()

# Olhando a função de autocorrelação
augment(consumo_fit_SE) %>%
  filter(.model == "energia.ets <- ETS(Consumo)") %>%
  ACF(.resid) %>%
  autoplot() +
  theme_bw()

# fazendo uma análise mais técnica dos resíduos

# Teste de Ljung-box
# H0: os resíduos são iid
# H1: os resíduos não são iid
# não quero rejeitar H0 (quero um pvalor grande)

augment(consumo_fit_SE) %>%
  features(.resid, ljung_box)

# Medidas de acurácia --------------------------

# Separando uma parte dos dados para gerar o modelo
# Vamos separar a série de treino para os dados antes de jan/2022
# e fazer uma previsão para 12 meses até jan/2023

consumo_fit_SE2 = consumo %>%
  mutate(Data = as.Date(Data)) %>% 
  filter(regiao == "SE" & Data <= "2022-01-01") %>% 
  mutate(Data = yearmonth(Data)) %>% 
  model(energia.ets <- ETS(Consumo))

# observando o modelo
consumo_fit_SE2

# prevendo 1 ano adiante
consumo_fit_SE2 %>%
  forecast(h = "1 year") %>%
  mutate(Data = as.Date(Data)) %>% 
  filter(regiao == "SE") %>% 
  autoplot() +
  scale_y_continuous(labels = scales::comma) +
  theme_bw()

consumo_fc_SE2 = consumo_fit_SE2 %>%
  forecast(h = "1 year")

consumo_fc_SE2

accuracy(consumo_fc_SE2, consumo %>%
           filter(regiao == "SE"))


#### ------------------------------------------------------------------------
## Fazendo agora as previsões em conjunto para mais de uma região
#### ------------------------------------------------------------------------

# pegando mais de uma região
# Escolhendo as regiões SE, E e CO

consumo_fit2=consumo %>%
  filter(regiao %in% c("SE", "S", "CO")) %>%
  model(
    energia.ets <- ETS(Consumo)
  )

consumo_fit2

# previsao para mais regiões
# previsão para as regiões SE, S e CO

consumo_fc2 = consumo_fit2 %>%
  forecast(h = 12)
consumo_fc2 %>% View()

autoplot(consumo_fc2)

# se quiser ver as previsões para mais estados
consumo_fc2 %>%
  autoplot() +
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  scale_y_continuous(labels = scales::comma) +
  theme_bw()

# se quiser ver somente as previsões para mais regiões em conjunto com os dados
consumo_fc2 %>%
  autoplot(consumo) +
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  scale_y_continuous(labels = scales::comma) +
  theme_bw()

# Visualizando sem os intervalos de confiança para as regiões e os dados juntos
consumo_fc2 %>%
  autoplot(consumo, level = NULL) +
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  scale_y_continuous(labels = scales::comma) +
  theme_bw()


# Análise dos resíduos: Não devem ser correlacionados
# caso sejam correlacionados, dignifica que ficaram informações nos resíduos
# que deveriam estar no modelo
# os resíduos devem possuir média zero, caso não seja então as previsões são viesadas


# se quiser ver todos os resíduos de todos os modelos
augment(consumo_fit2) %>%
  autoplot(.resid) +
  scale_color_viridis_d() +
  scale_y_continuous(labels = scales::comma) +
  theme_bw()



# fazendo uma análise mais técnica dos resíduos

# Teste de Ljung-box
# H0: os resíduos são iid
# H1: os resíduos não são iid
# não quero rejeitar H0 (quero um pvalor grande)

augment(consumo_fit2) %>%
  features(.resid, ljung_box)

# Medidas de acurácia -------------------------------------------------
# Vamos separar a série de treino para os dados antes de jan/2022
# e fazer uma previsão para 12 meses até jan/2023

# para mais de uma série juntas
# Separando uma parte dos dados para gerar o modelo

consumo_fit4 = consumo %>%
  mutate(Data = as.Date(Data)) %>% 
  filter(regiao %in% c("SE", "S", "CO") & Data <= "2022-01-01") %>% 
  mutate(Data = yearmonth(Data)) %>% 
  model(energia.ets <- ETS(Consumo))

# modelos gerados
consumo_fit4

# prevendo 1 ano adiante
consumo_fit4 %>%
  forecast(h = "1 year") %>%
  mutate(Data = as.Date(Data)) %>% 
  filter(regiao %in% c("SE", "S", "CO")) %>% 
  autoplot() +
  scale_y_continuous(labels = scales::comma) +
  theme_bw()

consumo_fc4 = consumo_fit4 %>%
  forecast(h = "1 year")

View(consumo_fc4)

accuracy(consumo_fc4, consumo %>%
           filter(regiao %in% c("SE", "S", "CO"))
)


######### Usando ARIMA e comparando com ETS

# ----- prevendo somente para SE

consumo_fit_energ = consumo %>%
  mutate(Data = as.Date(Data)) %>% 
  filter(regiao == "SE" & Data <= "2022-01-01") %>% 
  mutate(Data = yearmonth(Data)) %>% 
  model(arima = fable::ARIMA(Consumo),
        ets <- ETS(Consumo))

consumo_fit_energ

# prevendo 1 ano adiante
consumo_fit_energ %>%
  forecast(h = "1 year") %>%
  mutate(Data = as.Date(Data)) %>% 
  filter(regiao == "SE") %>% 
  autoplot() +
  scale_y_continuous(labels = scales::comma) +
  theme_bw()

consumo_fit_energ_fc= consumo_fit_energ %>%
  forecast(h = "1 year")

consumo_fit_energ_fc

consumo_fit_energ_fc %>%
  autoplot(consumo) +
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  scale_y_continuous(labels = scales::comma) +
  theme_bw()

accuracy(consumo_fit_energ_fc, consumo %>%
           filter(regiao == "SE"))



# ----- prevendo somente para mais de uma região 

consumo_fit_energ2 = consumo %>%
  mutate(Data = as.Date(Data)) %>% 
  filter(regiao %in% c("SE", "S", "CO","NE","N") & Data <= "2022-01-01") %>% 
  mutate(Data = yearmonth(Data)) %>% 
  model(arima = fable::ARIMA(Consumo),
        ets <- ETS(Consumo))

consumo_fit_energ2

# prevendo 1 ano adiante
consumo_fit_energ2 %>%
  forecast(h = "1 year") %>%
  mutate(Data = as.Date(Data)) %>% 
  filter(regiao %in% c("SE", "S", "CO","NE","N")) %>% 
  autoplot() +
  scale_y_continuous(labels = scales::comma) +
  theme_bw()

consumo_fit_energ_fc2= consumo_fit_energ2 %>%
  forecast(h = "1 year")

View(consumo_fit_energ_fc2)

consumo_fit_energ_fc2 %>%
  autoplot(consumo) +
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  scale_y_continuous(labels = scales::comma) +
  theme_bw()

accuracy(consumo_fit_energ_fc2, consumo %>%
           filter(regiao %in% c("SE", "S", "CO","NE","N"))
)                  







