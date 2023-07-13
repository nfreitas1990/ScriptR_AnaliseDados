# Series Temporais
# Novembro 2022

# Natália Freitas

#>>>> Series Temporais:

# O que é? ----------------------------------------------------------------
# Uma série temporal pode ser entendida como uma lista de números 
# associadas a marcos de tempo. Ou seja, é um conjunto de observações
#organizadas no tempo. Existe uma dependencia de ordem das observações.

# A série temporal pode ser Univariada, quando tem apenas uma variável 
# conectada ao tempo, ou multivariada, quando tem mais de uma variável
# conectada ao tempo.

# A série temporal depende de existir: Medida, Fato e Unidade de tempo.


# Existe diferença das tabelas no R? ------------------------------------------
# Do ponto de vista prático, não tem diferença. Portanto, precisaremos
# informar para o R que estamos trabalhando com dados de séries temporais.
# A tabela é a mesma que em qualquer outra análise (tempo | variável).
# o que muda é a forma como interpretaremos. Deixamos de pensar no tempo
# como uma variável e passamos a pensar que a variável X está ordenada
# no tempo. Que existe dependencia de ordem nas observações.

#>>>> Series Temporais são especiais: 
# Vamos querer usar séries temporais, quando de fato o tempo for um fator 
# importante. Quando o tempo não importa chamamos "dados transversais". Quando
# o tempo importar chamamos "dados temporais". Nas séries temporais temos que 
# pensar que as observações tem dependencia uma com a outra. Nunca podemos
# esquecer o tempo.

# Atenção: Para separar dados em treino | teste temos que considerar o tempo.
#          não podemos simplesmente aleatorizar a separação. 

#>>>> 
# Relações entre variáveis: -----------------------------------------------
# Duas variáveis x e y não tem relação se, quando fizermos um gráfico de X
# contra Y encontramos uma nuvem em que Y sempre se distribui mais ou menos
# do mesmo jeito se fizermos cortes em X.
# Duas variáveis tem relação quando fizermos um gráfico de X contra Y
# encontramos uma nuvem em que Y sempre se distribui difenrente (como em um 
# grafico de pontos com correlação positiva ou negativa).


# Serie Temporal vamos procurar sempre gráficos com relação (esperamos ex. que
# de um dia para o outro uma medida sempre aumente ou sempre diminua. 
# ex. Ao acontecer o impacto esperamos q a poluição vá diminuindo com o passar
# dos dias)





# Pacotes Descritivos------------------------------------------------------

library(tidyverse)
library(tidymodels)
library(tsibble)
library(feasts)
library(fable)
library(timetk)
library(modeltime)
library(ggplot2)

# Dados -------------------------------------------------------------------
# Usaremos um tsibble: Uma tabela interpretada, que diz ao R para
# interpretar como uma série de tempo. É uma variação da tibble.
tabela_poluicao <- readr::read_rds("https://raw.github.com/curso-r/main-series/main/dados/cetesb_pinheiros_diario_co.rds")
#write.csv(tabela_poluicao, "raw_datas_exemplos/tabela_poluicao.csv")


# Criando uma tsibble
y <- tsibble(
  Year = 2015:2019,
  Observation = c(123, 39, 78, 52, 110),
  index = Year)

y
# A tsibble: 5 x 2 [1Y]    <--- (1y) intervalo da obs. é 1 vez no ano
# Year Observation
# <int>       <dbl>
#   1  2015         123
# 2  2016          39
# 3  2017          78
# 4  2018          52
# 5  2019         110



# xxxxxxx
# Passo 1: Avisar que temos Série Temporal --------------------------------

# Precisamos dizer que temos uma tabela de séries temporais
# index: vamos indicar qual os dados que medem a passagem temporal
# key: vamos colocar quais são as variáveis de cada serie

# Analisando uma série temporal
tabela_poluicao_ts <- as_tsibble(tabela_poluicao, 
                                 index = data)


# Analisando Varias séries temporais
# No caso de termos mais de um poluente
tabela_poluicao_mais_poluentes <- as_tibble(
  tabela_poluicao,
  index = data,
  key = poluente
)

# Todos os gráficos, quando colocamos essa tabela com várias séries temporais
# ele faz os gráficos para todas as séries


# Autoplot ----------------------------------------------------------------
# Pode colocar var. categorica, mas o gráfico não fica legal.
# O melhor, e mais comum, é colocar variáveis contínuas
# argumento .vars para designar a coluna que será usada
feasts::autoplot(tabela_poluicao_ts, .vars =  concentracao)




# xxxxxxx
# Passo 2: Entender a relação das variáveis  ------------------------------

# Objetivo: Vamos olhar se as observações adjacentes possuem alguma relação ou não.
# ou seja, se existe relação entre a variavel no tempo zero e no tempo t+1. 

# ->Se não for uma série temporal dará uma nuvem de ponto qualquer. Mas se temos
#   uma variável temporal, vamos encontrar desenhos típicos de variáveis associadas.
#   (como gráficos que indicam correlação)

# -> Então, no geral, quando manipulamos uma série temporal Xt e construímos
#    gráficos, por exemplo, de Xt-1 contra Xt, vamos identificar que existe
#    relação entre essas variáveis.

# -> chamamos os Xt-1 de variáveis defasadas ou lags.



# Vamos então fazer gráficos de lag para saber como se comportam as variáveis
# esperando que haja relação entre o Xt+1 e Xt 



# x Opção 1: gglag ----------------------------------------------------------
# gg_lag: precisamos de uma tabela de série temporal (passo 1).
# O "lags" é sempre o valor da observação anterior empurrada para 
# baixo. Vai ser essencial para investigar a dependencia. Lag 1, significa uma 
# observação vs a anterior; lag= 2, significa a observação vs antes da anterior
# e assim por diante. Conforme passa o tempo, vamos ver se vai modificando.
# O lag é uma forma de verificar a dependencia temporal das observações
tabela_poluicao_ts |> 
  feasts::gg_lag(lags = 1, y = concentracao, geom = "point") 

# Interpretação: como existe correlação entre o tempo 1 e o tempo zero, 
# significa que a observação do tempo 1 é mto parecida com o tempo zero.
# coforme distanciamos os lags a tendencia é ir perdendo esta relação

tabela_poluicao_ts |> 
  feasts::gg_lag(lags = 100, y = concentracao, geom = "point")


# mais de um lag
tabela_poluicao_ts |> 
  feasts::gg_lag(lags = 1:5, y = concentracao, geom = "point")

tabela_poluicao_ts |> 
  feasts::gg_lag(lags = 1:10, y = concentracao, geom = "point")


# x Opção 2: gráfico manual -----------------------------------------------
# Segunda opção para fazer o gráfico do lag
tabela_com_lag <- tabela_poluicao |> 
  arrange(data) |> 
  mutate(
    lag_concentracao = lag(concentracao)) |> 
      ggplot(aes(lag_concentracao, concentracao)) + 
        geom_point()

tabela_com_lag

# mais de um lag
tabela_com_lag <- tabela_poluicao |> 
  arrange(data) |> 
  mutate(
    lag_concentracao = lag(concentracao),
    lag_concentracao2 = lag (concentracao, n = 2)) |>  # add outro lag
  ggplot(aes(lag_concentracao2, concentracao)) +       # tem que mudar aqui!
  geom_point()

tabela_com_lag

#> Obs: Aqui podemos ver como funciona o lag, empurrando uma linha para baixo.
#>      a primeira linha passa a ser NA e a informação que antes estava na
#>      primeira linha passa a ser a segunda linha, e assim por diante.



# x Opção 3: correlação -----------------------------------------------------
# Podemos ao invés de somente olhar o gráfico, fazer uma correlação
# entre os dados no t e no t+1. Para verificar se existe essa correlação
# entre as variáveis.
tabela_com_lag <- tabela_poluicao |> 
  arrange(data) |> 
  mutate(
    lag_concentracao=lag(concentracao)) |> 
  drop_na()

cor(tabela_com_lag$concentracao, tabela_com_lag$lag_concentracao)




# x Opção 4: Função autocorrelação (ACF)----------------------------------------
# É praticamente o gg_lag só que ao invés de vários gráficos, você substitui
# cada grafico pela correlaçaõ entre xt e lag(xt) correspondente.
# Com essa função não precisamos fazer cada correlação individualmente

# ACF: controi a função da autocorrelação. Dá para ver se existe padrão no
# comportamento da variável. 
# Neste caso, ao fazer o gráfico com esses dados, Podemos ver q a cada 10 dias
# tem um pico, por ex.
  tabela_poluicao_ts |> 
    feasts::ACF(y=concentracao) # temos a lista de correlação entre cada lag

# Gráfico da função de autocorrelação
tabela_poluicao_ts |> 
  feasts::ACF(y=concentracao) |> 
  autoplot()

# Interpretação: As bandas azuis, quando temos resíduos de regressão apenas
# devem ficar dentro dessas bandas.Se o grafico está foram das bandas significa
# que tem algum padrão acontecendo. Os dados não podem ser analisados com métodos
# de regressão comuns


# x Opção 5: Sazonalidade ----------------------------------------------------

# Exemplo 1
# Objetivo: tentar procurar efeito sazonal
tabela_poluicao_ts |> 
  feasts::gg_season()

# Escolher o periodo (7dias)
tabela_poluicao_ts |> 
  feasts::gg_season(period = 7)


# x Opção 6: Feature ( )------------------------------------------------------
# Utizado para computar funções
tourism |>
  features(Trips, list(mean = mean)) |>
  arrange(mean)

# Outras feature
# https://otexts.com/fpp3/other-features.html
# unitroot_kpss: gives the Kwiatkowski-Phillips-Schmidt-Shin (KPSS) statistic 
# for testing if a series is stationary, and the corresponding p-value. 
tourism |>
  features(Trips, unitroot_kpss)
# unitroot_pp: gives the Phillips-Perron statistic for testing if a series 
# is non-stationary, and the corresponding p-value.

# unitroot_nsdiffs: gives the number of seasonal differences required
#                   to make a series stationary
# shift_level_max: finds the largest mean shift between two consecutive sliding
#                  windows of the time series. This is useful for finding sudden
#                  jumps or drops in a time series
# longest_flat_spot: computes the number of sections of the data where the
# series is relatively unchanging.
# guerrero: computes the optimal  λ value for a Box-Cox transformation using
# the Guerrero method 

# x Opção 7: feat_acf ( ) -------------------------------------------------
# Utilizado para computar as diferenças sazonais entre as séries. Se temos 
# uma dado mensal, podemos computar a diferença entre os Janeiros consecutivos
# Fevereiro consecutivos. Isso permite que a gnt olhe como as séries estão 
# mudando ao longo dos anos. A autocorrelação da diferença de sazonalidade
# entre as séries fornece informações úteis.
resultado <- tabela_poluicao_ts |>
  features(concentracao, feat_acf) 

tourism |> features(Trips, feat_acf)

# INTERPRETAÇÃ:
# Valores próximos a zero, sem sazonalidade
# Valores prócimos a um, forte sazonalidade

# Seasonal acf1: the autocorrelation coefficient at the first seasonal 
# lag is also returned.

# ACF1 (AutoCorrelation Function 1): ACF1 é o valor da função de autocorrelação
# para o primeiro atraso (lag) da série temporal. Ele mede a correlação entre o
# valor atual da série e o valor que ocorreu um passo de tempo anterior. 
# Valores significativamente diferentes de zero indicam dependência serial na
# série temporal.

# ACF10 (AutoCorrelation Function 10): ACF10 é o valor da função de 
# autocorrelação para o décimo atraso (lag) da série temporal. Assim como 
# ACF1, ele mede a correlação entre o valor atual da série e o valor que ocorreu
# dez passos de tempo anteriores. Valores significativamente diferentes de zero 
# indicam dependência serial em um intervalo maior.

# diff1_acf1 (ACF1 da primeira diferença): diff1_acf1 é o valor da função de
# autocorrelação para o primeiro atraso (lag) da primeira diferença da série
# temporal. A primeira diferença é calculada subtraindo cada valor da série 
# pelo seu valor anterior. O diff1_acf1 mede a correlação entre a primeira 
# diferença atual e a primeira diferença que ocorreu um passo de tempo anterior.
# Valores significativamente diferentes de zero indicam dependência serial na 
# primeira diferença da série temporal.



# x Opção 8: feat_stl ( ) -------------------------------------------------
# Função usada para mostrar a série com maior sazonalidade (pontos mais acima
# e mais a direita) e a série com maior tendencia
tourism |>
  features(Trips, feat_stl)

tourism %>%
  features(Trips, feat_stl) %>%
  ggplot(aes(x = trend_strength, y = seasonal_strength_year, col = Purpose)) +
  geom_point() +
  facet_wrap(vars(State))

# Clearly, holiday series are most seasonal which is unsurprising. 
# The strongest trends tend to be in Western Australia and Victoria. 
# The most seasonal series can also be easily identified and plotted.


# Retorna ainda: 
# seasonal_peak_year indicates the timing of the peaks — which month or 
# quarter contains the largest seasonal component.
# seasonal_trough_year indicates the timing of the troughs — which month or
# quarter contains the smallest seasonal component.



# gg_sbseries ( ) ---------------------------------------------------------
# Mostra as séries separado pela média de cada mês 

gg_subseries(tourism)



# Gráficos ----------------------------------------------------------------
library(glue)
# Computa todas as análises possíveis no feature de uma única vez
tourism_features <- tourism |>
  features(Trips, feature_set(pkgs = "feasts"))
tourism_features

# Podemos fazer gráficos par-a-par de grupos de feature
tourism_features |>
  select_at(vars(contains("season"), Purpose)) |> # todos relacionados a season
  mutate(
    seasonal_peak_year = seasonal_peak_year +
      4*(seasonal_peak_year==0),
    seasonal_trough_year = seasonal_trough_year +
      4*(seasonal_trough_year==0),
    seasonal_peak_year = glue("Q{seasonal_peak_year}"),
    seasonal_trough_year = glue("Q{seasonal_trough_year}"),
  ) |>
  GGally::ggpairs(mapping = aes(colour = Purpose))





# Exemplo: Com outros dados --------------------------------------------------------
soja <- readr::read_rds("https://raw.github.com/curso-r/main-series/main/dados/soja.rds")
#write.csv(soja, "raw_datas_exemplos/soja.csv")

# tsibble
soja_ts <- soja |> 
  mutate(data = tsibble::yearmonth(paste0(CO_ANO, "-", CO_MES))) |> 
  as_tsibble(index= data)

# autoplot
autoplot(soja_ts, .vars = KG_LIQUIDO)

# sazonalidade
soja_ts |> 
  feasts::gg_season( y = KG_LIQUIDO)
  
# função de autocorrelação
soja_ts |> 
  feasts::ACF( y = KG_LIQUIDO, lag_max = 36) |> 
  autoplot()


# O que Esperar de séries temporais fracas --------------------------------
# O que esperar quando não temos series temporais. Ou seja, o que esperar de 
# dados que não vale a pena olhar como série temporal.Encontrar ruído branco.
arrests_ts <- USArrests |> 
  mutate(data= Sys.Date()+1:n()) |> 
  as_tsibble(index = data)

# Autocorrelação
# As barras do gráfico estão dentro das faixas azuis das bandas de confiança
arrests_ts |> 
  ACF(y= Murder) |> 
  autoplot()

# Sazonalidade
# Não se vê padrão
arrests_ts |>
  gg_season(y = Murder, period = 7)

# Lag
# Não possui padrão, tudo aleatorio
arrests_ts |> 
  gg_lag(y= Murder, lags = 1:9, geom = "point")
arrests_ts |> 
  gg_lag(y= Murder, lags = 1:9)



#> Essas séries parece no que diz respeito a séries temporais,
#> com uma série do que se chama ruído branco. O ruído branco é importante
#> pois é diametralmente oposto da série temporal. 



# O que a gnt espera que seja ruido branco? --------------------------------
# O ruído branco é uma série, ou sequencia de observações, que são independentes,
# bem distribuídas, é só uma estática, uma flutuação aleatória sem relação 
# nenhuma. Portanto, o ruído branco é diametralmente oposto da série temporal.
# É o dado longitudinal (usado nas análises normais) mais puro de todos.

# A gente espera que os residuos, erros de regressao, de machine learning 
# sejam ruídos brancos. Então se fizermos esses gráficos anteriores para 
# qualquer modelo, a gnt espera ter gráficos que se assemelhem a esses do ruído
# branco. Então se os resíduos de uma regressão qualquer tem a sua função de
# autocorrelação dentro das bandas de confiança, os lags e sazonalidades não
# apresentem correlação, é o ideal. Não precisa usar séries temporais.
# Isso é esperado pois a suposição da regressão é de que os erros da regressão
# seja um ruído branco, se não for está ferindo os pressupostos para a regressão

# Então podemos usar isso para qualificar a relação entre variáveis.
# Mas também para verificar o ajuste de modelos, p.ex. Para verificar
# se tem algum padrão nos resíduos de um modelo de regressão qualquer.

# O ruído branco é encontrado em dados que não vale a pena olhar como uma série
# temporal






# x x x x x x x x x x x ----------------------------------------------------
# Explicitar padrões de dados temporais ------------------------------------

# Se colocamos dados diários, o R entende que a série é mensal, mesmo sendo
# o mesmo dia. eg. 2020-01-05; 2020-01-06 

# Então temos algumas funções para informar o tipo de série:

# Funções do tsibble para dizer a unidade da base.
# Usado para transformar as datas nestas unidades.
#                                                 
# yearmonth(): mensal
# yearquarter(): trimestral
# yearweek(): semanal

# Exemplos yearmonth ( ):
tsibble::yearmonth("1997-01")
# [1] "1997 jan"
tsibble::yearmonth("1997-01") + 1  # se somar 1, passa para o próximo mês
# [1] "1997 fev"
tsibble::yearmonth("1997-01") + 12 # se somar 12, troca de ano
# [1] "1998 jan"


# Função para montar manualmente uma coluna de tempo
#                                                   
# make_yearmonth()
# make_yearquarter()
# make_yearweek()

# Exemplo: 
tsibble::make_yearmonth(year = c(1970,1970), month = c (1,2))


# Exemplo 3:

# Dados 
soja <- readr::read_rds("https://raw.github.com/curso-r/main-series/main/dados/soja.rds")


# Série Mensal ------------------------------------------------------------
# Criando a coluna Temporal nos dados  
# Com duas opções para dizer que a série é mensal: 
# usando yearmonth () e make_yearmonth ()
soja_ts <- soja |> 
  mutate(DATA = tsibble::yearmonth(paste0(CO_ANO, "-", CO_MES)),
         DATA_jeito2 = tsibble:: make_yearmonth(CO_ANO, CO_MES)) |> 
  as_tsibble(index= DATA)



# Série Anual -------------------------------------------------------------
# Se fosse uma série anual, como informar?

# Colocando os dados anuais.
soja_ts_anual <- soja |> 
                  group_by(CO_ANO) |>                 # agrupo série mensal em ano
                  summarise(
                    KG_LIQUIDO = sum(KG_LIQUIDO)) |>  # soma/média dos valores da série para o ano
                  as_tsibble(index = CO_ANO)          # indico o ano
  
# Separa o período em anual "# A tsibble: 26 x 2 [1Y]"
soja_ts_anual
 
#### Resumo:
# Se passamos para ele uma data: entende que é diário
# Se passamos para ele yearmonth(): entende que é mensal
# Se passamos para ele número inteiro: entende que é ano


# Grafico
# Geral
autoplot(soja_ts, KG_LIQUIDO)

# gglag: correlação
soja_ts |> 
  gg_lag( y = KG_LIQUIDO)
soja_ts |> 
  gg_lag( y = KG_LIQUIDO, geom = "point")

# função de autocorrelação
soja_ts |> 
  ACF(y = KG_LIQUIDO) |> 
  autoplot()

soja_ts |> 
  ACF(y = KG_LIQUIDO, lag_max = 60) |> 
  autoplot()

# sazonalidade
soja_ts |> 
  gg_season(y = KG_LIQUIDO)




# x x x x x x x x x x x ----------------------------------------------------
# Passo 3: Padrões das Series temporais ------------------------------------
#          Normalmente, tanto para previsão quanto para descrever vamos tentar 
#          enquadrar as séries temporais de acordo com alguns padrões:

#> Sazonalidade: periodicamente acontece alguma coisa? Ex. todo dezembro sempre aumenta.
#>               todo janeiro sempre diminui.

#> Tendência: tem algo sistematicamente acontecendo aos dados? quantidade 
#>            aumenta a cada ano? A tendencia significa ter algo afetando os dados que 
#>            que sempre afeta, aumentando ou caindo. 

#> Ruído: o que sobrou



# Existe sazonalidade? ----------------------------------------------------
# Como identificar se existe Séries sazonais?
# Em geral, no gráfico do autoplot já dá para ver
# Ex:
  autoplot(soja_ts, KG_LIQUIDO)

# Caso não dê para ver no autoplot, a função de correlação também mostra
# Ex:
soja_ts |> 
  ACF(y = KG_LIQUIDO, lag_max = 60) |> 
  autoplot()

#> Atenção é importante olhar os dois gráficos. Pq as vezes podemos ter uma correlação baixa
#> entre janeiro e dezembro, por exemplo, mas ter uma correlação alta com o q
#> acontece nos mesmos meses de um ano para outro. De um ano para outro, temos o evento
#> acontecendo acrescido da tendencia dos dados e de algum grau de ruído. 


# Como estimar sazonalidade? ----------------------------------------------
# A partir do momento em que identificamos haver sazonalidade
# como vamos estimar?

# Podemos estimar usando o gg_season()
soja_ts |> 
  gg_season(y = KG_LIQUIDO)


# INTERPRETAÇÃO: Mostra os anos (cor da linha) e o que acontece em cada mês
# conseguimos observar uma tendencia, ao ver as curvas sempre aumentando 
# nos meses de março até junho.

# Mas atenção!!!!! O gg_season vai funcionar apenas nos casos que não 
# houver tendencia. Neste caso precisamos:

# 1. Tirar a tendência dos dados

# 2. Fazer uma média dos perídos que a gnt acha que tem sazonalidade (anual, trimestral)

# 3. Ai sim, podemos usar o gg_season



# Passos para Analises Temporais -------------------------------------------
# Pensar nos componentes da série (tendencia/sazonalidade) ajudam bastante
# a pensar em como melhorar as previsões. 

# 1. Extrair/Estimar tendências 
#    Tendencia é o movimento oculto nos dados, seguindo uma direção crescente,
#    decrescente ou estacionária

# 2. Extrair/Estimar sazonalidades
#    Flutuações regulares dentro de um período completo de tempo. Representam
#    um tipo de padrão que se repete (picos, depressão) normalmente dentro de 
#    um ano

# 3. Extrair/Estimar ruído

# 4. Depois de fazer essa decomposição (que pode ser da maneira clássica ou 
# usando o STL, que é mais evoluído, calcular autocorrelação dos resíduos)

# Esses passos são tão comuns, que o processo é chamado de decomposição. Quando
# tiramos a tendencia, temos a serie normal. Depois faremos uma média de acordo
# com o período que acreditamos haver sazonalidade e verificamos (estimamos) a 
# sazonalidade.

# OBS: Isso pode ser usado para análise descritiva ou, as vezes, essa decomposição 
# já é boa o suficiente para se fazer um modelo.


# Estimar Tendência -------------------------------------------------------
# A tendencia é o movimento oculto nos dados, seguindo uma direção crescente,
# decrescente ou estacionária

#> Médias Móveis: é uma forma comum de estimar a tendência de uma série temporal
#  retirando variações periódicas que podem não importar. A média móvel é a 
#  média de alguma coisa em um dado período de tempo. A medida que novos dados 
#  são acrescidos, ela vai sendo recalculada. A MÉDIA MÓVEL SUAVIZA VARIAÇÕES.
#  Então, consegue captar a tendencia geral dos dados, ao inves de pegar as 
#  variações pontuais (picos pontuais). Por exemplo, pegamos a média dos últimos
#  7 dias, ou dos ultimos 3 meses.

# sintetiza a tendencia da série temporal

# Médias Móveis -----------------------------------------------------------
# Usado para estimar tendencias

# Função: recebe um vetor qualquer e aplica uma função em janelas (slide_dbl)
# Calcula várias médias em janelas de 6 meses para tras e para frente, neste 
# caso. 
# .complete: o que fazer nos casos que não der para aplicar a janela. Devo
#           ou não aplicar a uma janela faltando.TRUE: Roda somente com a 
#           janela completa. FALSE: roda com a janela incompleta.

# Neste caso: fazer a média móvel de 6 observações antes e nenhuma depois

# dados
x <- 1:100

# função: média móvel
x |>  
  slider::slide_dbl(
    .before = 6,
    .after = 0,
    mean,
    .complete = TRUE
  )

# Funcionamento da função: o primeiro valor, que podemos tomar a média
# de 6 observaçoes anteriores. Neste caso, a posição 7. 

# Para a sequencia
x <- 1:100
# então na posição 7, primeiro número q temos 6 observações anteriores
# ele primeiro faz a média móvel das observações: 
mean(1:7)
# no próximo número da sequencia (8) ele faz:
mean(2:8)
# no próximo número da sequencia (9) ele faz:
mean(3:9)
# e assim por diante ...

# Se não quiser incluir o próprio número na média podemos
# colocar .after = -1
x |>  
  slider::slide_dbl(
    .before = 6,
    .after = -1,
    mean,
    .complete = TRUE
  )
# Para a sequencia
x <- 1:100
# então no número 7, primeiro número q temos 6 observações anteriores
# ele primeiro faz a média móvel das observações: 
mean(1:6)

# no próximo número da sequencia (8) ele faz:
mean(2:7)

# assim por diante...

# Podemos fazer também de um período para frente e para tras. 
# média movel das 6 obs. para tras e 6 obs para frente.

x |>  
  slider::slide_dbl(
    .before = 6,
    .after = 6,
    mean,
    complete = TRUE
  )


# COM DADOS 
# Calculo da Média Móvel e Gráfico
soja_ts |> 
  mutate(
    MM = slider::slide_dbl(KG_LIQUIDO,         # calculando a média movel
                           .before = 12,
                           .after = -1,
                           mean,
                           complete = TRUE)) |> 
  autoplot(KG_LIQUIDO)+                       # calculando a serie 
  #ggplot(aes(x= DATA))+
  geom_line(aes(y= MM), color = "red")        # plotando a média movel da serie


# Para a tendencia o ideal seria considerar 6 meses antes e 6 meses depois
soja_ts |> 
  mutate(
    MM = slider::slide_dbl(KG_LIQUIDO,
                           .before = 6,
                           .after = 6,
                           mean,
                           complete = TRUE)) |> 
  autoplot(KG_LIQUIDO)+
  #ggplot(aes(x= DATA))+
  geom_line(aes(y=MM), color = "red")


# Para remover a tendencia da serie temporal, podemos diminuir as 
# médias móveis extraídas dos dados (pois a média móvel é a tendencia dos 
# dados, ao diminuir dos dados, estamos automaticamente descontando a tendencia
# da série)
# Exemplo:
soja_ts |> 
  mutate(
    MM = slider::slide_dbl(KG_LIQUIDO,
                           .before = 6,
                           .after = 6,
                           mean,
                           complete = TRUE),
    KG_LIQUIDO_sem_tend = KG_LIQUIDO - MM) |> 
  autoplot(KG_LIQUIDO_sem_tend)
  
#> INTERPRETAÇÃO: Aqui temos apenas a variação nos dados sem a
#> tendencia que havia de aumento


# O que acontece com o ACF?
soja_ts |> 
  mutate(
    MM = slider::slide_dbl(KG_LIQUIDO,
                           .before = 6,
                           .after = 6,
                           mean,
                           complete = TRUE),
    KG_LIQUIDO_sem_tend = KG_LIQUIDO - MM) |> 
  ACF(Y = KG_LIQUIDO_sem_tend, lag_max = 50) |> 
  autoplot()

#> INTERPRETAÇÃO: O comportamento do ACF, que mostra a sazonalidade
#> nao vai mudar em função de retirada da tendencia. Só que ela muda um pouco,
#> pq essa função acaba mostrando, dando indicios da tendencia, antes tinhamos
#> o mesmo padrão se repetindo (sazonalidade) só que esse padrão apresentava
#> valores proporcionalmente cada vez menores (tendencia). Essa parte já não 
#> conseguimos mais ver.


# Serie de Tempo Crescente ------------------------------------------------
x <- 1:100 + rnorm (length(x), sd = 3)
serie_de_tempo_crescente <- tsibble(
  tibble(X= x, tempo = 1:100), index= tempo)

autoplot(serie_de_tempo_crescente)
ACF(serie_de_tempo_crescente) |> 
  autoplot()

#> INTERPRETAÇÃO: é uma série com tendencia diminuindo. Sempre que a série
#> tiver tendencia, na função ACF ela fica como uma escadinha. Então quando
#> tiramos a tendencia, nós estamos tirando esta escadinha, e sobra apenas 
#> variação (sazonalidade e aleatoriedade). Se ao tirar a tendencia não 
#> sobrar mais nada, significa que eu não tenho mais sazonalidade, somente a
#> tendencia mesmo


# Mais de uma Média Móvel -------------------------------------------------
# Podemos fazer a média móvel, da média móvel, e assim suavizamos ainda mais
# a linha de tendência. 
soja_ts |> 
  mutate( 
    MM1 = KG_LIQUIDO |>
      slider::slide_dbl(.before = 5,.after = 6,
                        mean,
                        complete = TRUE),
    MM = MM1 |>
      slider::slide_dbl(.before = 6,.after = 0,
                        mean,
                        complete = TRUE)
      ) |> 
  autoplot(KG_LIQUIDO)+
  geom_line(aes(y=MM), color = "red")



# Série Estacionária ------------------------------------------------------
# Série estacionária é quando a série não varia muito. Quando a série
# fica ao redor do zero.


# Decomposição Clássica ---------------------------------------------------
# 1. Fazemos a média móvel para estimar a tendência. 
# 2. Fazemos o valor da série menos a tendência. 
# 3. Agora calculamos a sazonalidade fazendo a média dos valores para cada 
# período.
# 4. Calculamos o resto.


decomposicao <- soja_ts |> 
  # 1. estimar a tendencia a partir da média móvel
  mutate(tendencia = KG_LIQUIDO |> 
           slider::slide_dbl(.before = 5, .afeter = 6, mean, .complete = T) |>
           slider::slide_dbl(.before = 3, .afeter = 0, mean, .complete = T),
         # 2. jogar a tendencia fora
         estacionaria= KG_LIQUIDO - tendencia
           ) |> 
# sobrou uma série que fica ao redor do zero (s/tendendia). série estacionarias
  group_by(CO_MES) |> 
  mutate(
    # 3.estimar sazonalidade (média por mês)
    sazonalidade = mean(estacionaria, na.rm=T),
    # tirar tendencia (estacionaria) e a sazonalidade
    residuo = estacionaria - sazonalidade
  ) |> 
  ungroup()
decomposicao


# Fazer os gráficos manualmente
decomposicao |> 
  select(DATA,KG_LIQUIDO, tendencia, sazonalidade, residuo) |> 
  pivot_longer(
    cols= c(KG_LIQUIDO, tendencia, sazonalidade, residuo),
    names_to = "serie",
    values_to = "valor"
  ) |> 
  ggplot(aes(x=as.Date(DATA), y = valor))+
  geom_line()+
  facet_wrap(ncol= 1, ~serie, scales = "free")

# autocorrelação do resíduo 
decomposicao |> 
  ACF(y=residuo) |> 
  autoplot()



# Fazer os gráficos Automático
# Decompoe melhor para tirar a autocorrelação do resíduo.
# Seria bom pouca autocorrelação nos residuos. Se ainda existe autocorrelação
# quer dizer que ainda existem componentes temporais que o nosso modelo não
# consegue capturar.
# type: forma que está estimando "multiplicative" ou "aditive"
soja_ts |> 
  model(classical_decomposition(KG_LIQUIDO, type = "multiplicative")) |> 
  components() |> 
  autoplot()

# Essa serie não está boa. pq ainda existe padrão nos resíduos (random).
# ainda existe autocorrelação. Olha o ACF do residuo:
soja_ts |> 
  model(classical_decomposition(KG_LIQUIDO, type = "multiplicative")) |> 
  components() |> 
  ACF(random) |>  # autocorrelação dos resíduos ainda está grande
  autoplot()


# Decomposição STL --------------------------------------------------------
# Um jeito de analisar séries temporais, inclusive de produzir previsões é
# aplicar o que se chama normalmente de decomposição STL (Seasonal Trend Loess),
# que tenta ajustar funções muito flexíveis na equação abaixo:

# X(t) = S(t) + T(t) + E(t).
# Aqui teremos sendo um componente sazonal periódica, é algo
# que aumenta ao longo do tempo

# Vantagens com relação aos outros métodos:
#  - Sazonalidade pode mudar ao longo do tempo
#  - Funciona com sazonalidades mais complexas (eg, nao precisa ser mensal, anual, etc.)

# Essa é mais interessante para usar. 

stl_componets <- soja_ts |> 
  model(
    STL(KG_LIQUIDO ~ season(12)+ trend())
  ) |> 
components()

autoplot(stl_componets)

# Olhando a sazonalidade desta série, percebemos que a partir de jan 2010 a
# sazonalidade aumenta muito. Ou seja, a sazonalidade é mais diferente no final
# do que no começo da série.

# agora fazendo o gráfico de autocorrelação dos resíduos (neste caso chamado de remainder),
# ainda não está ótimo, mas já melhorou um pouco. Esta quase dentro das bandas
# azuis.
stl_componets |> 
  ACF(remainder) |> 
  autoplot()


# CONCLUINDO: Tudo que foi feito até aqui foi para uma análise descritiva 
# da série temporal. O modelo que fizemos manualmente (média móvel) é o mesmo
# que a decomposição clássica faz. O STL é um pouco mais elaborado. Usa uma
# média diferente da média móvel, e funciona um pouco melhor.

# A medida que tiramos tendencia e sazonalidade, esperamos ter um resíduo
# aleatório, sem autocorrelação e sem padrão. Mas as vezes não conseguimos.
# como no exemplo. E nesse caso, teremos que fazer outros modelos para ir 
# cada vez mais minimizando a autocorrelação do resíduo e tornando ele 
# aleatório, ou como conhecido, resíduo branco.

# Em qualquer modelo, esperamos que os erros sejam ruídos brancos. 


# x-------------------------------------------------------------------------
# x x x x x x x x x x x -------------------------------------------------------
# Previsão (= Forecasting) ----------------------------------------------------
# Após explorar os dados e descreve-los com os métodos anteriores. Podemos 
# pensar nas previões. O STL é usado apenas para explorar tendencia e 
# sazonalidades, para previsões, usaremos outros modelos.

# Temos alguns tipos de modelos:
# Simples: Naive (= modelo ingenuo) | Mean | Drift
# Clássico: Suavização Exponencial | ARIMA
# Outros: Regressões | Redes Neurais


# Criar previsões é difícil e, as vezes, impossível. Portanto, temos que 
# pensar em algumas coisas antes de qualquer modelo:

# 1. O que faz o que está sendo medido se modificar?
# 2. Temos dados disponíveis?
# 3. Quanto o passado é parecido com o futuro?
# 4. O quanto prever impacta o futuro?
# 5. Definir o horizonte da previsão. Quanto tempo para frente quero prever?
# 6. Definir a granularidade da previsão? Quero prever mensal? diário?


# Criar previsões é um processo cíclico. Isto é vamos aprendendo sobre a 
# base de dados conforme visualizamos os resultados

# Tratamento de dados de séries temporais as vezes é trabalhoso. Porque as vezes
# falta datas, e não pode ter data faltando em alguns programas, então neste caso
# tem que completar. Pode ser com zero, ou as vezes com alguma média




# Tidy: Arrumação dos dados -----------------------------------------------

#> 1. Transformação de escala: para muitos modelos normalizar as variáveis 
#>    ajuda na estimação.Tirar o log é muito comum.
#> 2. Inputação de missing: as vezes a base possui dados faltantes. Existem 
#>    técnicas para imputar esses valores de forma que façam sentido para
#>    séries. Direto as séries são furadas, então é imprescindível para o modelo.
#> 3. Correção de outlier: as vezes uma observação é um missing mas, ela é um valor
#>    muito diferente da série. Pode ser um erro na medida, ou pode ter sido causada
#>    por um evento muito raro (dependendo do caso faz sentido corrigir)



# Tamanho amostra ---------------------------------------------------------
# O ideal é que se tenha pelo menos 2 repetições sazonais. Então para uma 
# série mensal. Ideal que se tenha no minimo dois anos, para termos 2 medidas
# de cada mês. Esse seria o mínimo. Mas, em geral, séries temporais, sempre
# temos poucos dados



# Backtesting: Avaliação ---------------------------------------------------
#> É o que diferencia as previsões das séries temporais. É obrigatório para a
#> série temporal. É a melhor forma de validar o modelo que construímos.
#> Significa treinar o modelo com a série mais antiga e testar com dados mais 
#> próximos do presente. Podemos até fazer validação cruzada de outra forma
#> mas essa é obrigatória mesmo que outra forma seja utilizada.

#> - O backtesting ajuda a estimar o erro de previsão de uma forma mais segura
#> do que somente analizando os resíduos. 
#> - É o equivalente a validação cruzada para as séries temporais.
#> - Ajuda a evitar o overfitting. Pois garante que estamos usando só o 
#>   passado para estimar o futuro;
#> - É importante escolher o período de teste de forma que fique parecido
#    com o que pretendemos prever. Ex: não adianta testar o modelo em 2
#    meses se quando for usá-lo estivermos pegando previsão para 4
#    meses.
#  - Note que em séries temporais, mesmo com backtesting é mais fácil de
#    ter overfitting do que em problemas clássicos de ML. Principalmente
#    quando não entendemos os fatores que influenciam a série.




# Time series cross-validation --------------------------------------------
#> cortamos a série em algum ponto e dizemos "daqui para tras é treino e 
#> daqui para frente é teste". Não podemos fazer sorteio. 

#> Além de separar os dados do final, podemos ir fazendo vários pedaços finais,
#> usando varias janelas diferente, cada vez um período um pouco maior.

#> Na hora de ajustar o modelo, é uma boa fixar uma quantidade de ano (periodo)
#> para teste e treino. Para poder comparar os diferentes modelos. Pq senão,
#> um modelo pode ficar melhor, simplesmente pq usa um conjunto maior de dados.

#> Equivale a validação cruzada. Ajuda a evitar o overfiting; Em séries temporais
#> é muito mais fácil ter overfiting do que os problemas de Machine Learning normal
#> principalmente quando não entendemos os fatores que influenciam a série



# x -----------------------------------------------------------------------
# Pacotes de Modelagem ----------------------------------------------------
library(timetk)
library(modeltime)


# Dados -------------------------------------------------------------------
cetesb_pinheiros_co <- readr::read_rds("https://github.com/curso-r/main-series/raw/main/dados/cetesb_pinheiros_diario_co.rds")
#write.csv(cetesb_pinheiros_co, "raw_datas_exemplos/cetesb_pinheiros_co.csv")


# Introdução timetk/modeltime --------------------------------------------------
# Gráficos para explorar os dados. São interativos.

# Descrever: plotar série temporal
cetesb_pinheiros_co %>%
  plot_time_series(data, concentracao)

# Descrever: boxplot em função do período escolhido
cetesb_pinheiros_co %>%
  plot_time_series_boxplot(data, concentracao, .period = "1 months")

cetesb_pinheiros_co %>%
  plot_time_series_boxplot(data, concentracao, .period = "6 months")

cetesb_pinheiros_co %>%
  plot_time_series_boxplot(data, concentracao, .period = "1 year")

# Sazonalidade: versao do ggseason ()
cetesb_pinheiros_co %>%
  plot_seasonal_diagnostics(data, concentracao)

# ACF: função de autocorrelação
cetesb_pinheiros_co %>%
  plot_acf_diagnostics(data, concentracao)

# STL: Decomposição Não funciona, pois temos missing. Precisamos preencher os
# missing values antes
cetesb_pinheiros_co %>%
  plot_stl_diagnostics(data, concentracao)



# Tidy --------------------------------------------------------------------
# Arrumação dos dados. Algumas funções precisarão do preenchimento dos missing
# para funcionamento.
library(tidyverse)

# Base apresenta Missing implicito:


# Opção 1: Remover Missing ------------------------------------------------
# 1. Podemos jogar fora os missings. 
# Mas acaba ficando uma linha reta no gráfico q não é exatamente o que queremos. 

# Filtrar os missings
cetesb_pinheiros_co_implicito <- cetesb_pinheiros_co |> 
  filter(!is.na(concentracao))
# Consegue fazer o gráfico de STL (decomposição)
cetesb_pinheiros_co_implicito |> 
  plot_stl_diagnostics(data, concentracao)

# Não nos mostra mais nenhum missing
sum(is.na(cetesb_pinheiros_co_implicito$concentracao))




# Opção 2: Missing Explicitos ---------------------------------------------
# 2. Podemos transformar os missing implícitos em missing explícitos.
# O que queremos é fazer uma inputação



# x Pular Datas com Missing -------------------------------------------------
# função pad_by_time ( ): pula as datas com missings, então ficamos com buracos
# nos gráficos
cetesb_pinheiros_co_implicito <- cetesb_pinheiros_co_implicito |> 
                                  timetk::pad_by_time(data, .by = "day")
                                  sum(is.na(cetesb_pinheiros_co_implicito$concentracao))

cetesb_pinheiros_co_implicito |> 
  plot_time_series(data, concentracao)



# x Imputar Missing com Modelos ---------------------------------------------
# função ts_impute_vec ( ): faz inputações com base em modelos
# Essa base possui observações com missing
summary(cetesb_pinheiros_co$concentracao)


# period = 1,lambda=1: faz uma média entre a observação anterior e a próxima
# period = 1,lambda=NULL: faz inputação com base em uma interpolação linear entre
#                         os valores adjacentes. Acaba ligando um ponto no outro.
# period = 1, lambda = 2: faz uma transformação de box cox. Faz um polinomio.
# period = 365: faz um ajuste sazonal. Interpola os resíduos (Melhor Imputação!)
#               poderia colocar 30 dias para ficar sazonalidade mensal.
# concentração repetida: também é uma ótima opção (últimas duas linhas)

# Aqui temos a comparação entre essas opções:
imputacoes <- cetesb_pinheiros_co %>%
  mutate(
    concentracao_linear = ts_impute_vec(concentracao, period = 1, lambda = NULL),
    # tem só um pouquinho de dataleak
    concentracao_boxcox = ts_impute_vec(concentracao, period = 1, lambda = 2),
    # tem só um pouquinho de dataleak
    concentracao_sazonal = ts_impute_vec(concentracao, period = 365),
    # tem dataleak: essas duas ultimas linhas, troca os NAs pelo último valor
    concentracao_repetida = concentracao
  ) |>
  tidyr::fill(concentracao_repetida) # tem zero dataleak



imputacoes %>%
  select(data, starts_with("concentracao")) %>%
  pivot_longer(
    cols = starts_with("concentracao"),
    names_to = "imputacao",
    values_to = "value"
  ) %>%
  plot_time_series(data, value, .facet_var = imputacao)

# inputacao final: depois de olhar as opções, escolhemos uma forma de inputação
# e salvamos na base que será usada para o modelo

cetesb_pinheiros_co <- cetesb_pinheiros_co %>%
  mutate(
    concentracao = ts_impute_vec(concentracao, period = 365)
  )



# x -------------------------------------------------------------------------
# Modelagem ---------------------------------------------------------------

# Antes de começar um modelo muito complexo é comum utilizarmos modelos
# mais básicos possíveis. Para usar modelos complexos, temos que ganhar muito 
# em poder preditivo, senão não vale a pena.



# Algoritmos Simples: -----------------------------------------------------
# Dois algoritmos simples:

# Média Móvel (Modelo Inicial): fazer o mais simples possível. Utilizar a média 
#                               móvel dos últimos "m" períodos e utilizarmos como 
#                               valor predito.

# Modelo Ingênuo (Naive): a predição é uma repetição do último valor observado, 
#                         ou, se for sazonal, do ultimo periodo.



# Passo 1: Backtesting (Treino vs Teste) ----------------------------------

### Primeiro temos que Separar a Base de Treino e Teste (Backtesting)

# Pacotes
library(tidymodels)
library(timetk)


# Passo 1 xxxxxxx - Separar os dados Teste para guardar
# função time_series_split ():
# Usamos essa função para deixar uma parte dos dados de fora para testar no 
# final da modelagem.

# initial: quantidade de tempo que será usado para ajustar o modelo
# assess: quantidade de tempo usada para testar o modelo
# skip = "1 year": quero que pule 1 ano.
# cumulative = T: sempre usa o periodo inteiro para construir a base treino
# cumulative = F: usa somente o ano que foi pedido mesmo 

# Ex: Esse exemplo seria bom para séries de diárias, para séries diferentes
#     veremos outros ajustes adiante.
  split <- time_series_split(
  cetesb_pinheiros_co,
  data,
  initial = "40 months",   # usado para ajustar
  assess = "1 month"       # usado para testar
)

# Gráfico: visualização do que foi usado para teste e treino
split %>%                        # como ficou a divisão
  tk_time_series_cv_plan() %>%   # transforma o split em uma tabela
  plot_time_series_cv_plan(data, concentracao)  # plot da tabela



# Passo 2 xxxxxxx - Fazer slices com os dados treinos (backtest em si)
# Função time_series_cv ( ): 
# Depois que já separamos os dados finais de teste (usando time_series_split), 
# ficamos com o conjunto de treino.
# Agora aqui vamos separar varios slices de teste e treino para
# calcular os índices internos nesses dados de treino

# Efetivamente gerar as bases de treino e teste
# cumulative= F : significa que sempre usa 2 anos e 2 meses para a avaliação
backtest <- time_series_cv(
  training(split),
  data,
  cumulative = FALSE,
  initial = "24 months",  # usado para ajustar
  assess = "3 month",     # usado para testar
  skip = "2 month",       # de 2 em 2 meses
  slice_limit = 10        # separar base para deixar 1 teste (1 para teste e 9 para treino)
)

split
backtest
plot_time_series_cv_plan(backtest, data, concentracao)




# Passo 2: Ajustar Modelos ------------------------------------------------

# Ajustando o modelo ------------------------------------------------------
# Pacotes para ajuste -----------------------------------------------------
library(tidymodels)
library(modeltime.resample)



# Definição do Modelo Inicial ---------------------------------------------
# parecido com a sintaxe do parsnip/tidymodels
# linear_reg() %>%
#   set_mode("regression") %>%
#   set_engine("glmnet")

?modeltime::naive_reg

#
# Modelo 1: Especificação do Modelo Mais simples de Todos( =faz apenas uma média)
model_spec <- modeltime::naive_reg() %>%
                          set_engine("naive")

# Ajuste do Modelo 1
# lm(y ~ x1 + x2 + x3)
fitted <- model_spec %>%
            fit(concentracao ~ data, training(split))

##
# Modelo 2: Especificação do Modelo Mais simples com Sazonalidade de 1 ano
modelo_naive_com_sazonalidade <- modeltime::naive_reg(seasonal_period = "1 year") %>%
                                              set_engine("snaive")

# Ajuste do Modelo 2:
fitted2 <- modelo_naive_com_sazonalidade %>%
             fit(concentracao ~ data, training(split))

##
# Modelo 3: Especificação do Modelo Mais simples com Sazonalidade de 1 mês
modelo_naive_com_sazonalidade_mensal <- modeltime::naive_reg(seasonal_period = "1 month") %>%
  set_engine("snaive")

# Ajuste do Modelo 3:
fitted3 <- modelo_naive_com_sazonalidade_mensal %>%
               fit(concentracao ~ data, training(split))


# Tabela de Modelos: modeltime_table ( )--------------------------------------
# Cria tabela para Comparar o desempenho dos modelos

models_tbl <- modeltime_table(
  fitted,
  fitted2,
  fitted3
)



# ReAjustar o Modelo ------------------------------------------------------
# Re ajustar o modelo para as amostras do backtesting
# Usar a tabela com a especificação de todos os modelos para ajustar aos dados
library(modeltime.resample)
resamples_fitted <- models_tbl  %>%
                      modeltime_fit_resamples(
                        resamples = backtest,
                        control   = control_resamples(verbose = FALSE))


# Visualizar os Resultados ------------------------------------------------
# Plotar os resultados para comparar os erros de cada modelo
# metricas do pacote yardstick.
resamples_fitted %>%
  plot_modeltime_resamples(.metric_set = metric_set(rmse))

# Interpretação: Quanto mais para a direita a linha média do erro do modelo,
# mais aquele modelo errou. Neste caso, o modelo 2 errou bem mais do que os
# outros dois modelos. E o Modelo 1 e o Modelo 3 tiveram o mesmo desempenho
# Ou seja, isso é uma sugestão de que não vale a pena deixar o modelo mais
# complexo. Já que o modelo mais simples (1_NAIVE) tem menos erro

# Visualizar a tabela com a média dos erros.
resamples_fitted %>%
  modeltime_resample_accuracy(
    summary_fns = mean,
    metric_set = metric_set(rmse)
  )

# Métricas ----------------------------------------------------------------
# Quanto menor o valor das métricas, melhor o modelo

# As principais métricas são:
# ME - Mean Error - é a média da diferença entre realizado e previsto

# MAE - Mean absolute error: é a média da diferença absoluta entre realizado 
#       e previsto. Não é comparável entre séries.

# RMSE - Root mean squared error: é o desvio padrão total da amostra da
#        diferença entre o previsto e o realizado. Não é comparável entre séries.

# MPE - Mean Percentage Error: é a diferença percentual do erro 

# RSQ - R-squared: correlação ao quadrado entre predito e observado.

# MAPE - Mean absolute percentage error: é a diferença absoluta percentual do
#        erro. Comparável entre séries, porém assimétrica (prever a mais ou 
#        a menos tem valores diferentes)

# SMAPE - Symmetric mean absolute percentage error: tenta corrigir a assimetria
#         do MAPE mas,não funciona muito bem.

# MASE - Mean absolute scaled error: Mais complicado de calcular. Pode ser 
#        comparada entre   séries e é simétrica.

# TIC - Theil Inequality Coefficient (Theil's U): é o grau de ajuste da previsão
#       quanto menor, melhor. Ideal seria obter zero.

# ACF1 - Firt Order Autocorrelation function: autocorrelação dos resíduos



# Previsões com a Base de Teste -------------------------------------------
# Após selecionar um modelo, pegamos a base de teste e geramos as previsões
# para ela.
calibration_tbl <- models_tbl %>%
                    modeltime_calibrate(new_data = testing(split)) #faz a previsão no teste

# Aqui temos a previsão calculada efetivamente
forecasts <- calibration_tbl %>%
                modeltime_forecast(
                  new_data = testing(split),
                  actual_data = cetesb_pinheiros_co)

# Computar os erros
calibration_tbl %>%
  modeltime_accuracy()

# Visualizar as previsões dos modelos
plot_modeltime_forecast(forecasts)

# Interpretação: Temos os modelos e temos os dados reais
# e podemos ver o modelo que mais se aproxima dos dados
# reais. Que faz a melhor previsão.


# ReAjuste do Modelo com a Base toda ---------------------------------------------------
# Agora vamos refitar o modelo com a base toda

refit_tbl <- calibration_tbl %>%
              modeltime_refit(data = cetesb_pinheiros_co)


# Previsão para Futuro ---------------------------------------------------------
forecast_futuro <- refit_tbl %>%
                    modeltime_forecast(h = "1 month", actual_data = cetesb_pinheiros_co)
forecast_futuro %>%
                plot_modeltime_forecast()



# Revisando os Passos: ----------------------------------------------------
#> 1. Modelar
#> 2. Escrever as especificações
#> 3. Fita
#> 4. Cria a modeltime_table()
#> 5. modeltime_table() |> modeltime_fit_resamples()
#> 6. modeltime_table() |> modeltime_fit_resamples() |> modeltime_resample_accuracy()
# Já na base final, após escolha de modelo:
#> 7. modeltime_table() |> modeltime_calibrate() |> modeltime_forecast()


# Neste pacote modeltime:: teremos vários tipos diferentes de regressão
# que podemos fazer



# x -----------------------------------------------------------------------
# x -----------------------------------------------------------------------
# x -----------------------------------------------------------------------


# Tipos de Modelos: -------------------------------------------------------


#º1. Modelo: Regressão Linear --------------------------------------------------------

#> Podemos usar modelos de regressão linear para fazer previsões de séries
#> temporais. Neste cso, ao adicionamos variáveis preditoras que ajudam a
#> controlar o efeito temporal - sazonlidade e tendencia. 
#> Por exemplo:
#> - categorias indicando o mes, dia da semana, etc
#> - variáveis numericas que indicam a tendencia (tempo desde o inicio)
#> - outras flags que possam ajudar a explicar períodos atipicos ou 
#>   mudanças de tendencias
#> Devemos tomar muito cuidado com variáveis que precisam ser conhecidas
#> no período da previsão.

#> As variáveis categóricas em regressão precisam ser dummies. Quando 
#> modelamos com lm, ele já coloca automaticamente as categoricas em dummies.

#> Quando vamos começas a modelar, podemos fazer um modelo simples, naive e 
#> naive com sazonalidade, ou mesmo um modelo linear simples

# Dados -------------------------------------------------------------------
anac <- 
  readr::read_rds("https://github.com/curso-r/main-series/blob/main/dados/anac-sp.rds?raw=true") %>%
  mutate(DATA_ym = tsibble::yearmonth(paste(ANO, MES, sep = "-"))) %>%
  mutate(
    TEMPO_DESDE_INICIO = difftime(
      DATA,
      lubridate::ymd("1999-12-01"),
      units = "days"
    )/30,
    LAG_1 = lag(PASSAGEIROS_PAGOS, 1, default = 0) # criou uma coluna de lag
  )
# write.csv(anac, "raw_datas_exemplos/anac.csv")


# Visualizar Base ---------------------------------------------------------

# Grafico Interativo com pacote timetk:
# Melhor para explorar os dados
anac %>%
  plot_time_series(DATA, PASSAGEIROS_PAGOS)

# Gráfico Estático ggplot
# Melhor para relatórios
anac_ts <- anac %>%
            as_tsibble(index = DATA_ym) |>
            select(PASSAGEIROS_PAGOS, everything())
autoplot(anac_ts)


# Decomposição STL:  ------------------------------------------------------

stl_components <- anac_ts %>%
  model(
    STL(PASSAGEIROS_PAGOS ~ season(12) + trend())
  ) %>%
  components()
autoplot(stl_components)

# Sazonalidade ------------------------------------------------------------
stl_components %>%
  ACF(remainder) %>%
  autoplot()
gg_season(anac_ts)


# Autocorrelação da Série -------------------------------------------------
anac_ts |> 
  ACF() |> 
  autoplot()

# Interpretação: esse desenho em escada nos diz que temos uma tendencia 
# forte acontecendo. Mas podemos notar que temos uma queda, seguida de 
# um aumento leve e novamente queda. Isso faz com que a gnt analise tb
# a diferença da variável, ou seja, quanto que varia de um passo para o
# outro.

anac_ts |> 
  ACF(diff(PASSAGEIROS_PAGOS), lag_max = 48) |> 
  autoplot()

# Interpretação: a gnt consegue observar que de 12 em 12 meses temos 
# um pico, ou seja, que os janeiros de diferentes anos são parecidos
# com picos. A diferença entre os meses é sempre parecida nesse 
# período de alta.

# Ou seja, com a diferença podemos observar um padrão sazonal que 
# não conseguimos só com a autocorrelação, pq tb leva em conta a 
# tendencia. Então podemos fazer a autocorrelação da diferença para
# achar esses padrões, ou podemos tirar a tendencia e fazer a autoco
# relação, igual fizemos na outra seção.





# Modelo Regressão ---------------------------------------------------------


# REGRESSÃO: A regressão pode ser usada para fazer um modelo ingenuo lgal.
# Mas deve ser usada com cuidado pois ignora a parte temporal. Apesar de 
# as vezes produzir modelos com boas estimativas de erro. Pode está roubando
# nas estimativas. Como no caso do modelo 6. Que ao usar o LAG de passageiros
# usa os dados do futuro para prever o futuro, então rouba para obter
# um valor de erro mais baixo que os demais.

# Validação Cruzada SIMPLES -----------------------------------------------
# Aqui não estamos fazendo uma validação cruzada tão robusta quanto fizemos anteriormente
# Antes estavamos dizendo para "vai andando 12 meses para frente toda vez
# Usavamos o backtest com slices temporais.
# Aqui, estamos usando um inial de 20 anos, que pega quase toda a base, 
# e estamos usando apenas os últimos 12 meses para testar.

# Ajusta o modelo em 20 anos e testa nos últimos 12 meses
# Separar Treino e Teste
split <- time_series_split(
  anac,
  DATA,
  initial = "20 years",  # treinar o modelo
  assess = "12 month"    # testar o modelo
)

# Visualizar treino e teste
plot_time_series_cv_plan(
  tk_time_series_cv_plan(split),
  DATA, PASSAGEIROS_PAGOS)



# Definição do Modelo Inicial -----------------------------------------------
# Vamos fazer uma regressão: assim será mais fácil tratar o caso "outlier"
# que foi o ano de 2020 (pandemia) na quantidade de viagens.
# diferente da abordagem anterior que fizemos primeiro uma regressão naive
# que era a mais simples possível
model_spec <- parsnip::linear_reg() %>%
                set_engine("lm")


# Ajuste dos Modelos ------------------------------------------------------

# Modelo 1: o ano e o mês como covariavel
modelo1 <- model_spec |>
  fit(PASSAGEIROS_PAGOS ~ ANO + as.factor(MES), training(split))

# Modelo 2: apenas o mês como covariável
modelo2 <- model_spec |>
  fit(PASSAGEIROS_PAGOS ~ as.factor(MES), training(split))

# Modelo 3: colocar uma variável indicadora se o ano é 2020 
# para lidar com o problema do comportamento diferente só nesse ano
# Estratégia razoável para lidar com eventos bizarros
modelo3 <- model_spec |>
  fit(PASSAGEIROS_PAGOS ~ ANO+ I(ANO== 2020) + as.factor(MES), training(split))

# Modelo 4: Podemos testar interações também. Uma reta antes de 2020 e outra
# depois de 2020
modelo4 <- model_spec |>
  fit(PASSAGEIROS_PAGOS ~ ANO*I(ANO== 2020) + as.factor(MES), training(split))


# Modelo 5: estimar um beta para antes da pandemia e outro para depois
modelo5 <- model_spec |>
  fit(PASSAGEIROS_PAGOS ~ ANO*I(DATA<= as.Date("2020-04-01")) + as.factor(MES),
      training(split))


# Modeo 6: usar o LAG 
# Esse é um bom modelo naive. Ele só desloca a serie para a frente
# E também ignora que estamos trabalhando com séries temporais, 
# consegue apenas prever um mês para frente. é um modelo que não é 
# honesto. Ele anda para frente a série para conseguir novos numeros
# de previsão
modelo6 <- model_spec |>
  fit(PASSAGEIROS_PAGOS ~ LAG_1 + as.factor(MES), training(split))

# Tabela com os modelos: 1 e 2
models_tbl <- modeltime_table(
  modelo1,
  modelo2,
  modelo3, 
  modelo4, 
  modelo5,
  modelo6)


# Predição na base de teste
calibration_tbl <- models_tbl %>%
  modeltime_calibrate(new_data = testing(split))

forecasts <- calibration_tbl %>%
  modeltime_forecast(
    new_data    = testing(split),
    actual_data = anac)

# Verificar a acurácia dos modelos
calibration_tbl %>%
  modeltime_accuracy()

# Plotar as previsões com cada Modelo e Original da Base de teste
plot_modeltime_forecast(forecasts)


# Olhar os resíduos dos modelos
modeltime_residuals(calibration_tbl) |>
  plot_modeltime_residuals()

# Olhar as autocorrelações
# Interpretação: Nenhuma autocorrelação está boa. Então essa
# regressão não está bem ajustada
modeltime_residuals(calibration_tbl) |>
  group_by(.model_id) |>
  plot_acf_diagnostics(.index, .residuals)


# Colocar Co-variáveis ----------------------------------------------------
# Quando estamos trabalhando com as regressões podemos colocar covariáveis
# e fingir que não estamos trabalhando com uma série de tempo. Esse modelo,
# por ignorar o fator temporal, não produzirá boas previsões. Pois, 
# precisaria dos valores futuros para prever o futuro, é um modelo artificial
# que deve ser usado somente para entender as coisas.

modelo7 <- model_spec |> 
  fit(PASSAGEIROS_PAGOS ~ CARGA_PAGA_KG + as.factor(MES),
      training(split))
# Tabela com modelos
models_tbl <- modeltime_table(
  modelo1,
  modelo2,
  modelo3, 
  modelo4, 
  modelo5,
  modelo6, 
  modelo7)
# calibrar todos os modelos
calibration_tbl <- models_tbl %>%
  modeltime_calibrate(new_data = testing(split))

forecasts <- calibration_tbl %>%
  modeltime_forecast(
    new_data    = testing(split),
    actual_data = anac)

# Verificar a acurácia todos os modelos
calibration_tbl %>%
  modeltime_accuracy()
# Plotar as previsões com cada Modelo e Original da Base de teste
plot_modeltime_forecast(forecasts)
# Olhar os resíduos dos modelos
modeltime_residuals(calibration_tbl) |>
  plot_modeltime_residuals()
# Olhar as autocorrelações dos residuos
modeltime_residuals(calibration_tbl) |>
  group_by(.model_id) |>
  plot_acf_diagnostics(.index, .residuals)




# x ------------------------------------------------------------------------
# º2 Modelo: Suavização Exponencial ---------------------------------------



# Modelo: SUALIZAÇÃO EXPONENCIAL

#> É um modelo que considera a suposição de que observações que estão mais 
#> próximas tem maior peso na hra de calcular a próxima previsão e que esses
#> pesos diminuem exponencialmente ao longo do tempo. A ideia é dos anos 1950
#> e ao longo do tempo foi extendida para permitir incorporar tendencia,
#> sazonalidade entre outros.
#> Em comparação com as regressões, o método faz suposições adicionais que
#> permitem reduzir o número de parâmetros e, portanto, fazendo previsões 
#> mais robustas.

#> Observações mais próximas tem maior peso na predição e variáveis mais 
#> distantes (com lags maiores) tem menos peso para as predições.
#> Aplica uma formula para produzir a previsão. Então a previsão
#> de hoje é uma média geometrica das previsões anteriores
 
#> O peso não precisa ser sempre exponencial, pode ser mais flexivel
#> o calculo dos pesos.

#> O método estima com poucos parâmetros para garantir maior estabilidade
#> e não overfitar a base de treino. Entretanto, para o futuro, diminuirá
#> o seu desempenho.

#> 1ºOPCAO: Suavização exponencial mais simples
#>          - não considera a existencia de tendencia
#>          - não considera a existencia de sazonalidade
#>          - modelo pode ser descrito na forma de componentes
#>          - precisa estimar dois parâmtros (lo e alpha)



# Pacotes -----------------------------------------------------------------
library(tidymodels)


# Dados -------------------------------------------------------------------
soja <- readr::read_rds("https://github.com/curso-r/main-series/raw/main/dados/soja.rds")



# Modelo: Suavização exponencial simples ----------------------------------
# Função ses() para fazer a suavização exponencial
model <- forecast::ses(soja$KG_LIQUIDO/1000, initial = "simple")
model$model$par

#> Conclusão: O alpha foi 1. Isso significa que não vale a pena fazer a suavização
#> que seria melhor fazer uma média apenas


# Funções:
sigmoid <- function(x) {
  1/(1 + exp(-x))
}

suav_exp <- function(y) {
  function(pars) {
    alpha <- sigmoid(pars[1])
    l_0 <- pars[2]
    y_hat <- numeric(length = length(y))
    y_hat[1] <- l_0
    for (i in 2:length(y)) {
      y_hat[i] <- alpha*y[i-1] + (1-alpha)*y_hat[i-1]
    }
    #mean((y-y_hat)^2)
    y_hat
  }
}

y <- soja$KG_LIQUIDO/1000
opt <- optim(fn = suav_exp(y), par = c(0, y[1]))
sigmoid(opt$par[1])
opt$par[2]

y <- filter(anac, ANO <= 2019)$PASSAGEIROS_PAGOS
opt <- optim(fn = suav_exp(y), par = c(0, y[1]))
sigmoid(opt$par[1])
opt$par[2]


# Modelo SES ------------------------------------------------------------------
model <- forecast::ses(filter(anac, ANO <= 2019)$PASSAGEIROS_PAGOS, initial = "simple")
model$model$par
plot(model)




# º4 Modelo: Holt ---------------------------------------------------------
# Extende a suavização exponencial simples adicionando a tendencia
# A tendencia é estimada como uma média móvel das anteriores.
# Também podemos adicionar dampening

model <- forecast::holt(soja$KG_LIQUIDO/1000, initial = "simple")
plot(model)

model$model$par

holt <- function(y) {
  function(pars) {
    alpha <- sigmoid(pars[1])
    beta <- sigmoid(pars[2])
    l_0 <- pars[3]
    b_0 <- pars[4]
    y_hat <- numeric(length = length(y))
    y_hat[1] <- l_0 + b_0
    
    l_anterior <- l_0
    b_anterior <- b_0
    for (i in 2:length(y)) {
      l <- alpha*y[i-1] + (1-alpha)*(l_anterior + b_anterior)
      b <- beta*(l - l_anterior) + (1-beta)*b_anterior
      
      y_hat[i] <- l + b
      
      l_anterior <- l
      b_anterior <- b
    }
    mean((y-y_hat)^2)
  }
}

y <- soja$KG_LIQUIDO/1000
opt <- optim(
  fn = holt(y),
  par = c(inv_sigmoid(0.3), inv_sigmoid(0.1), y[1], y[2] - y[1])
)

sigmoid(opt$par[1])
sigmoid(opt$par[2])
opt$par[3]
opt$par[4]

# º5 Modelo: Holt-Winters ---------------------------------------------------------
# Extende o método do Holt adicionando um componente de sazonalidade.
# Funciona melhor do que o modelo Holt e SES, quando temos tendencia e sazonalidade
# SES é bom quando queremos estimar apenas tendencia
model <- forecast::hw(ts(soja$KG_LIQUIDO/1000,frequency = 12), initial = "simple")
plot(model)



