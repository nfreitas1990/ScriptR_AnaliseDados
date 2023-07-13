
# Modelo: SUALIZAÇÃO EXPONENCIAL


# Pacotes -----------------------------------------------------------------
library(fable)
library(feasts)
library(timetk)
library(modeltime)
library(tidymodels)
library(tidyverse)

# Dados -------------------------------------------------------------------
anac <- readr::read_rds("https://github.com/curso-r/main-series/blob/main/dados/anac-sp.rds?raw=true") %>%
  mutate(DATA_ym = tsibble::yearmonth(paste(ANO, MES, sep = "-"))) %>%
  filter(DATA < lubridate::ymd("2019-01-01")) %>%
  mutate(
    TEMPO_DESDE_INICIO = difftime(
      DATA,
      lubridate::ymd("1999-12-01"),
      units = "days"
    )/30,
    LAG_1 = lag(PASSAGEIROS_PAGOS, 1, default = 0)
  )

# Analise Gráfica ---------------------------------------------------------
anac %>%
  plot_time_series(DATA, PASSAGEIROS_PAGOS)


# TS ----------------------------------------------------------------------
anac_ts <- anac %>%
  as_tsibble(index = DATA_ym) |>
  select(PASSAGEIROS_PAGOS, everything())
autoplot(anac_ts)


# Decomposição ------------------------------------------------------------
stl_components <- anac_ts %>%
  model(
    STL(PASSAGEIROS_PAGOS ~ season(12) + trend())
  ) %>%
  components()
autoplot(stl_components)


# Função de Autocorrelação ------------------------------------------------
stl_components %>%
  ACF(remainder) %>%
  autoplot()

# Função de sazonalidade --------------------------------------------------
gg_season(anac_ts)



# Remover os anos atipicos ------------------------------------------------
# Modelo
split <- time_series_split(
  anac,
  DATA,
  initial = "18 years",
  assess = "12 month"
)

# Separar Treino e Teste: Backtesting -------------------------------------
plot_time_series_cv_plan(
  tk_time_series_cv_plan(split),
  DATA, PASSAGEIROS_PAGOS)

# 1. Definição do modelo inicial --------------------------------------------------------
# Essa função exp_smoothing () ajuda a escolher os parâmetros para 
# parametrizar
model_spec <- modeltime::exp_smoothing() %>%
  set_engine("ets")

# Opçao 2:
# Podemos adicionar o
# damping = none
# trend = none - para excluir tendencia
#         additive - para tendencia aditiva
#         multiplicative- tendencia multiplicativa
#         auto - automático
# season = none - tirar sazonalidade
# Neste caso, ele vai dar uma média simples, chutar o último número e pronto
# model_spec <- modeltime::exp_smoothing(
#   damping = 'none',
#   trend = 'none',
#   season = 'none'
# ) %>%
#   set_engine("ets")

# 2. Ajuste do modelo --------------------------------------------------------
# Por enquanto não vamos usá-lo
# Modelo 1:
# Ajuste:(obs: está usando a definição do model_spec)
modelo <- model_spec |>
  fit(PASSAGEIROS_PAGOS ~ DATA, training(split))

# Modelo 2
# Definição:
modelo_naive_com_sazonalidade <-
  modeltime::naive_reg(seasonal_period = "1 year") %>%
  set_engine("snaive")
# Ajuste:
modelo_naive <- modelo_naive_com_sazonalidade %>%
  fit(PASSAGEIROS_PAGOS ~ DATA, training(split))

# Modelo 3
# Ajuste: (obs: está usando a definição do model_spec)
modelo3 <- model_spec |>
  fit(PASSAGEIROS_PAGOS ~ ANO + DATA, training(split))


# Tabela com todos os modelos ---------------------------------------------
models_tbl <- 
  modeltime_table(
  modelo,
  modelo_naive, 
  modelo3
)

# Calibração --------------------------------------------------------------
calibration_tbl <- models_tbl %>%
  modeltime_calibrate(new_data = testing(split))


# Validação ---------------------------------------------------------------
# quando a gnt tira o new data e acrescentar o h=200 conseguimos prever 
# para o futuro. neste caso, 200 passos a frente. Para isso, temos que 
# tirar da tabela models_tbl os outros modelos, e fitar tudo com o modelo
# escolhido antes de rodar esse código aqui embaixo
forecasts <- calibration_tbl %>%
  modeltime_forecast(
    #new_data    = testing(split),
    h = 200
  )

# Métricas de erro --------------------------------------------------------
calibration_tbl %>%
  modeltime_accuracy()
#> Observar qual o modelo possui o menor mape e rmse

# Gráfico -----------------------------------------------------------------
plot_modeltime_forecast(forecasts)


# Resíduos ----------------------------------------------------------------
modeltime_residuals(calibration_tbl) |>
  plot_modeltime_residuals()


# Função de Autocorrelação ------------------------------------------------
modeltime_residuals(calibration_tbl) |>
  group_by(.model_id) |>
  plot_acf_diagnostics(.index, .residuals)

#> Esse modelo vira um pouco uma caixa preta. Já fica dificil olhar cada 
#> parâmetro. Esse vai ser mais um modelo para colocar para comparar com 
#> outros modelos






# Observando Séries e seus Comportamentos ---------------------------------

# y é um numero fixo acrescido de um erro aleatorio -----------------------
# Se pegamos uma série de tempo e ela tem comportamento parecido com essa
# com a funçao de autocorrelaçao ACF em escada (o que indica tendencia).
# com a funçao de autocorrelaçao ACF das diferenças entre o tempo0 e tempo1
# náo dando significativo (dentro das barras azuis). 
# Teríamos uma série como essa que geramos, na qual a observação no tempo
# atual é a observaçao no passado, mais um tanto de valor, mais o erro. 

set.seed(123)

y_fixo_com_ruido <- tsibble::tsibble(
  mutate(tibble(x = 1:50), y = x+rnorm(50)),
  index = x)

y_fixo_com_ruido |>
  autoplot()

y_fixo_com_ruido |>
  ACF() |>
  autoplot()

y_fixo_com_ruido |>
  PACF(y) |>
  autoplot()

y_fixo_com_ruido |>
  ACF(diff(y)) |>
  autoplot()

y_fixo_com_ruido |>
  PACF(diff(y)) |>
  autoplot()

# que padrão é esse? esse é novo!

# série em que o Y é sempre o Y anterior + um errinho aleatório -----------
# Neste caso, a funçao ACF tb é uma escada, mas faz um desenho diferente
# o ACF da diferença dá náo significativo (=ruído branco)
set.seed(123)

y_cresce_aleatoriamente <- tsibble::tsibble(
  tibble(y = cumsum(rnorm(50, 1, 2)), x = 1:50),
  index = x)

y_cresce_aleatoriamente |>
  autoplot()

y_cresce_aleatoriamente |>
  ACF(y) |>
  autoplot()

y_cresce_aleatoriamente |>
  PACF(y) |>
  autoplot()

y_cresce_aleatoriamente |>
  gg_lag(geom = "point")

y_cresce_aleatoriamente |>
  ACF(diff(y)) |>
  autoplot()

# série em que o Y e o Y anterior X peso + erro elatório ------------------

#set.seed(123)

N <- 1000

erros <- rnorm(N,mean =  1)

y <- NULL
y[1] = 1

peso <- -0.8

for(i in 2:(N+1)){
  y[i] = peso*y[i-1]+erros[i-1]
}

y <- y[-1]

y_cresce_com_peso <- tsibble(
  tibble(
    y = y,
    x = 1:N),
  index = x)

y_cresce_com_peso |>
  autoplot()

y_cresce_com_peso |>
  ACF(y) |>
  autoplot()

y_cresce_com_peso |>
  ACF(diff(y)) |>
  autoplot()

# A situação em que tem peso é muito mais complicada...

# o peso faz com que diff(y) não seja só o errinho
# se o peso é negativo a aucorrelação oscila. Por conta desse complicador
# foi inventado a função de autocorrelação parcial


# Função de autocorrelação parcial ----------------------------------------
# Faz a mesma coisa do que a autocorrelação ACF , só que tenta ajustar a 
# autocorrelaçao do lag 2 excluindo o efeito do Lag 1.
# Então ela faz uma regressão, é como ela tira o efeito do primeiro lag.
# ela dá uma idéia de como é o padrão autoregressivo dentro de uma série.
# É autoregressivo pq é como se fosse a regressao da observaçao com ela mesmo. 
# Conseguimos ver qual lag explica mais, tem mais peso para a explicaçao
# Entáo só sobrevive nessa regressáo aqueles lags q efetivamente satisfazem 
# a formula. Ajudando a limpar os desenhos da função de autocorrelação
y_cresce_com_peso |>
  PACF(y) |>
  autoplot()

# as correlações são os betas de regressões como essa:
y_cresce_com_peso |>
  mutate(y_lag = lag(y),
         y_lag_2 = lag(y, 2),
         y_lag_3 = lag(y, 3)) |>
  lm(y~ y_lag+ y_lag_2+ y_lag_3, data = _)




