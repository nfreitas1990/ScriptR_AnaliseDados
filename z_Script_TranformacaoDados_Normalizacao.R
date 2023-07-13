# Análise: REGRESSAO LINEAR SIMPLES 
# Atualizado: Março 2023
# Autor: NATALIA F SOUZA



# Normalidade -------------------------------------------------------------
# Para os modelos lineares precisaremos realizar um teste para testar os 
# erros em relação aos fitted values. Porque se não houver normalidade
# dos erros (resíduos) os betas estimados pelo modelo náo podem ser usados
# para fins preditivos. Os betas do modelo, os intervalos de confiança não
# é adequado para predição.

# Se não houver normalidade temos que investigar a forma funcional da relação
# entre as variáveis para saber se haverá outra relação que melhor representa
# a relação.

# Dados -------------------------------------------------------------------
load(file = "bebes.RData")


# Exploratorio ------------------------------------------------------------
#Estatísticas univariadas
summary(bebes)

# Pacotes utilizados ------------------------------------------------------
pacotes <- c("plotly","tidyverse","ggrepel","fastDummies","knitr","kableExtra",
             "splines","reshape2","PerformanceAnalytics","correlation","see",
             "ggraph","psych","nortest","rgl","car","ggside","tidyquant","olsrr",
             "jtools","ggstance","magick","cowplot","emojifont","beepr","Rcpp",
             "equatiomatic")

options(rgl.debug = TRUE)

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

# Grafico Dispersão com ajustes (fits) linear e não-linear ----------------------------------------------------
# loess: mostra uma forma de regressão polinomial
# Comparando os dois modelos, podemos notar que o modelo não linear será melhor avaliado
# terá um R-square maior e menor erro do que o modelo linear, pois está mais ajustado
# aos pontos
ggplotly(
  bebes %>% 
    ggplot() +
    geom_point(aes(x = idade, y = comprimento),
               color = "grey20", alpha = 0.6, size = 2) +
    geom_smooth(aes(x = idade, y = comprimento),     # esse mostra o modelo linear (lm)
                method = "lm", formula = y ~ x,
                color = "#FDE725FF", se = F) +
    geom_smooth(aes(x = idade, y = comprimento),     # esse mostra o modelo não linear (loess) 
                method = "loess", formula = y ~ x,
                color = "#440154FF", se = F) +
    labs(x = "Idade em semanas",
         y = "Comprimento em cm") +
    theme_bw()
)


# Modelo ------------------------------------------------------------------
# Estimação do modelo OLS linear 
modelo_linear <- lm(formula = comprimento ~ idade,
                    data = bebes)
summary(modelo_linear)


# Teste de Normalidade --------------------------------------
# Existem vários testes para medir normalidade. O teste que tem
# sido mais utilizado é o teste Shapiro-Francia. Este teste tem 
# um bom ajuste, por isso é mto usado

# Shapiro-Wilk ------------------------------------------------------------
# Para Amostras pequenas ( n<= 30 obeservações) é aconselhável fazer o teste
# Shapiro-Wilk:
# Ho: Existe normalidade - Diferença não são estatisticamente significantes
# Ha: Não Existe normalidade - Diferenças são estatisticamente significantes
# Logo, se p<0.05 (rejeito Ho) -> não existe normalidade
# Logo, se p>0.05 (não rejeito Ho) -> existe normalidade
shapiro.test(modelo_linear$residuals)


# Shapiro-Francia ---------------------------------------------------------
# Para Amostras Grandes (n> 30 obeservações) é aconselhável fazer o teste
# Shapiro-Francia
# Esse teste pode não funcionar bem, pois ela foi criada para funcionar
# com dados com tamanho entre 5 - 5000 observações. Então vamos quebrar o
# código dessa função (F2) para modificar partes dessa função 
nortest::sf.test(modelo_linear$residuals) #função 'sf.test' do pacote 'nortest'

# código da função
# mudar o intervalo de funcionamento
function (x) 
{
  DNAME <- deparse(substitute(x))
  x <- sort(x[complete.cases(x)])
  n <- length(x)
  if ((n < 5 || n > 5000)) 
    stop("sample size must be between 5 and 5000")
  y <- qnorm(ppoints(n, a = 3/8))
  W <- cor(x, y)^2
  u <- log(n)
  v <- log(u)
  mu <- -1.2725 + 1.0521 * (v - u)
  sig <- 1.0308 - 0.26758 * (v + 2/u)
  z <- (log(1 - W) - mu)/sig
  pval <- pnorm(z, lower.tail = FALSE)
  RVAL <- list(statistic = c(W = W), p.value = pval, method = "Shapiro-Francia normality test", 
               data.name = DNAME)
  class(RVAL) <- "htest"
  return(RVAL)
}

# Criando a função a partir da anterior (alternando o intervalo)
sf.test2 <- function (x) 
{
  DNAME <- deparse(substitute(x))
  x <- sort(x[complete.cases(x)])
  n <- length(x)
  if ((n < 5 || n > 5000000)) 
    stop("sample size must be between 5 and 5000")
  y <- qnorm(ppoints(n, a = 3/8))
  W <- cor(x, y)^2
  u <- log(n)
  v <- log(u)
  mu <- -1.2725 + 1.0521 * (v - u)
  sig <- 1.0308 - 0.26758 * (v + 2/u)
  z <- (log(1 - W) - mu)/sig
  pval <- pnorm(z, lower.tail = FALSE)
  RVAL <- list(statistic = c(W = W), p.value = pval, method = "Shapiro-Francia normality test", 
               data.name = DNAME)
  class(RVAL) <- "htest"
  return(RVAL)
}

# rodando a função alterada
sf.test2(modelo_linear$residuals)


# Gráfico Diagnótico -----------------------------------------------------------------

# Histograma dos resíduos do modelo OLS linear
# normalidade dos resíduos
bebes %>%
  mutate(residuos = modelo_linear$residuals) %>%
  ggplot(aes(x = residuos)) +
  geom_histogram(aes(y = ..density..), 
                 color = "grey50", 
                 fill = "grey90", 
                 bins = 30,
                 alpha = 0.6) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(modelo_linear$residuals),
                            sd = sd(modelo_linear$residuals)),
                aes(color = "Curva Normal Teórica"),
                size = 2) +
  scale_color_manual("Legenda:",
                     values = "#FDE725FF") +
  labs(x = "Resíduos",
       y = "Frequência") +
  theme(panel.background = element_rect("white"),
        panel.grid = element_line("grey95"),
        panel.border = element_rect(NA),
        legend.position = "bottom")

# Homocedasticidade dos resíduos
# Visualização do comportamento dos resíduos em função dos fitted values do
# do modelo linear, com destaque para as distribuições das variáveis
# (pacote 'ggside')
# As curvas mostram que a maior quantidade não está distribuida em torno do 0
bebes %>%
  ggplot(aes(x = modelo_linear$fitted.values, y = modelo_linear$residuals)) +
  geom_point(color = "#FDE725FF", size = 2.5) +
  geom_smooth(aes(color = "Fitted Values"),
              method = "lm", formula = y ~ x, se = F, size = 2) +
  geom_xsidedensity(aes(y = after_stat(density)),
                    alpha = 0.5,
                    size = 1,
                    position = "stack") +
  geom_ysidedensity(aes(x = after_stat(density)),
                    alpha = 0.5,
                    size = 1,
                    position = "stack") +
  xlab("Fitted Values") +
  ylab("Resíduos") +
  scale_color_tq() +
  scale_fill_tq() +
  theme_tq() +
  theme(ggside.panel.scale.x = 0.4,
        ggside.panel.scale.y = 0.4)


# Transformação Log -------------------------------------------------------
# Transformação por Logaritmos
log10(iris$Sepal.Length)
#ou
iris |> 
  summarise(log10(Sepal.Length))


# Transformação Log+1 -----------------------------------------------------
# Quando os dados possuem zeros temos que somar 1 para a transformação log.
# porque o log de zero é indifinido. Então somamos 1, e fazemos o log 1,
# e registro volta a ser zero.
log10(iris$Sepal.Lenght + 1)
# ou
logsepal <- iris |> 
  summarise(log10(Sepal.Length) + 1 )

# salvar uma nova coluna na planilha com o log
cbind(iris, logsepal)
#ou
iris |> 
  mutate(logsepal = log10(Sepal.Length))


# Transformar Tabelas -----------------------------------------------------
# Com a função "decostand" podemos transformar os dados em muitas coisas diferentes
# segue algumas transformações

#PADRONIZAR
vegan::decostand(iris$Sepal.Length,method="standardize")

#TRANSFORMAR PARA PRESENCA/AUSENCIA
vegan::decostand(iris$Species,method="pa")

#TRANSFORMAR PARA LOG
vegan::decostand(iris$Sepal.Length,method="log")


# Transformações Box Cox --------------------------------------------------
# Tanto a forma linear quanto a logarítmica são dois casos particulares de 
# uma família mais extensa de transformações não-lineares. 
# A transformação de potência é definida como uma função de variação contínua,
# em relação ao parâmetro de potência λ (lambda).
# A escolha do melhor valor de lambda pode ser automatizada. 
# Na linguagem R, sua implementação pode ser feita através do pacote “forecast”,
# através da função “BoxCox.lambda()”.

# paper para transformações de dados
# https://www.ime.usp.br/~abe/lista/pdfQWaCMboK68.pdf

# O BoxCox informa o lambda que maximiza a aderencia do Y a normalidade então
# depois de transformado os resíduos tendem a se normalizar. O valor de labda
# varia de mais infinito a menos infinito. Então, apesar da tabela, podemos
# usar o lambda que a análise fornecer, ou então usar uma transformação já
# conhecida

# ATENÇÃO: só podemos aplicar boxcox para variável positiva que não contem zero 
# Então temos que somar 1

# Tabela de Resultado
# lambda    Transformação
# -2        x^-2
# -1        x^-1    - inversa
# -0.5      x^-0.5  - raiz quadratica inversa
# 0         log(x)  - logaritma ln(x) - logaritmo natural
# 0.5       sqrt(x) - raiz quadrada
# 1         x       - linear
# 2         x^2     - quadratica
# 3         x^3     - cubica

# 1- DESCOBRIR O MELHOR LAMBDA
#Para calcular o lambda de Box-Cox
#função 'powerTransform' do pacote 'car'
lambda_BC <- car::powerTransform(bebes$comprimento)
lambda_BC

# 2- CRIAR VARIÁVEL TRANSFORMADA NO DATASET
#Inserindo o lambda de Box-Cox na base de dados para a estimação de um novo modelo
# Ytransformado = (Y^λ - 1)/ λ
bebes$bc_comprimento <- (((bebes$comprimento ^ lambda_BC$lambda) - 1) / 
                           lambda_BC$lambda)

# 3 - USAR NOVA VARIÁVEL PARA RODAR O MODELO
#Estimando um novo modelo OLS com variável dependente transformada por Box-Cox
modelo_bc <- lm(formula = bc_comprimento ~ idade,
                data = bebes)

#Parâmetros do modelo
summary(modelo_bc) # atenção, para a interpretação temos que voltar para a escala original
                   # retransformar


# Comparação: Modelo Linear vs Transformado -------------------------------
# Dois modelos: 1 que rodamos assumindo a linearidade, apesar de não ser 
# significativo no teste de aderência. E outro modelo após a transformação e
# linearização (modelo_bc)

# Comparando os parâmetros do modelo_linear com os do modelo_bc
# CUIDADO!!! OS PARÂMETROS NÃO SÃO DIRETAMENTE COMPARÁVEIS!
export_summs(modelo_linear, modelo_bc,
             model.names = c("Modelo Linear","Modelo Box-Cox"),
             scale = F, digits = 4)

# Repare que há um salto na qualidade do ajuste para o modelo não linear (R²)
data.frame("R2 OLS" = round(summary(modelo_linear)$r.squared, 4),
           "R2 BoxCox" = round(summary(modelo_bc)$r.squared, 4)) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", position = "center", 
                full_width = F, 
                font_size = 30)

# Teste de Shapiro-Francia para os resíduos do modelo_bc
sf.test(modelo_bc$residuals) #função 'sf.test' do pacote 'nortest'

# Histograma dos resíduos do modelo_bc
bebes %>%
  mutate(residuos = modelo_bc$residuals) %>%
  ggplot(aes(x = residuos)) +
  geom_histogram(aes(y = ..density..), 
                 color = "grey50", 
                 fill = "gray90", 
                 bins = 30,
                 alpha = 0.6) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(modelo_bc$residuals),
                            sd = sd(modelo_bc$residuals)),
                aes(color = "Curva Normal Teórica"),
                size = 2) +
  scale_color_manual("Legenda:",
                     values = "#440154FF") +
  labs(x = "Resíduos",
       y = "Frequência") +
  theme(panel.background = element_rect("white"),
        panel.grid = element_line("grey95"),
        panel.border = element_rect(NA),
        legend.position = "bottom")

#Visualização do comportamento dos resíduos em função dos fitted values
#do modelo não linear com transformação de Box-Cox, com destaque para as
#distribuições das variáveis (pacote 'ggside')
bebes %>%
  ggplot(aes(x = modelo_bc$fitted.values, y = modelo_bc$residuals)) +
  geom_point(color = "#440154FF", size = 2.5) +
  geom_smooth(aes(color = "Fitted Values"),
              method = "lm", formula = y ~ x, se = F, size = 2) +
  geom_xsidedensity(aes(y = after_stat(density)),
                    alpha = 0.5,
                    size = 1,
                    position = "stack") +
  geom_ysidedensity(aes(x = after_stat(density)),
                    alpha = 0.5,
                    size = 1,
                    position = "stack") +
  xlab("Fitted Values") +
  ylab("Resíduos") +
  scale_color_tq() +
  scale_fill_tq() +
  theme_tq() +
  theme(ggside.panel.scale.x = 0.4,
        ggside.panel.scale.y = 0.4)

#Fazendo predições com os modelos OLS linear e Box-Cox
#qual é o comprimento esperado de um bebê com 52 semanas de vida?
#Modelo OLS Linear:
predict(object = modelo_linear,
        data.frame(idade = 52),
        interval = "confidence", level = 0.95)

#Modelo Não Linear (Box-Cox):
predict(object = modelo_bc,
        data.frame(idade = 52),
        interval = "confidence", level = 0.95)


# Interpretação: voltar a escala original ---------------------------------
# Não podemos nos esquecer de fazer o cálculo para a obtenção do fitted
# value de Y (variável 'comprimento')
# ((Y * lambda)+1) ^ (1/ lambda)
(((54251.12 * 2.659051) + 1)) ^ (1 / 2.659051)

#Salvando os fitted values dos dois modelos (modelo_linear e modelo_bc) no
#dataset 'bebes'
bebes$yhat_linear <- modelo_linear$fitted.values
bebes$yhat_modelo_bc <- (((modelo_bc$fitted.values*(lambda_BC$lambda))+
                            1))^(1/(lambda_BC$lambda))

#Visualizando os fitted values dos dois modelos no dataset
bebes %>%
  select(idade, comprimento, yhat_linear, yhat_modelo_bc) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 25)

#Ajustes dos modelos: valores previstos (fitted values) X valores reais
bebes %>%
  ggplot() +
  geom_smooth(aes(x = comprimento, y = yhat_linear, color = "OLS Linear"),
              method = "lm", se = F, formula = y ~ splines::bs(x, df = 5),
              size = 1.5) +
  geom_point(aes(x = comprimento, y = yhat_linear),
             color = "#FDE725FF", alpha = 0.6, size = 2) +
  geom_smooth(aes(x = comprimento, y = yhat_modelo_bc, color = "Box-Cox"),
              method = "lm", se = F, formula = y ~ splines::bs(x, df = 5),
              size = 1.5) +
  geom_point(aes(x = comprimento, y = yhat_modelo_bc),
             color = "#440154FF", alpha = 0.6, size = 2) +
  geom_smooth(aes(x = comprimento, y = comprimento), method = "lm", 
              color = "gray30", size = 1.05,
              linetype = "longdash") +
  scale_color_manual("Modelos:", 
                     values = c("#440154FF", "#FDE725FF")) +
  labs(x = "Comprimento", y = "Fitted Values") +
  theme(panel.background = element_rect("white"),
        panel.grid = element_line("grey95"),
        panel.border = element_rect(NA),
        legend.position = "bottom")



