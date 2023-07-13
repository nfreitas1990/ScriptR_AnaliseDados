

# Natália Freitas
# Modelos Logisticos



# Contextualização --------------------------------------------------------
# A regressão logística é um modelo estatístico que permite estimar a chance da 
# ocorrência de um determinado desfecho categórico (Y) em função de um ou mais
# preditores (X), que podem ser contínuos ou categóricos. Quando a variável 
# dependente apresenta apenas dois níveis ou classes, a regressão é chamada de 
# binária. Quando há mais níveis ou classes, é chamada de multinomial.

# Desta maneira, é possível entender a regressão logística como um complemento
# da regressão linear aplicada a variáveis categóricas a partir de uma função 
# de ligação, uma generalização do teste Qui-quadrado ou, de maneira geral, 
# um caso particular da família dos modelos lineares generalizados (GLM), que 
# implementa uma ligação logit.



# MODELO REGRESSAO      VARIAVEL DEPENDENTE               DISTRIBUIÇÃO    FUNÇÃO LIGAÇÃO
# Linear                 Quantitativa                      Normal            Yhat             
# Com transf.BoxCox      Quantitativa              Normal após transf.    ((Y^lambda) - 1)/lambda   
# Logística Binaria      Qualitativa (2cat.)             Bernoulli         ln(p/1-p)          
# Logistica Multinomial  Qualitativa M(M>2) Categorias     Binomial         ln(pm/1-pm)       
# Poisson                Quantitativa com valores          Poisson          ln(lambda poisson)
#                        inteiros e ñ negativo                                                
# Binomial Negativa       Quantitativa com valores          Poisson-gamma    ln(lambda bneg)  
#                         inteiros e ñ negativos                                              


# Interpretação -----------------------------------------------------------
# Os Betas representam a chance de um evento ocorrer (Otts ratios). 
# A chance é a probabilidade do evento (y) ocorrer dividido pela probabilidade
# de não ocorrer o evento (y). Então o Beta1 é o quanto aumenta ou diminui 
# a chance do Y ocorrer.

# Exemplo: para o aumento em 1 unidade do numero de exclamações em um email, 
# aumenta em 4% a chance do email ser um spam.

# Não interpreta da mesma forma como da regressão linear simples e multipla.
# Aqui o valor dos Estimate estão em exponencial. Por isso, temos que transformar
# os resultados para que tenhamos um valor que dá uma interpretação direta.


# Método Máxima Verossimilhança -------------------------------------------
# A Máxima Verossimilhança estima os parâmetros através do processo iterativo 
# para maximizar o acerto da probabilidade de ocorrência de um evento à
# sua real ocorrencia

#> O método classico para estimar os parâmetros de uma regressão consiste no
#> método de mínimos quadrados, onde a reta passa no ponto que minimiza
#> a soma dos desvios quadráticos. A verossimilhança é o valor proporcional a 
#> probabilidade de um modelo qualquer (uma das inclinações de retas simulados) 
#> gerar os nossos dados. Então podemos maximizar a verossimilhanca, busca o valor 
#> de inclinação (estimativa do modelo) que maximissa a verossimilhança. 
#> É usada tanto para estimar os parâmetros dos modelos, quanto para dar uma
#> ideia do ajuste do modelo. Portanto, os parâmetros Betas não são mais 
#> estimados por Minimos quadrados. Agora é utilizado o método de Maxima 
#> Verossimilhança.
#> Os valores de verossimilhanças são tão pequenos que costumam ser 
#> apresentados em log.



# x -----------------------------------------------------------------------

# 1. Regressão Logistica Binária ------------------------------------------
#    Tecnica supervisionada de Machine Learning utilizada para explicar ou
#    predizer a probabilidade de ocorrência de determinado evento em função
#    de uma ou mais variáveis explicativas. 

#    Variável dependente (resposta): binária.

#    Variáveis preditoras: métricas ou não métricas. Se a variável não for métrica,
#    for qualitativa, vamos dummizar a variável qualitativa antes de rodar os 
#    modelos.



# Conceito ----------------------------------------------------------------
# A regressão logística é uma regressão linear. Mas nesse caso o Y é Zero ou 1.
# A variável resposta é binária. Então quando vamos explorar os dados olhamos 
# para a proporção da variável resposta (0 e 1), se aumenta ou diminui, conforme
# o aumento das variáveis explicativas. 

# Então olhamos para as proporções de zero e 1. E essas proporções serão associadas
# as variáveis respostas. Então fazemos o log (p/1-p) (=log da chance de acontecer o evento),
# e depois de achar os betas usamos a formula da regressão p = 1/ 1 + e* -(beta0 +beta1* x)

# Atenção: usando essa formula p = 1/ 1 + e* -(beta0 +beta1* x), estamos calculando
# a probabilidade do evento. Usando com sinal positivo p = 1/ 1 + e* +(beta0 +beta1* x)
# estamos calculando a probabilidade no não-evento


# Objetivo ----------------------------------------------------------------
# Estimar a probabilidade de ocorrência de determinado evento ou de que uma
# observação venha a se enquadrar nessa ou naquela categoria


# Função Logística --------------------------------------------------------
# A função logistica é definida para que se estabeleça a probabilidade de ocorrência
# de determinado evento e a importancia das variáveis explicativas para esta 
# ocorrencia.
# -Os parâmetros são estimados pelo processo iterativo para maximizar o acerto da
#  probabildade de ocorrência de um evento a sua real ocorrência (Método de 
#  Máxima Verossimilhança).
# -Os resultados atribuíveis à variável dependente estarão entre 0 e 1.
# -Análise do ajuste do Modelo: testes de significância dos parâmetros e tabela de
#  classificação  (matriz de confusão)

# Interpretação -----------------------------------------------------------
# Os resultados são interpretados em termos de probabilidade. E estão em escala
# log. Então teremos que fazer o exponencial dos resultados para ter uma 
# interpretação direta.
# Os coeficientes da regressão representam mudanças percentuais nas chances.
# Se após exponenciar os valores estimados dos Slopes (B1) obtivermos 1.04: 
# significa que para cada unidade de X, a chance de Y acontecer aumenta em 4%.
# Se fosse 0.95, significaria que para cada unidade de X, a chance de y acontecer (sucesso)
# reduz em 5%.
# Se fosse 4.04, significaria que a cada unidade de X, a chance de sucesso de Y 
# aumenta 400%, ou seja, aumenta 4 vezes.



# Pressupostos ------------------------------------------------------------
# Os seguintes pressupostos devem ser investigados:
# (i) Os dados são aleatórios e representativos da população
# (ii) A variável dependente é dicotômica/binária
# (iii) Os preditores não apresentam alta correlação entre eles
# (iv) Há uma relação linear entre preditores contínuos e o logit do desfecho



# Distribuição Bernoulli --------------------------------------------------
# Quando há apenas dois resultados possíveis para cada observação. No entanto,
# mesmo uma variável com um grande numero de resultados pode ser redefinida como 
# uma tentativa de Bernoulli se for possível colapsar a amplitude de resposta
# a dois resultados. (e.g. um indice com valores contínuos e colocar: acima 1= 
# impactado; abaixo 1= referencia)



# Odds --------------------------------------------------------------------
# Chance de ocorrencia de um evento é representado por (p/1-p). Isto significa
# a que a chance é a probabilidade do evento ocorrer (p) dividido pela probabilidade
# do evento não ocorrer (1-p).

# Mas trabalhamos com log (p/1-p) (=log da chance de acontecer o evento),
# e depois de achar os betas usamos a formula da regressão:
# p = 1 / 1 + e* -(beta0 +beta1* x)

# Portanto, não podemos confundir probabilidade com chance de um evento ocorrer
# São coisas diferentes. A chance é calculada com base na probabilidade de sucesso
# no evento.



# Gráfico -----------------------------------------------------------------
# Curva logistica (= curva S ou curva sigmóide). Uma curva que é assintótica
# em zero (chance de não ocorrer) e 1 (chance de ocorrer). A fórmula do Logit
# garante probabilidades entre zero e 1, embora tenda a mais e menos infinito.



# Logito ------------------------------------------------------------------
# é o logaritmo natural da chance de ocorrência de um evento (sucesso). A partir
# do logito, defini-se a expressão de probabilidade de ocorrência do evento em
# estudo, em função das variáveis explicativas.
# O logito é o vetor que contém as variáveis explicativas: " -(Bo + B1*X1 +B2*X2) "

# sendo a formula complera: p = 1 / 1 + e* -(Bo + B1*X1 +B2*X2)




# Pacotes -----------------------------------------------------------------
pacotes <- c("plotly","tidyverse","knitr","kableExtra","fastDummies","rgl","car",
             "reshape2","jtools","stargazer","lmtest","caret","pROC","ROCR","nnet",
             "magick","cowplot","globals","equatiomatic")

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


# Conceitual --------------------------------------------------------------
# Plotando a curva sigmóide teórica de ocorrência de um evento para um range
# do logito z entre -5 e +5

prob <- function(z){
  prob = 1 / (1 + exp(-z))
}

data.frame(z = -5:5) %>%
  ggplot() +
  stat_function(aes(x = z, color = "Prob. Evento"),
                fun = prob,
                size = 2) +
  geom_hline(yintercept = 0.5, linetype = "dotted") +
  scale_color_manual("Legenda:",
                     values = "#440154FF") +
  labs(x = "Logito z",
       y = "Probabilidade") +
  theme_bw()


#                #              #             #              #            #
# x -----------------------------------------------------------------------
# EXEMPLO 1 : BASE ATRASADO -----------------------------------------------


# Dados -------------------------------------------------------------------
load(file = "raw_datas_exemplos/Atrasado.RData")
Atrasado

# Análise Exploratória ----------------------------------------------------
# Visualizando a base de dados
Atrasado %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 25)

View(Atrasado)
# Estatísticas descritivas
summary(Atrasado)

# Tabela de frequências absolutas da variável 'atrasado'
# Atenção, não precisamos balancear o Y para rodar o modelo logístico.
# Ou seja, não precisa ter o mesmo número de observações para cada categoria da
# variável Y.
table(Atrasado$atrasado) 
Atrasado$sem <-  as.double(Atrasado$sem)

# Estimar Modelo Logistico Binario ----------------------------------------
modelo_atrasos <- glm(formula = atrasado ~ dist + sem, 
                      data = Atrasado, 
                      family = "binomial")

# Parâmetros do modelo_atrasos
summary(modelo_atrasos)

# Call:
#   glm(formula = atrasado ~ dist + sem, family = "binomial", data = Atrasado)
# 
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -2.2672  -1.0719   0.3296   0.8985   1.5071  
# 
# Coefficients:
#              Estimate    Std. Error  z value   Pr(>|z|)   
# (Intercept)   -26.16654    8.44197  -3.100     0.00194 **
#   dist          0.19038    0.07637   2.493     0.01267 * 
#   sem           2.36288    0.79512   2.972     0.00296 **
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 135.37  on 99  degrees of freedom
# Residual deviance: 100.93  on 97  degrees of freedom
# AIC: 106.93
# 
# Number of Fisher Scoring iterations: 7


# Pr(>|z|): Significancia dos parametros. Todos foram significativos. Então,
#           não precisamos fazer stepwise para eliminar os sem significancia.

# Interpretação -----------------------------------------------------------
# Não interpreta da mesma forma que a regressão linear simples e multipla
# Aqui o valor dos Estimate estão em exponencial. Por isso, usamos 
# funções para soltar o valor que dá a interpretação direta
gtsummary::tbl_regression(modelo_atrasos) # em log
gtsummary::tbl_regression(modelo_atrasos, exponentiate = T) # direto

# Visualização do modelo no ambiente Viewer
equatiomatic::extract_eq(modelo_atrasos, use_coefs = T,
           wrap = T, show_distribution = T) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = F,
                font_size = 25)


# summary( ) --------------------------------------------------------------
# Note que o summary() do modelo não traz a estatística geral do modelo,
# nem tampouco do valor de LL e dos intervalos de confiança.
summary(modelo_atrasos)


# Intervalos de Significancia -------------------------------------
# Extração dos intervalos de confiança ao nível de siginificância de 5%
confint(modelo_atrasos, level = 0.95)

# Interpretação: O intervalo de confiança não contém o zero. O q significa que 
# o parâmetro deles foi significativo (p<0.05).


# Qualidade do Modelo: Log-Likelihood ----------------------------------------------------------
# Extração do valor de Log-Likelihood (LL)
# É o valor que maximiza os acertos do modelo
# Quanto maior o valor, melhor o modelo
logLik(modelo_atrasos)



# Outputs -----------------------------------------------------------------
# Além do summary (), outras maneiras de apresentar os outputs do modelo 

# Opção 1
jtools::summ(modelo_atrasos, confint = T, digits = 3, ci.width = .95)
# MODEL INFO:
#   Observations: 100
# Dependent Variable: atrasado
# Type: Generalized linear model
# Family: binomial 
# Link function: logit 
# 
# MODEL FIT:
#   χ²(2) = 34.439, p = 0.000
# Pseudo-R² (Cragg-Uhler) = 0.393
# Pseudo-R² (McFadden) = 0.254
# AIC = 106.933, BIC = 114.748 
# 
# Standard errors: MLE
# 
#   Est.      2.5%    97.5%   z val.       p
# 
#   (Intercept)         -26.167   -42.712   -9.621   -3.100   0.002
# dist                  0.190     0.041    0.340    2.493   0.013
# sem                   2.363     0.804    3.921    2.972   0.003
# 

# INTERPRETAÇÃO:
# χ²(2) = 34.439, p = 0.000  -> É análogo ao F. Testa se existe pelo menos um 
#                               beta significativo (=estatisticamente diferente
#                               de zero)

# AIC = 106.933, BIC = 114.748 -> Akaike Info Criterion e Bayesian Info Criterion
#                                 usados para ver o ajuste dos modelos. Quanto
#                                 menor o valor, melhor o modelo. Muito usam o 
#                                 BIC para comparar modelos com quantidade de variáveis
#                                 diferentes, pq leva em consideração essa diferença
#                                 Eles penalizam modelos com muitos betas


# Pseudo-R² (Cragg-Uhler) = 0.393  -> similar ao R²: não é mto indicado
# Pseudo-R² (McFadden) = 0.254     -> similar ao R²: não é mto indicado


# Opção 2
jtools::export_summs(modelo_atrasos, scale = F, digits = 6)

# Opção 3: interessante para mostrar nos trabalhos
stargazer::stargazer(modelo_atrasos, nobs = T, type = "text") # mostra o valor de Log-Likelihood



# Predições ---------------------------------------------------------------
# Fazendo predições para o modelo_atrasos. 
# Exemplo: qual a probabilidade média de se chegar atrasado quando o trajeto 
# tem 7 km e passa-se por 10 semáforos no percurso?
predict(object = modelo_atrasos, 
        data.frame(dist = 7, sem = 10), 
        type = "response")


# Abrindo o predict
# B0= -26.17
# B1= + 0.19* (distancia)
# B2 = +2.36*(sem semaforo)
# probabilidade = 1/1+ e^ - (Bo+B1X1+B2X2) 
probabilidade = (1)/(1+exp (- (-26.17+0.19*7+2.36*10)))
probabilidade
# [1] 0.224436



# Predição para base de dados ---------------------------------------------
# Adicionando os valores previstos de probabilidade na base de dados
# Para cada valor observado no dataset, teremos também o valor predito
Atrasado$phat <- modelo_atrasos$fitted.values

# Visualizando a base de dados com a variável 'phat'
Atrasado %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 22)
# estudante	atrasado	dist	sem	  phat
# Gabriela	0	      12.5	  7	  0.0007120 -> probabilidade quase zero de chegar atrasado
# Patricia	0	      13.3	  10	0.4985613 -> probabilidade 49% de chegar atrasado
# Antonio	  0	      15.4	  10	0.5972531 -> probabilidade 59% de chegar atrasado


# Ajuste linear entre a variável dependente e a variável 'sem' (apenas para fins
# didáticos) -> NAO ADEQUADO O MODELO LINEAR
ggplotly(
  Atrasado %>% 
    ggplot() +
    geom_point(aes(x = sem, y = atrasado), color = "orange", size = 2) +
    geom_smooth(aes(x = sem, y = phat), 
                method = "lm", formula = y ~ x,
                se = FALSE,
                color = "darkorchid", size = 2) +
    labs(x = "Quantidade de Semáforos",
         y = "Atrasado") +
    theme_bw()
)

# Ajuste Logistico Deterministico -----------------------------------------
# Ajuste logístico determinístico entre a variável dependente e a variável 'sem'
# Sigmóide
ggplotly(
  Atrasado %>% 
    ggplot() +
    geom_point(aes(x = sem, y = atrasado), color = "orange", size = 2) +
    geom_smooth(aes(x = sem, y = phat), 
                method = "glm", formula = y ~ x, 
                method.args = list(family = "binomial"), 
                se = FALSE,
                color = "darkorchid", size = 2) +
    labs(x = "Quantidade de Semáforos",
         y = "Atrasado") +
    theme_bw()
)


# Ajuste Logistico Probabilistico -----------------------------------------
# Ajuste logístico probabilístico entre a variável dependente e a variável 'sem'
# Sigmóide
ggplotly(
  Atrasado %>% 
    ggplot() +
    geom_point(aes(x = sem, y = phat), color = "orange", size = 2) +
    geom_smooth(aes(x = sem, y = phat), 
                method = "glm", formula = y ~ x, 
                method.args = list(family = "binomial"), 
                se = FALSE,
                color = "darkorchid", size = 2) +
    labs(x = "Quantidade de Semáforos",
         y = "Atrasado") +
    theme_bw()
)


# Ponto de Corte ----------------------------------------------------------
# Depois que definimos as probabilidades, temos que adotar um ponto de corte
# para dizer a partir de que probabilidade vamos considerar o sucesso do evento


# Inicial: adotar um cutoff de 0.5.
# Então quando a probabilidade é maior do que 50% assumimos que a pessoa 
# chegaria atrasada

# Matriz de confusão para cutoff >= 0.5
caret::confusionMatrix(table(predict(modelo_atrasos, type = "response") >= 0.5,
                      Atrasado$atrasado == 1)[2:1, 2:1])


# INTERPRETAÇÃO:
# Confusion Matrix and Statistics
# 
# LINHA: Classificado
# COLUNA: Real

#        TRUE FALSE
# TRUE    46    16
# FALSE   13    25
#        Accuracy : 0.71            
#        95% CI : (0.6107, 0.7964)
#        No Information Rate : 0.59            
#        P-Value [Acc > NIR] : 0.008742 

#       ---REAL--- 
#        TRUE              FALSE          |
# TRUE    TRUE POSITIVE   FLASE POSITIVO  |Predicted
# FALSE   FALSE NEGATIVE  TRUE NEGATIVE   |          


# Coluna TRUE
# Então temos 46 pessoas que tinham chance acima de 50% de chegar atrasado e de
# fato cehgou atrasado. Enquanto 13 pessoas tinham chance acima de 50% de chegar
# atrasado, mas não chegaram. (46 atrasados e classificados como atrasados)
# (13 atrasados e classificados como NÃO atrasados pelo modelo)

# Coluna FALSE
# Temos 16 pessoas que não chegaram atrasadas e foram classificadas como atrasadas
# pelo modelo. Enquanto, temos 25 pessoas atrasadas que foram classificadas como
# atrasadas pelo modelo


# Total de 59 atrasados (46+13) (o modelo classificou 46 certo e 13 errado)
# Total de 41 não-atrasados (16+25) (o modelo classificou 16 errado e 25 certo)


# Qualidade do Modelo: Acurácia ----------------------------------------------------------------
# Eficiencia global do modelo
# (TRUE|TRUE) + (FALSE|FALSE) / total de observações
(46+25)/100
# [1] 0.71


# Qualidade do Modelo: Sensitividade ------------------------------------------------------------
# Taxa de acerto somente de quem se atrasou (59) (= sucesso)
# (quantidade de TRUE|TRUE)/ total de TRUE (atrasados)
(46)/59


# Qualidade do Modelo: Especificidade ----------------------------------------------------------
# Taxa de acerto somente de quem se NAO atrasou (41) (= não sucesso)
# (quantidade de TRUE|TRUE)/ total de TRUE (atrasados)
(25/41)


# Qualidade do Modelo: Precisão ----------------------------------------------------------------
# é a taxa de acerto de quem foi classificado como evento
# 72 pessoas classificado como evento (46+26)
(46/46+26)


# Qualidade do Modelo: F1 Score ----------------------------------------------------------------
# ´euma média harmonica entre sensitividade e precisão
F1=2*Sensitividade*Precisão/ Sensitividade+Precisão


# Matriz de Sentividade|Especf|Acuracia -----------------------------------
# Visualizando os principais indicadores desta matriz de confusão
data.frame(Sensitividade = confusionMatrix(table(predict(modelo_atrasos,
                                                    type = "response") >= 0.5,
                                            Atrasado$atrasado == 1)[2:1, 2:1])[["byClass"]][["Sensitivity"]],
           Especificidade = confusionMatrix(table(predict(modelo_atrasos,
                                                          type = "response") >= 0.5,
                                                  Atrasado$atrasado == 1)[2:1, 2:1])[["byClass"]][["Specificity"]],
           Acuracia = confusionMatrix(table(predict(modelo_atrasos,
                                                    type = "response") >= 0.5,
                                            Atrasado$atrasado == 1)[2:1, 2:1])[["overall"]][["Accuracy"]]) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", position = "center",
                full_width = F, 
                font_size = 27)


# Recalculando Cutoff -----------------------------------------------------
# Dependendo do Cutoff os valores de acuracia, sensibilidade e Especificidade
# vão mudar.

# Matriz de confusão para cutoff = 0.3
confusionMatrix(table(predict(modelo_atrasos, type = "response") >= 0.3,
                      Atrasado$atrasado == 1)[2:1, 2:1])
# Accuracy : 0.67 
# Sensitivity : 1.0000  
# Specificity : 0.1951  

# Interpretação: neste caso, caiu a acurácia, em contrapartida maximizamos 
# a sensitividade (acertamos todos que se atrasaram), mas acamos penalizando
# a especificidade (ou seja, acertamos bem pouco de quem não-se atrasou)


# Matriz de confusão para cutoff = 0.7
confusionMatrix(table(predict(modelo_atrasos, type = "response") >= 0.7,
                      Atrasado$atrasado == 1)[2:1, 2:1])



# Didatico: IGUALANDO OS CRITÉRIOS DE ESPECIFICIDADE E DE SENSITIVIDADE -------------
# Tentaremos estabelecer um critério que iguale a probabilidade de acerto
# daqueles que chegarão atrasados (sensitividade) e a probabilidade de acerto
# daqueles que não chegarão atrasados (especificidade).

# ATENÇÃO: o que será feito a seguir possui fins didáticos, apenas. DE NENHUMA
# FORMA o procedimento garante a maximização da acurácia do modelo!

# Criar objeto com os dados necessários para a futura plotagem da curva ROC.
predicoes <- ROCR::prediction(predictions = modelo_atrasos$fitted.values, 
                        labels = as.factor(Atrasado$atrasado))

# Extrair do objeto 'predicoes' os dados de sensitividade e de especificidade
# para a plotagem.
dados_curva_roc <- ROCR::performance(predicoes, measure = "sens") 

# Desejamos os dados da sensitividade e de especificidade. Então, devemos
# digitar os seguintes códigos:

sensitividade <- (performance(predicoes, measure = "sens"))@y.values[[1]] 

especificidade <- (performance(predicoes, measure = "spec"))@y.values[[1]]

# Extraindo os cutoffs:
cutoffs <- dados_curva_roc@x.values[[1]] 

# Até o momento, foram extraídos 3 vetores: 'sensitividade', 'especificidade' 
# e 'cutoffs'. Poder-se-ia plotar normalmente a partir daqui com a linguagem 
# base do R, mas demos preferência à ferramenta ggplot2. Assim, criamos um data 
# frame que contém os vetores mencionados.
 
dados_plotagem <- cbind.data.frame(cutoffs, especificidade, sensitividade)

# Visualizando o novo dataframe dados_plotagem
dados_plotagem %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 22)

# Plotando:
ggplotly(dados_plotagem %>%
           ggplot(aes(x = cutoffs, y = especificidade)) +
           geom_line(aes(color = "Especificidade"),
                     size = 1) +
           geom_point(color = "#95D840FF",
                      size = 1.9) +
           geom_line(aes(x = cutoffs, y = sensitividade, color = "Sensitividade"),
                     size = 1) +
           geom_point(aes(x = cutoffs, y = sensitividade),
                      color = "#440154FF",
                      size = 1.9) +
           labs(x = "Cutoff",
                y = "Sensitividade/Especificidade") +
           scale_color_manual("Legenda:",
                              values = c("#95D840FF", "#440154FF")) +
           theme_bw())




# Qualidade do Modelo: Curva ROC ---------------------------------------------------------------
# Receiver Operating Characteristic: Indicador de eficiencia de modelos
# As métricas de Acuracia, Sensibilidade, Especificidade são dependentes do 
# cutoff (corte) da probabilidade do evento ocorrer. Já a curva ROC independe
# de cutoff. 

# A curva ROC = sensitividade vs 1- especificidade
# O indicador da curva é a área embaixo da curva ROC. Quanto maior a área, 
# maior o valor, e, consequentemente, melhor é o modelo

# Predição do Modelo
predicoes <- ROCR::prediction(predictions = modelo_atrasos$fitted.values, 
                              labels = as.factor(Atrasado$atrasado))

# Extrair do objeto 'predicoes' os dados de sensitividade e de especificidade
# para a plotagem.
dados_curva_roc <- ROCR::performance(predicoes, measure = "sens") 

# Desejamos os dados da sensitividade e de especificidade. Então, devemos
# digitar os seguintes códigos:

sensitividade <- (ROCR::performance(predicoes, measure = "sens"))@y.values[[1]] 

especificidade <- (ROCR::performance(predicoes, measure = "spec"))@y.values[[1]]


# Calcular a ROC
ROC <- pROC::roc(response = Atrasado$atrasado, 
                predictor = modelo_atrasos$fitted.values)

# Plotagem da curva ROC propriamente dita
ggplot() +
  geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1),
               color = "grey40", size = 0.2) +
  geom_line(aes(x = 1 - especificidade, y = sensitividade),
            color = "darkorchid", size = 2) +
  labs(x = "1 - Especificidade",
       y = "Sensitividade",
       title = paste("Área abaixo da curva:",
                     round(ROC$auc, 4),
                     "|",
                     "Coeficiente de Gini:",
                     round((ROC$auc[1] - 0.5) / 0.5, 4))) +
  theme(panel.background = element_rect(NA),
        panel.border = element_rect(color = "black", fill = NA),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10)
  )


# Qualidade do Modelo: GINI --------------------------------------------------------------------
# Gini = (ROC - 0.5)/0.5



# Qualidade do Modelo: AIC | BIC ------------------------------------------
# AIC = 106.933, BIC = 114.748 -> Akaike Info Criterion e Bayesian Info Criterion
#                                 usados para ver o ajuste dos modelos. Quanto
#                                 menor o valor, melhor o modelo. Muito usam o 
#                                 BIC para comparar modelos com quantidade de variáveis
#                                 diferentes, pq leva em consideração essa diferença
#                                 Eles penalizam modelos com muitos betas



# Comparação de modelos ---------------------------------------------------
modelo_nulo <- glm(formula = atrasado ~ 1, 
                      data = Atrasado, 
                      family = "binomial")
modelo_atrasos <- glm(formula = atrasado ~ dist + sem, 
                      data = Atrasado, 
                      family = "binomial")

# Comparação usando ANOVA 
# Se os modelos são diferentes (p<0.05), escolher o modelo mais complexo
# Se os modelos não são diferentes (p>0.05), manter o modelo mais simples
anova(modelo_nulo,modelo_atrasos, test= "Chi") # p <0.05 - sigo modelo_atrasos






#                #              #             #              #            #
# x -----------------------------------------------------------------------
# EXEMPLO 2: Stepwise --------------------------------------------------------------
# Utilizando o Procedimento Stepwise para a seleção de variáveis explicativas 
# na regressão logistica binária


# Dados -------------------------------------------------------------------
# BASE DE DADOS CHALLENGER
load("raw_datas_exemplos/challenger.RData")

# Objetivo: Avaliar a chance de ocorrência de falha no anel de vedação de uma
# espaçonave.

# Exploratorio ------------------------------------------------------------
challenger %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 22)

# desgaste: quantidade de vezes em que ocorreu stress térmico
# temperatura: temperatura de lançamento (graus ºF)
# pressão: pressão de verificação de vazamento (psi-libra-força por polegada ao quadrado)
# t: teste para o lançamento (id) - ensaios feitos antes do lançamento

summary(challenger)


# Obtendo dado Qualitativo para var. resposta ----------------------------------
# Não há uma variável binária para servir como uma variável dependente, certo?
# A quantidade de desgaste será substituído por ocorrencia de falha (sim ou não)
# Então vamos criá-la considerando a ocorrência de desgastes de peças como a
# ocorrência de um evento que chamaremos de 'falha':
challenger %>%
  mutate(falha = ifelse(desgaste > 0,
                        yes = "sim",
                        no = "não"),
         falha = factor(falha)) -> challenger

# Vamos observar as alterações na base de dados original:
challenger %>%
  select(desgaste, falha, everything()) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = F,
                font_size = 22)


# Função: Transformar temp farenhite em Celsios -------------------------------------
celsius <- function(far){
  celsius = 5*((far-32)/9)
  print(celsius)
}


# Estimando o modelo logístico binário ------------------------------------
modelo_challenger <- glm(formula = falha ~ . -desgaste -t, # menos id e quantidade de desgaste 
                         data = challenger,
                         family = "binomial")

# Parâmetros do modelo_default
summary(modelo_challenger)

# Note que não há a explicitação da estatística geral do modelo,
# nem tampouco do valor de LL e dos intervalos de confiança.

# Uma solução rápida para o caso pode ser a utilização da função summ do pacote jtools
summ(model = modelo_challenger, confint = T, digits = 4, ci.width = 0.95)
export_summs(modelo_challenger, scale = F, digits = 4)


# Stepwise ----------------------------------------------------------------
step_challenger <- step(object = modelo_challenger,
                        k = qchisq(p = 0.05, df = 1, lower.tail = FALSE))

# Parâmetros do modelo step_challenger
summ(model = step_challenger, confint = T, digits = 4, ci.width = 0.95)


# INTERPRETAÇÃO
# Coefficients:
#                 Estimate     Std. Error z value Pr(>|z|)  
# (Intercept)     21.843631  11.936459   1.830   0.0673 .
# temperatura    -0.350098   0.172977  -2.024   0.0430 *
#   pressão      0.006007   0.009749   0.616   0.5378 


# Quanto maior a temperatura, menor é a chance de falha (do evento acontecer)
# Quanto menor a temperatura, maior é a chance de falha
# Isso por conta do sinal negativo na frente do estimate



# Predição ----------------------------------------------------------------
# Fazendo predições para o modelo step_challenger:
# Exemplo 1: qual a probabilidade média de falha a 70ºF (~21ºC)?
predict(object = step_challenger,
        data.frame(temperatura = 70),
        type = "response")

# Exemplo 2: qual a probabilidade média de falha a 77ºF (25ºC)?
predict(object = step_challenger,
        data.frame(temperatura = 77),
        type = "response")

# Exemplo 3: qual a probabilidade média de falha a 34ºF (~1ºC) - manhã do lançamento?
predict(object = step_challenger,
        data.frame(temperatura = 34),
        type = "response")

# Construção da sigmoide - probabilidade de evento em função da variável 'temperatura'
ggplotly(
  challenger %>% 
    mutate(phat = predict(object = step_challenger,  # modelo
                          newdata = challenger,      # dados usados
                          type = "response"),
           falha = as.numeric(falha) - 1) %>% 
    ggplot() +
    geom_point(aes(x = temperatura, y = falha), color = "#95D840FF", size = 2) +
    geom_smooth(aes(x = temperatura, y = phat), 
                method = "glm", formula = y ~ x, 
                method.args = list(family = "binomial"), 
                se = F,
                color = "#440154FF", size = 2) +
    labs(x = "Temperatura",
         y = "Falha") +
    theme_bw()
)

# Nossa homenagem aos astronautas
image_scale(image_read("https://img.ibxk.com.br///2016/01/29/29182307148581.jpg?w=1200&h=675&mode=crop&scale=both"),"x320")



# Curva ROC ---------------------------------------------------------------

curva_roc <- roc(response = challenger$falha,
                predictor = step_challenger$fitted.values) 

curva_roc$auc
# Area under the curve: 0.8627

# Interpretação: Esse não é um modelo ruim para prever o evento, já que está 
# proximo ao valor 1.


# Gráfico ROC -------------------------------------------------------------
ggplotly(
  ggroc(curva_roc, color = "blue")
)


# x -----------------------------------------------------------------------
# Exemplo 3 ---------------------------------------------------------------
# Regres. Logistica Binaria com quali e quantitativa  ---------------------


# Dados -------------------------------------------------------------------
load("raw_datas_exemplos/dados_fidelidade.RData")


# Exploratorio ------------------------------------------------------------
# Visualizando a base de dados dados_fidelidade
dados_fidelidade %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = F,
                font_size = 13)

# Estatísticas Univariadas da Base de Dados
summary(dados_fidelidade)

# Tabela de frequências absolutas das variáveis qualitativas referentes aos
# atributos da loja na percepção dos consumidores
table(dados_fidelidade$atendimento)
table(dados_fidelidade$sortimento)
table(dados_fidelidade$acessibilidade)
table(dados_fidelidade$preço)

# Note que as variáveis qualitativas já estão como fator (fct)
glimpse(dados_fidelidade)



# Sem Dummizar -----------------------------------------------------------------
# Temos que dumizar as variáveis categoricas manualmente para usar o stepwise
# Embora, se não dumizar, o glm dumiza de forma implicita dentro do modelo
# as variáveis que estão como factor. Então, ok. não dumizar as variáveis 
# qualitativa (factor) para o primeiro output. Mas para rodar o stepwise
# temos que fazer a dumização para o processo surtir efeito

# Estimar Modelo ----------------------------------------------------------
modelo_fidelidade <- glm(formula = fidelidade ~ . - id, 
                         data = dados_fidelidade, 
                         family = "binomial")

# Parâmetros do modelo_fidelidade
summary(modelo_fidelidade)

# Outro modo de apresentar os outputs do modelo_fidelidade
summ(modelo_fidelidade, confint = T, digits = 3, ci.width = .95)
export_summs(modelo_fidelidade, scale = F, digits = 6)


# Stepwise ----------------------------------------------------------------
# Procedimento Stepwise
step_fidelidade <- step(object = modelo_fidelidade,
                        k = qchisq(p = 0.05, df = 1, lower.tail = FALSE))

#Parâmetros do modelo step_fidelidade
summary(step_fidelidade)
# Note que sem a dummização, o R consegue calcular corretamente os parâmetros,
# mas o procedimento Stepwise, quando aplicado, não surte efeitos!



# Dummies -----------------------------------------------------------------
# Dummizando as variáveis atendimento, sortimento, acessibilidade e preço. O 
# código abaixo, automaticamente, fará: a) a dummização das variáveis originais;
# b)removerá as variáveis dummizadas originais; c) estabelecerá como categorias 
# de referência as categorias de label 1 de cada variável original.
fidelidade_dummies <- dummy_columns(.data = dados_fidelidade,
                                    select_columns = c("atendimento", 
                                                       "sortimento",
                                                       "acessibilidade", 
                                                       "preço"),
                                    remove_selected_columns = T,
                                    remove_first_dummy = T)

# Atenção!!!! Note que a variável Sexo não entrou no processo de dumização
# porque por ser 2 categorias, ele já é 0 e 1 (ou seja, dumizada). Mas poderia
# ter incluído tb, pois não faria diferença.


# Restimando o Modelo -----------------------------------------------------
# Visualizando a base de dados fidelidade_dummies
fidelidade_dummies %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 16)

# Modelo
modelo_fidelidade_dummies <- glm(formula = fidelidade ~ . -id, 
                                 data = fidelidade_dummies, 
                                 family = "binomial")

# Parâmetros do modelo_fidelidade_dummies
summary(modelo_fidelidade_dummies)

# Valor do LL do modelo_fidelidade_dummies
logLik(modelo_fidelidade_dummies)

# Procedimento Stepwise
step_fidelidade_dummies <- step(object = modelo_fidelidade_dummies,
                                k = qchisq(p = 0.05, df = 1, lower.tail = FALSE))

# Parâmetros do modelo step_fidelidade_dummies
summary(step_fidelidade_dummies)

# Outro modo de apresentar os outputs do modelo step_fidelidade_dummies
summ(step_fidelidade_dummies, confint = T, digits = 3, ci.width = .95)
export_summs(step_fidelidade_dummies, scale = F, digits = 6)



# Extrair Equação ----------------------------------------------------------
extract_eq(step_fidelidade_dummies, use_coefs = T,
           wrap = T, show_distribution = T) |> 
  kable() |> 
  kable_styling(bootstrap_options = "striped",
              full_width = F,
              font_size = 25)


# logLik ------------------------------------------------------------------
# Valor do LL do modelo step_fidelidade_dummies
logLik(step_fidelidade_dummies)



# Comparando Modelos ------------------------------------------------------
# Comparando os modelos step_fidelidade_dummies e modelo_fidelidade_dummies
# Avalia o Ganho ou perda de LogLik ao tirar ou acrescentar os preditores
lmtest::lrtest(modelo_fidelidade_dummies, step_fidelidade_dummies)

# Resultado: 
# Df  LogLik Df  Chisq Pr(>Chisq)
# 1  19 -773.57                     
# 2  18 -773.60 -1 0.0738     0.7859

# Interpretação: O qui-quadrado mostra que não tem diferença signicantes entre
# os modelos

# Comparar tabelas dos modelos
export_summs(modelo_fidelidade_dummies, step_fidelidade_dummies,
             model.names = c("Modelo Dummies","Modelo Dummies Stepwise"),
             scale = F, digits = 4)



# Matriz de Confusão ------------------------------------------------------
confusionMatrix(
  table(predict(step_fidelidade_dummies, type = "response") >= 0.5, 
        dados_fidelidade$fidelidade == "sim")[2:1, 2:1])



# IGUALANDO OS CRITÉRIOS DE ESPECIFICIDADE E DE SENSITIVIDADE -------------
# Analogamente ao realizado para o Exemplo 01, vamos estabelecer um critério
# que iguale a probabilidade de acerto daqueles que apresentarão fidelização ao
# estabelecimento varejista (sensitividade) e a probabilidade de acerto daqueles
# que não apresentarão fidelização (especificidade).

# ATENÇÃO: o que será feito a seguir possui fins didáticos, apenas. DE NENHUMA
# FORMA o procedimento garante a maximização da acurácia do modelo!

# função prediction do pacote ROCR
# a função prediction cria um objeto com os dados necessários
# para a futura plotagem da curva ROC
predicoes <- ROCR::prediction(predictions = step_fidelidade_dummies$fitted.values, 
                        labels = dados_fidelidade$fidelidade) 

# função performance do pacote ROCR
# A função peformance() extraiu do objeto 'predicoes' os 
# dados de sensitividade, de sensibilidade e de especificidade para a plotagem.
dados_curva_roc <- ROCR::performance(predicoes, measure = "sens") 


# Porém, desejamos os dados da sensitividade, então devemos fazer o seguinte 
# ajuste:
sensitividade <- dados_curva_roc@y.values[[1]] 
# extraindo dados da sensitividade do modelo

especificidade <- performance(predicoes, measure = "spec") 
#extraindo os dados da especificidade, mas também há que se fazer um ajuste para a 

# plotagem:
especificidade <- especificidade@y.values[[1]]

cutoffs <- dados_curva_roc@x.values[[1]] 
# extraindo os cutoffs do objeto 'sensitividade'.

# Até o momento, foram extraídos 3 vetores: 'sensitividade', 'especificidade' 
# e 'cutoffs'. Poder-se-ia plotar normalmente a partir daqui com a linguagem 
# base do R, mas demos preferência à ferramenta ggplot2. Assim, criamos um data 
# frame que contém os vetores mencionados.

dados_plotagem <- cbind.data.frame(cutoffs, especificidade, sensitividade)

# Visualizando o novo dataframe dados_plotagem
dados_plotagem %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 22)

# Plotando:
ggplotly(dados_plotagem %>%
           ggplot(aes(x = cutoffs, y = especificidade)) +
           geom_line(aes(color = "Especificidade"),
                     size = 1) +
           geom_point(color = "#95D840FF",
                      size = 1.9) +
           geom_line(aes(x = cutoffs, y = sensitividade, color = "Sensitividade"),
                     size = 1) +
           geom_point(aes(x = cutoffs, y = sensitividade),
                      color = "#440154FF",
                      size = 1.9) +
           labs(x = "Cutoff",
                y = "Sensitividade/Especificidade") +
           scale_color_manual("Legenda:",
                              values = c("#95D840FF", "#440154FF")) +
           theme_bw())



# Curva Roc ---------------------------------------------------------------
# Para aumentar a area embaixo da curva, somente acrescentando variáveis que 
# explicam o efeito. Não é possível aumentar a area atraves do aumento amostral
# ou outra forma.
ROC <- roc(response = dados_fidelidade$fidelidade, 
           predictor = step_fidelidade_dummies$fitted.values)


# Plotagem da curva ROC propriamente dita
ggplot() +
  geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1),
               color = "grey40", size = 0.2) +
  geom_line(aes(x = 1 - especificidade, y = sensitividade),
            color = "darkorchid", size = 2) +
  labs(x = "1 - Especificidade",
       y = "Sensitividade",
       title = paste("Área abaixo da curva:",
                     round(ROC$auc, 4),
                     "|",
                     "Coeficiente de Gini:",
                     round((ROC$auc[1] - 0.5) / 0.5, 4))) +
  theme(panel.background = element_rect(NA),
        panel.border = element_rect(color = "black", fill = NA),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10)
  )


# xx ----------------------------------------------------------------------
# Supondo unica variável --------------------------------------------------
modelo_preliminar <- glm(fidelidade ~ idade, data = dados_fidelidade,
                         family = "binomial")
summary(modelo_preliminar)

ROC_preliminar <- roc(response = dados_fidelidade$fidelidade,
                            predictor = modelo_preliminar$fitted.values)
ROC_preliminar

logLik(modelo_preliminar)
logLik(step_fidelidade_dummies)

# testando se os modelos são diferentes
lrtest(modelo_preliminar, step_fidelidade_dummies)

# Resultado:
# #Df  LogLik Df  Chisq Pr(>Chisq)    
# 1   2 -1693.5                         
# 2  18  -773.6 16 1839.8  < 2.2e-16 ***

# Interpretação: Se o p deu sginificativa significa que o modelo_preliminar
# que só tinha a idade se beneficiou da adição de novas variáveis que ocorreu 
# no modelo step_fidelidade_dummies. Pois os modelos foram estatisticamente 
# diferentes


# Comparação: Graficos Roc  -------------------------------------------------
plot(ROC_preliminar, col= "orange", lty = 2, main = "comparação entre ROCs")
plot(ROC, col= "blue", lty = 1, add = T)

# Comparação: Áreas Roc  -------------------------------------------------
# Teste de DeLong para comparar area abaixo do curva roc
pROC::roc.test(ROC,ROC_preliminar)

#Interpretação: p<0.05 existe diferença significativa entre as áreas abaixo da
# curva ROC entre os dois modelos

# DeLong's test for two correlated ROC curves
# 
# data:  ROC and ROC_preliminar
# Z = 37.206, p-value < 2.2e-16
# alternative hypothesis: true difference in AUC is not equal to 0
# 95 percent confidence interval:
#  0.2424050 0.2693644
# sample estimates:
# AUC of roc1 AUC of roc2 
#   0.9557630   0.6998782 
