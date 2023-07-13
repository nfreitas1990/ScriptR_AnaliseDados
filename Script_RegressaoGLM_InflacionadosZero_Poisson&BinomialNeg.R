

# Natália Freitas
# Modelos Inflacionados de Zero

# São de dois tipos:
# Modelos Inflacionados de zero do tipo poisson 
# Modelos Inflacionados de zero do tipo binomial negativo

# Esse script é uma continuação do Script 2023_RegressaoGLM_Poisson e 
# 2023_RegressaoGLM_BinomialNegativa



# Contextualização --------------------------------------------------------
#> Esses modelos são considerados uma combinação entre um modelo para dados
#> de contagem e um modelo para dados binários, já que são utilizados para
#> investigar as razões que levam a determinada quantidade de ocorrências
#> (contagens) de um fenomeno, bem como as razões que levam (ou não) à 
#> ocorrência propriamente dita deste fenômeno, independente da quantidade de
#> contagens observadas. 
#> Enquanto um modelo Poisson inflacionados de zeros é estimado a partir da 
#> combinação de uma distribuição de bernoulli com uma distribuição Poisson, um
#> modelo binomial negativo inflacionado de zeros é estimado por meio da 
#> combinação de uma distribuição bernoulli com uma distribuição binomial negativa
#> (poisson-gamma)


#                  Poisson | Binomial | Poisson Inflacionado| Binomial Negativo    
#                          | Negativa |  de zeros (ZIP)     | Inflacionado de zeros
#                          |          |                     | (ZINB)               
# _________________________________________________________________________________
# Superdispersão  |  Não   |  Sim     |     Não             |  Sim                 
# nos dados da    |        |          |                     |                      
# variável Y      |        |          |                     |                      
# _________________________________________________________________________________
# Quantidade      |        |          |                     |                      
# Excessiva de    |        |          |                     |                      
# Zeros variável  |Não     |    Não   |           Sim       |   Sim                
# dependente (y)  |        |          |                     |                      


#   - Poisson :          sem superdispersão | sem excessos de zero
#   - Binomial Negativo: com superdispersão | sem excessos de zero
#   - ZIP :              sem superdispersão | sem excessos de zero
#   - ZINB :             com superdispersão | com excessos de zero







# x -----------------------------------------------------------------------
# x -----------------------------------------------------------------------
# x -----------------------------------------------------------------------

# Zero Inflated Poisson (ZIP) ---------------------------------------------


# Referencia --------------------------------------------------------------
# LAMBERT, D. Zero-inflated Poisson regression, with an application to defects
# in manufacturing. Technometrics, v. 34, n. 1, p. 1-14, 1992.



# Teste de Voung ----------------------------------------------------------
# Usado para definir a existencia, ou não de uma quantidade excessiva de zeros
# na variável dependente Y. O teste representará um importante output a ser 
# analisado na estimação de modelos de regressão para dados de contagem, quando
# houver a suspeita de existência de inflação de zeros.



# Modelos Inflacionados de Zero do Tipo Poisson --------------------------
# Podemos definir que:
# a probabilidade p de ocorrencia de nenhuma contagem para dada observação é
# calculada levando-se em consideração a soma de um componente dicotômico com
# um componente de contagem e, portanto, deve-se definir a probabilidade p.logit
# de não ocorrer nenhuma contagem devido exclusivamente ao componente dicontomico,
# a probabilidade p de ocorrência de determinada contagem m, segue a propria 
# expressão da probabilidade da distribuição de poisson, multiplicada por
# (1-p logit)

# Os modelos de regressão Poisson inflacionados de zeros apresentam dois 
# processos geradores de zeros, sendo um devido a distribuição binária (neste
# caso são gerados os chamdos zeros estruturais) e outro devido a distribuição
# de Poisson (neste caso, são gerados dados de contagem entre os quais os 
# chamados zeros amostrais)


# Distribuição ------------------------------------------------------------

# Exemplo de uma função da distribuição ZI Poisson, com lambda = 1 e 
# plogit = 0,7
zip_lambda1_plogit07 <- function(m){
  lambda <- 1
  plogit <- 0.7
  ifelse(m == 0, 
         yes = (plogit) + ((1 - plogit) * exp(-lambda)),
         no = (1 - plogit) * ((exp(-lambda) * lambda ^ m) / factorial(m)))
}


# Comparando as distribuições Poisson, BNeg e ZIP -------------------------
data.frame(m = 0:20) %>% 
  ggplot(aes(x = m)) +
  stat_function(fun = poisson_lambda1, size = 0.7, 
                aes(color = "Poisson: Lambda = 1")) +
  stat_function(fun = poisson_lambda4, size = 0.7, 
                aes(color = "Poisson: Lambda = 4")) +
  stat_function(fun = poisson_lambda10, size = 0.7, 
                aes(color = "Poisson: Lambda = 10")) +
  stat_function(fun = bneg_theta2_delta2, size = 0.7, 
                aes(color = "BNeg: Theta = 2 e Delta = 2")) +
  stat_function(fun = bneg_theta3_delta1, size = 0.7, 
                aes(color = "BNeg: Theta = 3 e Delta = 1")) +
  stat_function(fun = bneg_theta3_delta05, size = 0.7, 
                aes(color = "BNeg: Theta = 3 e Delta = 0,5")) +
  stat_function(fun = zip_lambda1_plogit07, size = 1.5, 
                aes(color = "ZIP: Lambda = 1 e plogit = 0,7")) +
  scale_color_viridis_d("Distribuição:") +
  labs(y = "Probabilidade", x = "m") +
  theme_bw()



# Pacotes -----------------------------------------------------------------
pacotes <- c("plotly","tidyverse","knitr","kableExtra","reshape2","ggrepel",
             "fastDummies","lmtest","splines","jtools","questionr","MASS",
             "pscl","overdisp","magick","cowplot","beepr")

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
load("raw_datas_exemplos/corruption.RData")


# Estimação: Modelo ZIP ---------------------------------------------------

# Modelo ZERO-INFLATED POISSON (ZIP)

# Seleção de Variáveis 
# Não existe a função de stepwise para os modelos inflacionados. Então a 
# seleção será manual.


# Função para estimar o modelo:
modelo_zip <- pscl::zeroinfl(formula = violations ~ corruption + post + staff
                       | corruption,
                       data = corruption,
                       dist = "poisson")


# | pipe : tudo que vier depois do | são as variáveis que podem estar 
# potencializando a qauntidade de zeros. Neste caso, escolhemos só "corruption"
# poderíamos ter escolhidos outras variáveis. Se não colocarmos nada depois
# do pipe | temos um poisson tradicional.

# "|corruption" especifica que o modelo tem um componente de inflação zero que
# é explicado pela variável "corruption".

# Output ------------------------------------------------------------------
# Parâmetros e LogLikelihood do modelo_zip
summary(modelo_zip)
logLik(modelo_zip)
 
# Call:
#   pscl::zeroinfl(formula = violations ~ corruption + post + staff | corruption, data = corruption, dist = "poisson")
# 
# Pearson residuals:
#   Min      1Q  Median      3Q     Max 
# -2.5861 -1.1226 -0.4228 -0.2410 20.5587 
# 
# Count model coefficients (poisson with log link):
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)  2.488857   0.031505  78.999  < 2e-16 ***
#   corruption   0.093714   0.029982   3.126  0.00177 ** 
#   postyes     -4.287651   0.204560 -20.960  < 2e-16 ***
#   staff        0.020020   0.001239  16.163  < 2e-16 ***
#   
#   Zero-inflation model coefficients (binomial with logit link):
#               Estimate Std. Error z value Pr(>|z|)    
#   (Intercept)  -1.6117     0.2437  -6.613 3.77e-11 ***
#   corruption   -0.9524     0.1955  -4.871 1.11e-06 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
# 
# Number of iterations in BFGS optimization: 14 
# Log-likelihood: -1781 on 6 Df


# Count model coefficient
# corruption   0.093714 >>> significa que quanto maior o nivel de corrupção, 
#                           maior é a contagem de violações 

# Zero-inflation model coefficients
# corruption   -0.9524 >>> significa que quanto maior o nivel de corrupção, 
#                           MENOR é a probabilidade de ocorrência de zero 
#                           violações (menor é a contagem de zero)
 

# Os coeficientes do modelo são apresentados separadamente para o modelo de contagem 
# (poisson com link log) e o modelo de inflação zero (binomial com link logit).
# Os coeficientes estimados para cada variável preditora indicam a relação entre
# cada uma das variáveis e a probabilidade de haver uma contagem zero de 
# "violations" ou a contagem de "violations" quando ela é maior que zero.



# lambda -------------------------------------------------------------------------
lambda = (1-(1/(1+exp-(alfa+ b1*corruption))))* (exp(alfa+b1x1+b2x2...))



# Teste de Vuong ----------------------------------------------------------
# VUONG, Q. H. Likelihood ratio tests for model selection and non-nested
# hypotheses. Econometrica, v. 57, n. 2, p. 307-333, 1989.

# O teste é usado para verificar se existe inflação de zero

# Estimar modelo poisson para comparar 
modelo_poisson <- glm(formula = violations ~ staff + post + corruption,
                      data = corruption,
                      family = "poisson")

vuong(m1 = modelo_poisson, m2 = modelo_zip)

# Vuong Non-Nested Hypothesis Test-Statistic: 
#   (test-statistic is asymptotically distributed N(0,1) under the
#    null that the models are indistinguishible)
# -------------------------------------------------------------
#   Vuong            z-statistic       H_A        p-value
# Raw                   -2.987812 model2 > model1 0.0014049
# AIC-corrected         -2.967241 model2 > model1 0.0015024
# BIC-corrected         -2.929214 model2 > model1 0.0016991

# Interpretação: Olhando para o "raw" vemos que p<0.05 então temos indicios de
# que ocorre inflação de zero


# Comparando os LL dos modelos Poisson e ZIP
data.frame(LL_Poisson = round(logLik(modelo_poisson), 1),
           LL_ZIP = round(logLik(modelo_zip), 1)) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", position = "center",
                full_width = F,
                font_size = 30)

# Likelihoo-ratio test
lmtest::lrtest(modelo_poisson, modelo_zip)

# Interpretação: ao comparar o log-lik dos modelos, percebemos que houve um 
# ganho (p<0.05):
# 2   6 -1781.3  2 580.97  < 2.2e-16 ***
# mostra que devemos ficar o modelo 2 que tem a inflação.
# (violations ~ corruption + post + staff | corruption) 



# Grafico: Comparação de Modelos ------------------------------------------
# Para comparar o modelo Poisson, Binomial Negativo e ZIP 
modelo_bneg <- MASS::glm.nb(formula = violations ~ staff + post + corruption,
                      data = corruption) 

data.frame(Poisson = logLik(modelo_poisson),
           ZIP = logLik(modelo_zip),
           BNeg = logLik(modelo_bneg)) %>% 
  melt() %>% 
  ggplot(aes(x = variable, y = value)) +
  geom_bar(aes(fill = factor(variable)), 
           stat = "identity",
           color = "black") +
  geom_text(aes(label = format(value, digts = 3)), 
            color = "black", 
            size = 3.7,
            vjust = -0.5,
            angle = 90) +
  scale_fill_manual("Legenda:", values = c("#440154FF", "#453781FF", "orange")) +
  coord_flip() +
  labs(x = "Estimação",
       y = "Log-Likelihood") +
  theme_bw()

# ZIP é melhor do que Poisson. Mas Binomial Negativo é melhor do que ambos.
# A intenção é sempre achar o maior valor de LogLike. Entretanto, cuidado, essa
# escala está em negativo, então neste grafico as maiores barras representam 
# os menores valores.



# Comparação de previsões -------------------------------------------------

# Supondo que considerássemos a estimação ZIP como a mais adequada, qual seria a 
# quantidade média esperada de violações de trânsito para um país cujo corpo 
# diplomático seja composto por 23 membros, considerando o período anterior à 
# vigência da lei e cujo índice de corrupção seja igual a 0.5?


# Modelo Poisson:
predict(object = modelo_poisson, 
        newdata = data.frame(staff = 23,
                             post = "no",
                             corruption = 0.5),
        type = "response")

# Modelo Binomial Negativo:
predict(object = modelo_bneg, #linha 275 deste script
        newdata = data.frame(staff = 23,
                             post = "no",
                             corruption = 0.5),
        type = "response")

# Modelo ZIP:
predict(object = modelo_zip,
        newdata = data.frame(staff = 23,
                             post = "no",
                             corruption = 0.5), 
        type = "response")




# Calculo Manual do fit Modelo ZIP: predict -----------------------------
summary(modelo_zip)
(1-(1/ (1+ exp(-(-1.6117-0.9524*0.5)))))*
  (exp(2.488857+0.020020*23-4.2876510*0+ 0.093714*0.5))




# Qual seria a quantidade média esperada de violações de trânsito para o mesmo
# país ao se considerar o início da vigência da lei?

# Modelo Poisson:
predict(object = modelo_poisson,
        newdata = data.frame(staff = 23,
                             post = "yes",
                             corruption = 0.5),
        type = "response")

# Modelo Binomial Negativo:
predict(object = modelo_bneg,
        newdata = data.frame(staff = 23,
                             post = "yes",
                             corruption = 0.5),
        type = "response")

#Modelo ZIP:
predict(object = modelo_zip,
        newdata = data.frame(staff = 23,
                             post = "yes",
                             corruption = 0.5),
        type = "response")


# x -----------------------------------------------------------------------
# x -----------------------------------------------------------------------
# x -----------------------------------------------------------------------


# Zero Inflated Binomial Negativa -----------------------------------------

#> Os modelos de regressão do tipo binomial netivo inflacionados de zeros,
#> podemos definir que, enquanto a probabilidade p de ocorrência de nenhuma 
#> contagem para dada observação i, ou seja, p(Yi=0) é também calculada levando
#> em consideração a soma de um componente dicotomico com um componente de 
#> contagem, a probabiliade o de ocorrência de determinada contagem m(m=1,2,3..)
#> , ou seja, p(Yi=m), segue agora a expressão da probabilidade da distribuição
#>  Poisson-Gama

#> Esta distribuição tem a capacidade de capturar a inflação de zero (como a ZIP)
#> Mas ao mesmo tempo consegue capturar a cauda longa (como a binomial negativa).



# Distribuição ------------------------------------------------------------

# Exemplo de uma função da distribuição ZI Binomial Negativa, com theta = 2,
# delta = 2, plogit = 0,7 e lambda_bneg = 2
zinb_theta2_delta2_plogit07_lambda2 <- function(m){
  theta <- 2
  delta <- 2
  plogit <- 0.7
  lambda_bneg <- 2
  ifelse(m == 0,
         yes = (plogit) + ((1 - plogit) * (((1) / (1 + 1/theta * lambda_bneg)) ^ theta)),
         no = (1 - plogit) * ((delta ^ theta) * (m ^ (theta - 1)) * 
                                (exp(-m * delta))) / factorial(theta - 1))
}

# Comparando as distribuições Poisson, BNeg, ZIP e ZINB
data.frame(m = 0:20) %>% 
  ggplot(aes(x = m)) +
  stat_function(fun = poisson_lambda1, linewidth = 0.7, 
                aes(color = "Poisson: Lambda = 1")) +
  stat_function(fun = poisson_lambda4, linewidth = 0.7, 
                aes(color = "Poisson: Lambda = 4")) +
  stat_function(fun = poisson_lambda10, linewidth = 0.7, 
                aes(color = "Poisson: Lambda = 10")) +
  stat_function(fun = bneg_theta2_delta2, linewidth = 0.7, 
                aes(color = "BNeg: Theta = 2 e Delta = 2")) +
  stat_function(fun = bneg_theta3_delta1, linewidth = 0.7, 
                aes(color = "BNeg: Theta = 3 e Delta = 1")) +
  stat_function(fun = bneg_theta3_delta05, linewidth = 0.7, 
                aes(color = "BNeg: Theta = 3 e Delta = 0,5")) +
  stat_function(fun = zip_lambda1_plogit07, linewidth = 0.7, 
                aes(color = "ZIP: Lambda = 1 e plogit = 0,7")) +
  stat_function(fun = zinb_theta2_delta2_plogit07_lambda2, linewidth = 1.5, 
                aes(color = "ZINB: Theta = 2, Delta = 2 e plogit = 0,7")) +
  scale_color_viridis_d("Distribuição:") +
  labs(y = "Probabilidade", x = "m") +
  theme_bw()

# codigo integrado com os scripts 2023_regressaoGLM_Poisson / BinomialNegativo



# Estimando Modelo --------------------------------------------------------
modelo_zinb <- pscl::zeroinfl(formula = violations ~ corruption + post + staff
                        | corruption,
                        data = corruption,
                        dist = "negbin")



# Parâmetros e LogLik -----------------------------------------------------
summary(modelo_zinb)
logLik(modelo_zinb)
modelo_zinb$theta
1/modelo_zinb$theta #phi



# Teste de Vuong (1989) ---------------------------------------------------
vuong(m1 = modelo_bneg, 
      m2 = modelo_zinb)

# Comparando os LL dos modelos Bneg e ZINB --------------------------------
data.frame(LL_Bneg = round(logLik(modelo_bneg), 2),
           LL_ZINB = round(logLik(modelo_zinb), 2)) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", position = "center", 
                full_width = F, 
                font_size = 30)



# Likelihoo-ratio test ----------------------------------------------------
lrtest(modelo_bneg, modelo_zinb)


# Gráfico Comparativo de Modelos ------------------------------------------
my_plot2 <-
  data.frame(Poisson = logLik(modelo_poisson),
             ZIP = logLik(modelo_zip),
             Bneg = logLik(modelo_bneg),
             ZINB = logLik(modelo_zinb)) %>%
  melt() %>%
  ggplot(aes(x = variable, y = value)) +
  geom_bar(aes(fill = factor(variable)), 
           stat = "identity",
           color = "black") +
  geom_text(aes(label = format(value, digts = 3)),
            color = "black",
            size = 3.5,
            vjust = -0.5,
            angle = 90) +
  scale_fill_manual("Legenda:", values = c("#440154FF", "#453781FF",
                                           "orange", "#FDE725FF")) +
  coord_flip() +
  labs(x = "Estimação",
       y = "Log-Likelihood") +
  theme_cowplot()
my_plot2

# Modelo ZINB apresenta melhor desempenho. Foi capaz de captar tanto a superdispersão
# quanto a inflação de zero. 



# Com JPEG
ggdraw() +
  draw_image("https://i.pinimg.com/originals/4a/ac/99/4aac9978c444c55cd462fd92c8ac400e.png",
             x = -0.07, y = 0.244, scale = .40) +
  draw_plot(my_plot2)
beep("mario")


# Comparação entre previsões ----------------------------------------------

# Supondo que considerássemos a estimação ZINB como a mais adequada, qual seria a 
# quantidade média esperada de violações de trânsito para um país cujo corpo 
# diplomático seja composto por 23 membros, considerando o período anterior à 
# vigência da lei e cujo índice de corrupção seja igual a 0.5?

# Modelo Poisson:
predict(object = modelo_poisson, #linha 144 deste script
        newdata = data.frame(staff = 23,
                             post = "no",
                             corruption = 0.5),
        type = "response")

# Modelo Binomial Negativo:
predict(object = modelo_bneg, #linha 275 deste script
        newdata = data.frame(staff = 23,
                             post = "no",
                             corruption = 0.5),
        type = "response")

# Modelo ZIP:
predict(object = modelo_zip, #linha 447 deste script
        newdata = data.frame(staff = 23,
                             post = "no",
                             corruption = 0.5), 
        type = "response")

# Modelo ZINB:
predict(object = modelo_zinb,
        newdata = data.frame(staff = 23,
                             post = "no",
                             corruption = 0.5), 
        type = "response")


# Qual seria a quantidade média esperada de violações de trânsito para o mesmo
# país, porém agora considerando a vigência da lei?

# Modelo Poisson:
predict(object = modelo_poisson,
        newdata = data.frame(staff = 23,
                             post = "yes",
                             corruption = 0.5),
        type = "response")

# Modelo Binomial Negativo:
predict(object = modelo_bneg,
        newdata = data.frame(staff = 23,
                             post = "yes",
                             corruption = 0.5),
        type = "response")

# Modelo ZIP:
predict(object = modelo_zip,
        newdata = data.frame(staff = 23,
                             post = "yes",
                             corruption = 0.5),
        type = "response")

# Modelo ZINB:
predict(object = modelo_zinb,
        newdata = data.frame(staff = 23,
                             post = "yes",
                             corruption = 0.5), 
        type = "response")


# Comparação: Adicionando Fitted Values  ----------------------------------
# Adicionando os fitted values dos modelos estimados para fins de comparação
corruption %>%
  mutate(fitted_poisson = modelo_poisson$fitted.values,
         fitted_bneg = modelo_bneg$fitted.values,
         fitted_zip = modelo_zip$fitted.values,
         fitted_zinb = modelo_zinb$fitted.values) %>% 
  dplyr::select(country, code, violations, fitted_poisson, 
                fitted_bneg, fitted_zip, fitted_zinb) %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F,
                font_size = 16)


# Fitted values dos modelos POISSON, BNEG, ZIP e ZINB, considerando, para fins
# didáticos, a variável dependente 'violations' em função apenas da variável
# preditora 'staff'
ggplotly(
  corruption %>%
    ggplot() +
    geom_point(aes(x = staff, y = violations), alpha = 0.5, size = 2) +
    geom_smooth(aes(x = staff, y = modelo_poisson$fitted.values,
                    color = "POISSON"), se = F) +
    geom_smooth(aes(x = staff, y = modelo_bneg$fitted.values,
                    color = "BNEG"), se = F) +
    geom_smooth(aes(x = staff, y = modelo_zip$fitted.values,
                    color = "ZIP"), se = F) +
    geom_smooth(aes(x = staff, y = modelo_zinb$fitted.values,
                    color = "ZINB"), se = F) +
    scale_color_manual("Estimação:",
                       values = c("orange", "#440154FF", "#FDE725FF", "#453781FF")) +
    labs(x = "Number of Diplomats (staff)",
         y = "Unpaid Parking Violations (violations)") +
    theme(panel.background = element_rect("white"),
          panel.grid = element_line("grey95"),
          panel.border = element_rect(NA),
          legend.position = "bottom")
)


# Interpretação: Modelo Poisson e ZIP não consegue capturar bem a cauda longa.
# Mas os modelos ZINB e Binomial negativo consegue capturar a cauda.



# Comparando com OLS ------------------------------------------------------
# Comparando com modelos de mínimos quadrados

modelo_lm <- lm(violations ~ staff+ post+corruption, data = corruption)
summary(modelo_lm)

# Shapiro Francia: testar normalidade dos resíduos
# > Não tem resíduos normal
nortest::sf.test(modelo_lm$residuals)
logLik((modelo_lm))

# Comparar modelos
# > São significativamente diferentes os modelos 
lrtest(modelo_zinb, modelo_lm)


# BoxCox:Vamos transformar para ver se melhora o lm
# Só podemos aplicar boxcox para variável positiva que não contem zero. Então
# caso tenha zero temos que somar 1 ou 0.001
corruption$violations1 <- corruption$violations + 0.001
lambda_BC <- car::powerTransform(corruption$violations1)
lambda_BC

# Inserir o lambda na base de dados para estimar novo modelo
corruption$bc_violations <- (((corruption$violations1^lambda_BC$lambda)-1)/lambda_BC$lambda)

#estimando novamente o modelo
modelo_bc <- lm(bc_violations ~ staff+ post+corruption, data = corruption)
summary(modelo_bc)
nortest::sf.test(modelo_bc$residuals) #não é normal
logLik((modelo_bc))
lrtest(modelo_zinb, modelo_bc)

# Grafico de comparação
my_plot3 <-
  data.frame(Poisson = logLik(modelo_poisson),
             lm = logLik(modelo_lm),
             lm_bc = logLik(modelo_bc),
             ZINB = logLik(modelo_zinb)) %>%
  melt() %>%
  ggplot(aes(x = variable, y = value)) +
  geom_bar(aes(fill = factor(variable)), 
           stat = "identity",
           color = "black") +
  geom_text(aes(label = format(value, digts = 3)),
            color = "black",
            size = 3.5,
            vjust = -0.5,
            angle = 90) +
  scale_fill_manual("Legenda:", values = c("#440154FF", "#453781FF",
                                           "orange", "#FDE725FF")) +
  coord_flip() +
  labs(x = "Estimação",
       y = "Log-Likelihood") +
  theme_cowplot()
my_plot3
