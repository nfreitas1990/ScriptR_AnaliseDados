

# Natália Freitas
# Modelos Lineares


# Esse script é uma continuação do Script 2023_RegressaoGLM_Poisson.
# No caso do teste de superdispersão significativo (p<0.05), significa que 
# temos uma calda longa, e é preferível usar a binomial negativa em detrimento
# da poisson


# Contextualização --------------------------------------------------------

# Modelos de regressão para dados de contagem:
#   - Poisson
#   - Binomial Negativo

# Objetivo: ambos objetivam analisar o comportamento, em função de variáveis 
#           preditoras, de determinada variável dependente que se apresenta na 
#           forma quantitativa,com valores discretos e não negativos.
#           Deve ser definida também a exposição (unidade temporal, espacial,
#           social, etc.)   

# Características:
# - Variável Y Quantitativa
# - Valores Discretos (número inteiros, ou seja, de contagem)
# - Valores Não Negativos (Zero conta como valor)
# - Para dada exposição (por unidade, por tempo, por distrito): precisa definir
#   a exposição para ajudar escolher melhor o modelo


# Característica que diferencia bionomial negativa e poisson: 
# Poisson: assimetria da distribuição com calda curta (teste superdispersão >0.05)
# Binomial Negativa: assimetria da distribuição com calda muito longa (teste
# superdispersão <0.05)


# Função de Ligação:
# Poisson: ln(lambda) 
# Binomial Negativa: ln(lambda)

# Obs: ln é o logaritmo natural, também conhecido como logaritmo neperiano, 
# é o logaritmo de base e, um número irracional aproximadamente igual a 2,71828.
# É definido para todos os números reais estritamente positivos.



# Distribuição Binomial Negativa | Poisson Gamma --------------------------
# Similar a distribuição de Poisson, só que apresenta calda longa na 
# distribuição

#----- Modelo NB1
#> Média = lambda
#> Variancia = lambda +((1/theta)*lambda) 

# Se a variância fosse só lambda, teríamos a poisson. Mas neste caso, temos um
# termo somado ao lambda que corresponde ao termo de superdispersão. Também tem
# na literatura o termo ao quadrado.

#----- Modelo NB2
#> Média = lambda
#> Variancia = lambda +((1/theta)*lambda)2 



# Quando usar? ------------------------------------------------------------
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


# Função da distribuição binomial negativa --------------------------------
# Diferente da distribuição poisson que só tinha 1 parâmetro (lambda), esta
# distribuição tem 2 parâmetros (theta e delta);

# theta: parâmetro de forma da distribuição Poisson-Gama (binomial negativa)
# delta: parâmetro de taxa de decaimento da distribuição Poisson-Gama 

#OBS: o termo (1/theta) é conhecido com phi. Que é um output do R.


# Criando uma função da distribuição binomial negativa, com theta=2 e delta=2
bneg_theta2_delta2 <- function(m){
  theta <- 2
  delta <- 2
  ((delta ^ theta) * (m ^ (theta - 1)) * (exp(-m * delta))) / factorial(theta - 1)
}

# Criando uma função da distribuição binomial negativa, com theta=3 e delta=1
bneg_theta3_delta1 <- function(m){
  theta <- 3
  delta <- 1
  ((delta ^ theta) * (m ^ (theta - 1)) * (exp(-m * delta))) / factorial(theta - 1)
}

# Criando uma função da distribuição binomial negativa, com theta=3 e delta=0,5
bneg_theta3_delta05 <- function(m){
  theta <- 3
  delta <- 0.5
  ((delta ^ theta) * (m ^ (theta - 1)) * (exp(-m * delta))) / factorial(theta - 1)
}

# Plotagem das funções estabelecidas anteriormente
data.frame(m = 1:20) %>%
  ggplot(aes(x = m)) +
  stat_function(fun = bneg_theta2_delta2, 
                aes(color = "Theta igual a 2 e Delta igual a 2"),
                size = 1.5) +
  stat_function(fun = bneg_theta3_delta1, 
                aes(color = "Theta igual a 3 e Delta igual a 1"),
                size = 1.5) +
  stat_function(fun = bneg_theta3_delta05, 
                aes(color = "Theta igual a 3 e Delta igual a 0,5"),
                size = 1.5) +
  scale_color_viridis_d("Valores de" ~ theta ~ "e" ~ delta ~ "") +
  labs(y = "Probabilidades", x = "m") +
  theme_bw()

# Distribuição Binomial Negativa ------------------------------------------
nbinom <- rnbinom (1000, size = 1, mu = 2)
hist(nbinom)


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

# Estimar Modelo: Binomial Negativo ---------------------------------------
# Modelo Binomial do Tipo 2 (NB2) - mais indicado para ser usado
library(MASS)
modelo_bneg <- glm.nb(formula = violations ~ staff + post + corruption,
                      data = corruption)                                                                                   

# Output ------------------------------------------------------------------

# Parâmetros do modelo_bneg
summary(modelo_bneg)




# Extrair Parametros ------------------------------------------------------
# Parâmetro de forma da distribuição binomial negativa

#phi
1 / modelo_bneg$theta 
#theta
modelo_bneg$theta


# Significancia -----------------------------------------------------------
# Estatística z de Wald do parâmetro theta para verificação da
# significância estatística

# Se o valor da conta for maior que 1.96 significa que a 95%
# de nível de confianca o parâmetro de forma theta é significativamente 
# diferente de zero. Ou seja, se isso ocorrer, theta (coponente da superdispersão)
# é diferente de zero. Se theta não for significativamente diferente de zero, 
# o parametro de superdispersão seria anulado, o que significaria que a variancia
# é igual a média, o que seria o mesmo q fazer a distribuição de poisson
# >1.96: binomial negativ
# <1.96: poisson
modelo_bneg$theta / modelo_bneg$SE.theta  



# Extrair LogLike ---------------------------------------------------------
# valor de Log-Likelihood (LL): quanto maior o LL, melhor o modelo
logLik(modelo_bneg)


# Extrair parametros ------------------------------------------------------
# Parâmetros do modelo_bneg
summ(modelo_bneg, digits = 4, confint = T, ci.width = 0.95)
export_summs(modelo_bneg, scale = F, digits = 4)


# Comparar Modelo ---------------------------------------------------------
# Comparando os modelos Poisson e Binomial Negativo

modelo_poisson <- glm(formula = violations ~ staff + post + corruption,
                      data = corruption,
                      family = "poisson")

modelo_bneg <- MASS::glm.nb(formula = violations ~ staff + post + corruption,
                      data = corruption)

# comparar os parâmetros e AIC
export_summs(modelo_poisson, modelo_bneg, scale = F, digits = 4,
             model.names = c("POISSON","BNEG"))

# comparar os loglike
data.frame(LL_Poisson = round(logLik(modelo_poisson), 1),
           LL_Bneg = round(logLik(modelo_bneg), 1)) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", position = "center", 
                full_width = F, 
                font_size = 30)

# Likelihoo-ratio test
lrtest(modelo_poisson, modelo_bneg)



# Comparação: Grafico -----------------------------------------------------
# Gráfico para a comparação dos LL dos modelos Poisson e Binomial Negativo
my_plot <-
  data.frame(Poisson = logLik(modelo_poisson),
             BNeg = logLik(modelo_bneg)) %>% 
  melt() %>% 
  ggplot(aes(x = variable, y = value)) +
  geom_bar(aes(fill = factor(variable)), 
           stat = "identity",
           color = "black") +
  geom_text(aes(label = round(value, digits = 3)), 
            color = "black", 
            size = 3.7,
            vjust = -0.5,
            angle = 90) +
  scale_fill_manual("Legenda:", values = c("#440154FF", "orange")) +
  coord_flip() +
  labs(x = "Estimação",
       y = "Log-Likelihood") +
  theme_cowplot()
my_plot

# Com JPEG
ggdraw() +
  draw_image("https://cdn.pixabay.com/photo/2016/08/21/18/48/emoticon-1610518_960_720.png",
             x = -0.12, y = 0.23, scale = .33) +
  draw_plot(my_plot)
beep(6)


# Previsões ---------------------------------------------------------------
# Comparação entre previsões:

# Qual seria a quantidade média esperada de violações de trânsito para um país
# cujo corpo diplomático seja composto por 23 membros, considerando o período
# anterior à vigência da lei e cujo índice de corrupção seja igual 0.5?

# Modelo Poisson:
predict(object = modelo_poisson, #linha 144 deste script
        newdata = data.frame(staff = 23,
                             post = "no",
                             corruption = 0.5),type = "response")
# Interpretação:

# Modelo Binomial Negativo:
predict(object = modelo_bneg,
        newdata = data.frame(staff = 23,
                             post = "no",
                             corruption = 0.5),type = "response")
# Interpretação:



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


# Adicionando os fitted values dos modelos estimados até o momento, para fins de 
# comparação:
corruption %>%
  mutate(fitted_poisson = modelo_poisson$fitted.values,
         fitted_bneg = modelo_bneg$fitted.values) %>% 
  dplyr::select(country, code, violations, fitted_poisson, 
                fitted_bneg) %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 21)


# Fitted values dos modelos POISSON e BINOMIAL NEGATIVO, considerando,
# para fins didáticos, apenas a variável preditora 'staff':
corruption %>%
  ggplot() +
  geom_point(aes(x = staff, y = violations), alpha = 0.5, size = 2) +
  geom_smooth(aes(x = staff, y = modelo_poisson$fitted.values,
                  color = "POISSON"), se = F, size = 1.5) +
  geom_smooth(aes(x = staff, y = modelo_bneg$fitted.values,
                  color = "BNEG"), se = F, size = 1.5) + 
  scale_color_manual("Estimação:",
                     values = c("orange", "#440154FF")) +
  labs(x = "Number of Diplomats (staff)",
       y = "Unpaid Parking Violations (violations)") +
  theme(panel.background = element_rect("white"),
        panel.grid = element_line("grey95"),
        panel.border = element_rect(NA),
        legend.position = "bottom")



# Estimação Próximas: Poisson e Bin Negativo ------------------------------

# Para fins didáticos, vamos gerar novo dataset 'corruption2', com quantidades
# de violações de trânsito iguais, no máximo, a 3. Este procedimento poderá,
# eventualmente, eliminar o fenômeno da superdispersão nos dados da variável
# dependente e, consequentemente, tornar as estimações dos modelos POISSON e
# BINOMIAL NEGATIVO praticamente iguais.

# Gerando novo dataset 'corruption2' com violations <= 3
corruption2 <- corruption[which(corruption$violations <= 3),1:6]

# Histograma da variável dependente 'violations' no dataset 'corruption2'
ggplotly(
  corruption2 %>%
    ggplot(aes(x = violations,
               fill = ..count..)) +
    geom_histogram(bins = 4,
                   color = "black") +
    scale_fill_gradient("Contagem",
                        low = "#440154FF", 
                        high = "#FDE725FF") +
    labs(x = "Quantidade de violações de trânsito",
         y = "Frequência") +
    theme_bw()
)


# Diagnostico: igualdade média e variância? ------------------------------------

# Diagnóstico preliminar para observação de eventual igualdade entre a média e
# a variância da variável dependente 'violations' no dataset 'corruption2'
corruption2 %>%
  summarise(Média = mean(violations),
            Variância = var(violations)) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F,
                font_size = 30)


# Estimar Modelo Poisson ----------------------------------------------------------

# Estimação do modelo_poisson2
modelo_poisson2 <- glm(formula = violations ~ staff + post + corruption,
                       data = corruption2,
                       family = "poisson")

# Parâmetros do modelo_poisson2
summary(modelo_poisson2)


# Teste Superdispersão ----------------------------------------------------
# Teste de superdispersão no dataset 'corruption2'
overdisp(x = corruption2,
         dependent.position = 3,
         predictor.position = 4:6)


# Estimar Modelo Binomial Negativa ----------------------------------------
# Estimação do modelo_bneg2
modelo_bneg2 <- glm.nb(formula = violations ~ staff + post + corruption,
                       data = corruption2)

# Parâmetros do modelo_bneg2
summary(modelo_bneg2)


# Significancia Estatistica Modelo ----------------------------------------
# Significância estatística do parâmetro de forma da distribuição
# binomial negativa para o modelo_bneg2
modelo_bneg2$theta / modelo_bneg2$SE.theta # menor que 1.96 (= melhor é poisson)



# Comparação Parâmetros ---------------------------------------------------
# Comparando os parâmetros e os valores de LL de modelo_poisson2 e modelo_bneg2
export_summs(modelo_poisson2, modelo_bneg2, scale = F, digits = 4,
             model.names = c("POISSON2","BNEG2"))

data.frame(LL_Poisson2 = round(logLik(modelo_poisson2), 1),
           LL_Bneg2 = round(logLik(modelo_bneg2), 1)) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", position = "center", 
                full_width = F, 
                font_size = 30)


# Teste de Comparação entre modelos ---------------------------------------
# Likelihoo-ratio test para a comparação entre modelo_poisson2 e modelo_bneg2
lrtest(modelo_poisson2, modelo_bneg2)

# Interpretação: não existe diferença significativa entre os modelos.



# Resumo
# - Podemos testar se o teste poisson é ideal, através do teste de superdispersão
#   overdisp (teste de cameron). Se der significativo, significa que tem 
#   superdispersão, logo o ideal seria binomial negativa;
# - Podemos testar se o teste binomial negativo é idela, através do teste de 
#   significancia do parametro de distribuição theta. Se resultado for <1.96, 
#   significa que o teste poisson é ideal, se for >1.96 o teste binomial é ideal



