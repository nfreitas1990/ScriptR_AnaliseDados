
# Natália Freitas
# Modelos Lineares


# Contextualização --------------------------------------------------------

# Modelos de regressão para dados de contagem:
#   - Poisson
#   - Binomial Negativo (Poisson Gamma)
#   - Zero Inflated Poisson (ZIP)
#   - Zero Inflated Negative Binomial (ZINB)

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



# Atenção! ! ! !  ---------------------------------------------------------
# Quando temos muitos mais não eventos do que eventos, certamente teremos que 
# usar modelos de inflação de zeros. Só que teremos que testar se o excesso de
# zero é significativo e se há superdispersão para saber se usaremos o ZIP ou
# o ZINB


# Poisson -----------------------------------------------------------------

# Distribuição Poisson:
#> A distribuição poisson é uma variável aleatória definida por apenas um 
#> parâmetro (λ), equivalente à média, chamada de lambda. A distribuição poisson
#> tem uma característica interessante, seu desvio padrão é igual à média.
#> Portanto, se a média aumenta, o desvio acompanha esse aumento e a distribuição
#> passa a ter um maior espalhamento. Portanto, temos resíduos heterocedásticos

# Podemos utilizar a distribuição de possion para substituir a distribuição
# normal para representar melhor as var. de contagens, principalmente quando a 
# distribuição dos resíduos não é normal. Embora a relação entre as variáveis
# não seja linear, ela é linearizávl. Pois em geral, tem formato exponencial
# Argumento family = "poisson" 

# A distribuição de poisson se diferencia da distribuição binomial negativa, 
# ambas apresentam assimetria, mas a calda da distribuição de poisson é curta e
# da distribuição binomial negativa é longa.


# Indicado:
# Variável de Contagem;
# Contagem é limitada ao zero (positivos)
# Valores discretos (Inteiros) 
# Quando temos muito zeros  

# Relação entre variável resposta e preditora:
# Não é linear. Tem formato exponencial. Por isso, usamos 
# a função de ligação log 

# Resíduos:
# Não tem distribuição normal
# São heterocedásticos

# Função de ligação: log

# Característica: em geral, na distribuição de poisson a maioria dos valores
# ocorrem em baixa frequencia e poucas observações ocorrem em alta frequencia.
# Os valores médios das contagens e a variância das contagens são iguais. Neste
# caso, não precisamos de dois parâmetros para descrever essa distribuição
# Pois, se a média = variância, só precisamos de um parâmetro, que foi chamado
# de lambda (Λ).

# Lambda: A medida que aumenta o valor de lambda, a distribuição vai ficando 
#         mais simetrica ao ponto que amostras muito grandes a distribuição
#         se assemelha a uma normal. Com lambda > 30 temos praticamente a 
#         distribuição normal. Com contagens baixas (coisas com pouca quantidade)
#         a curva (e.g.lambda =0.8) é bastante assimetrica. no entanto, se 
#         contamos coisas muito abundantes a curva se torna simétrica se 
#         assemelha a normal.


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


# Distribuição Poisson ----------------------------------------------------
pois <- rpois(1000, lambda = 2)
hist(pois, breaks = 10)


# Conceitual --------------------------------------------------------------
# Função da Distribuição de Probabilidade ---------------------------------

# Mostra que conforme aumenta o valor de lambda a distribuição de poisson
# começa a se aproximar de uma distribuição normal

# Estabelecendo uma função da distribuição Poisson com lambda = 1
poisson_lambda1 <- function(m){
  lambda <- 1
  (exp(-lambda) * lambda ^ m) / factorial(m)
}

# Estabelecendo uma função da distribuição Poisson com lambda = 4
poisson_lambda4 <- function(m){
  lambda <- 4
  (exp(-lambda) * lambda ^ m) / factorial(m)
}

# Estabelecendo uma função da distribuição Poisson com lambda = 10
poisson_lambda10 <- function(m){
  lambda <- 10
  (exp(-lambda) * lambda ^ m) / factorial(m)
}

# Plotagem das funções estabelecidas anteriormente
data.frame(m = 0:20) %>%
  ggplot(aes(x = m)) +
  stat_function(fun = poisson_lambda1, size = 1.5,
                aes(color = "01")) +
  stat_function(fun = poisson_lambda4, size = 1.5,
                aes(color = "04")) +
  stat_function(fun = poisson_lambda10, size = 1.5,
                aes(color = "10")) +
  scale_color_viridis_d("Valores de" ~ lambda ~ "") +
  labs(y = "Probabilidades", x = "m") +
  theme_bw()



# Dados -------------------------------------------------------------------
#Fisman, R.; Miguel, E. Corruption, Norms, and Legal Enforcement:
#Evidence from Diplomatic Parking Tickets.
#Journal of Political Economy, v. 15, n. 6, p. 1020-1048, 2007.
#https://www.journals.uchicago.edu/doi/abs/10.1086/527495

load(file = "raw_datas_exemplos/corruption.RData")
corruption

# Visualizando a base de dados
corruption %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = F,
                font_size = 21)

# Visualização das observações e das  especificações 
# referentes às variáveis da base de dados
glimpse(corruption) 



# Exploratório ------------------------------------------------------------
summary(corruption)

corruption[corruption$violations == 167,]
corruption[corruption$country == "Brazil",]

# Tabela de frequências da variável dependente
# função freq ( ) para gerar tabelas de frequência 
questionr::freq(corruption$violations) %>%
  kable()%>%
  kable_styling(bootstrap_options = "striped",
                full_width = F,
                font_size = 25)

# Histograma da variável dependente
ggplotly(
  corruption %>%
    ggplot(aes(x = violations,
               fill = ..count..)) +
    geom_histogram(bins = round(2 * nrow(corruption) ^ (1 / 3)),
                   color = "black") +
    scale_fill_gradient("Contagem",
                        low = "#440154FF", 
                        high = "#FDE725FF") +
    labs(x = "Quantidade de violações de trânsito",
         y = "Frequência") +
    theme_bw()
)


# Igualdade entre Média e Variancia ---------------------------------------
# Na distribuição de poisson, média e variância são iguais para a variável Y.
# Aqui conseguimos de forma preliminar ver se são iguais. Existe teste para 
# significancia disso, mas assim dá para ver de forma preliminar. 
# Se for muito diferente (como neste caso) o ideal seria uma binomial negativa.

# Diagnóstico preliminar para observação de eventual igualdade entre a média e
corruption %>%
  summarise(Média = mean(violations),
            Variância = var(violations)) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F,
                font_size = 30)


# Superdispersão ----------------------------------------------------------
# Quando a variância é bem maior do que a média. Neste caso, a binomial negativa
# é preferível em detrimento de poisson




# Comportamento das variáveis 'corruption' e 'violations' antes e depois do 
# início da vigência da lei
corruption %>%
  mutate(lnviolations = log(violations),
         lnviolations = ifelse(lnviolations == -Inf,
                               yes = 0, 
                               no = lnviolations)) %>%
  ggplot(aes(x = corruption, y = lnviolations)) +
  geom_point(color = "black") +
  geom_smooth(aes(color = "Fitted Values"),
              method = "lm",
              formula = y ~ splines::bs(x),
              se = FALSE, size = 2) +
  geom_text_repel(aes(label = code), # pacote ggrepel
                  size = 2,
                  color = "black",
                  max.overlaps = 100) +
  labs(y = "Violações de Trânsito em NY (logs)",
       x = "Índice de Corrupção dos Países") +
  scale_color_manual("Label:",
                     values = "gold") +
  facet_wrap(~post) +
  theme_bw()

corruption %>%
  mutate(lnviolations = log(violations),
         lnviolations = ifelse(lnviolations == -Inf,
                               yes = 0, 
                               no = lnviolations))



# Estimar Modelo ----------------------------------------------------------
modelo_poisson <- glm(formula = violations ~ staff + post + corruption,
                      data = corruption,
                      family = "poisson")


# Output ------------------------------------------------------------------
# Parâmetros do modelo_poisson
summary(modelo_poisson)


# # Interpretação do Output  ------------------------------------------------

# Call:
#   glm(formula = violations ~ staff + post + corruption, family = "poisson", 
#       data = corruption)
# 
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -9.1425  -2.8326  -0.6008  -0.3940  24.6141  
# 
# Coefficients:
#                Estimate Std. Error z value Pr(>|z|)    
# (Intercept)    2.212739   0.031107   71.13   <2e-16 ***
#   staff        0.021870   0.001228   17.81   <2e-16 ***
#   postyes     -4.296762   0.197446  -21.76   <2e-16 ***
#   corruption   0.341765   0.027495   12.43   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for poisson family taken to be 1)
# 
# Null deviance: 6397.7  on 297  degrees of freedom
# Residual deviance: 3644.0  on 294  degrees of freedom
# AIC: 4151.6
# 
# Number of Fisher Scoring iterations: 7

# Call: esta é a chamada da função glm(), que mostra os argumentos que foram 
# usados para ajustar o modelo.

# Deviance Residuals: estas são as deviances residuais do modelo, que são 
# usadas para avaliar o ajuste do modelo aos dados. Os valores negativos 
# indicam que o modelo superestima as observações, enquanto os valores positivos
# indicam que o modelo subestima as observações. Valores altos de deviance 
# residual podem indicar problemas com o ajuste do modelo.

# Coefficients: estes são os coeficientes estimados para cada variável 
# preditora no modelo, juntamente com seus erros-padrão, estatísticas z e 
# p-valores. Os coeficientes indicam a direção e a magnitude do efeito que cada 
# variável tem na variável resposta. Os p-valores indicam se cada variável é 
# estatisticamente significativa para prever a variável resposta.

# Null deviance: esta é a deviance nula do modelo, que é a deviance do modelo
# que contém apenas o intercepto. Ela pode ser usada como uma referência para 
# avaliar o ajuste do modelo final.

# Residual deviance: esta é a deviance residual do modelo, que é a deviance do 
# modelo ajustado menos a deviance nula. Ela indica o quanto de deviance o 
# modelo final ainda tem, depois de explicar o máximo possível de deviance com 
# as variáveis preditoras incluídas no modelo.

# AIC: este é o critério de informação Akaike do modelo, que é uma medida 
# de ajuste que leva em conta tanto o ajuste do modelo aos dados quanto a 
# complexidade do modelo. Quanto menor o AIC, melhor é o ajuste do modelo.

# Number of Fisher Scoring iterations: este é o número de iterações que foram 
# necessárias para ajustar o modelo usando o algoritmo de maximização da 
# verossimilhança.


# Log-Likelihood ----------------------------------------------------------
# Extração do valor de Log-Likelihood (LL)
# A intenção é sempre a Maximização do LogLike
# É o valor que maximiza os acertos do modelo
logLik(modelo_poisson)


# Visualização Output -----------------------------------------------------
# Outra forma de visualização dos parâmetros - função summ do pacote jtools
summ(modelo_poisson, digits = 4, confint = T, ci.width = 0.95)
export_summs(modelo_poisson, scale = F, digits = 4)


# Extrair equação ---------------------------------------------------------
equatiomatic::extract_eq(modelo_poisson, use_coefs = T, coef_digits = 4) |> 
  kable() |> 
  kable_styling(font_size = 12)


# Qualidade Modelo: LogLik entre Modelo -----------------------------------
# Likelihood ratio test: para comparação de LL's entre modelos
# Neste caso, comparação com o modelo nulo (somente intercepto)
lmtest::lrtest(modelo_poisson) 

# Esse teste qui-quadrado é semelhante ao teste F dos modelos anteriores
# indica o ajuste geral do modelo. Quando >0.05 significa que o Model1 (
# modelo completo) é significativamente melhor que o Model 2 (modelo nulo)

# Likelihood ratio test
# 
# Model 1: violations ~ staff + post + corruption
# Model 2: violations ~ 1
#    Df     LogLik    Df    Chisq   Pr(>Chisq)    
# 1   4     -2071.8                         
# 2   1     -3448.6   -3   2753.7  < 2.2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


# Model 1: este é o modelo completo, que inclui todas as variáveis preditoras.
# Model 2: este é o modelo nulo, que inclui apenas o intercepto.
# Df: este é o número de graus de liberdade do modelo
# LogLik: este é o logaritmo da verossimilhança do modelo, que é uma medida de
#         ajuste que leva em conta a complexidade do modelo. Quanto maior o 
#         logaritmo da verossimilhança, melhor é o ajuste do modelo.
# Df: esta é a diferença no número de graus de liberdade entre os dois modelos.
# Chisq: este é o valor do teste de razão de verossimilhança, que é usado para
#        testar a hipótese nula de que o modelo nulo é tão bom quanto o modelo 
#        completo. Quanto maior o valor do teste, mais evidência há contra 
#        a hipótese nula.
# Pr(>Chisq): este é o valor p do teste de razão de verossimilhança, que indica 
#             a probabilidade de obter um valor de teste tão extremo ou mais 
#             extremo do que o observado, sob a hipótese nula de que o modelo 
#             nulo é tão bom quanto o modelo completo. Valores de p menores 
#             que 0,05 (ou seja, < 0,05) são geralmente considerados 
#             estatisticamente significativos, o que significa que há 
#             evidências suficientes para rejeitar a hipótese nula e 
#             concluir que o modelo completo é significativamente melhor que 
#             o modelo nulo.




# Comparação entre modelos ------------------------------------------------
# Duas opções:
# Opção 1: lmtest::lrtest()   - avalia LogLikelihood
# Opção 2: anova ( )          - avalia Deviance

# Modelo Nulo:
modelo_poisson_nulo <- glm(formula = violations ~ 1,
                           data = corruption,
                           family = "poisson")
# Modelo 1:
modelo_poisson <- glm(formula = violations ~ staff + post + corruption,
                      data = corruption,
                      family = "poisson")
# Modelo 2:
modelo_poisson2 <- glm(formula = violations ~ staff,
                       data = corruption,
                       family = "poisson")


# lrtest( ) ---------------------------------------------------------------
# OPÇÃO 1: lmtest::lrtest()

lmtest::lrtest(modelo_poisson, modelo_poisson_nulo) 
# Model 1: violations ~ staff + post + corruption
# Model 2: violations ~ 1
#    #Df  LogLik Df  Chisq Pr(>Chisq)    
# 1   4 -2071.8                         
# 2   1 -3448.6 -3 2753.7  < 2.2e-16 ***


lmtest::lrtest(modelo_poisson, modelo_poisson_nulo, modelo_poisson2) 
# Model 1: violations ~ staff + post + corruption
# Model 2: violations ~ 1
# Model 3: violations ~ staff
#    #Df  LogLik Df   Chisq Pr(>Chisq)    
# 1   4 -2071.8                          
# 2   1 -3448.6 -3 2753.71  < 2.2e-16 ***
# 3   2 -3366.3  1  164.76  < 2.2e-16 ***

# Chisq: este é o valor do teste de razão de verossimilhança, que é usado
# para testar a hipótese nula de que o modelo nulo é tão bom quanto o modelo
# completo (para Model 1 vs Model 2) ou que o modelo com menos preditores é
# tão bom quanto o modelo com mais preditores (para Model 1 vs Model 3).

# Pr(>Chisq): concluir que o modelo completo (ou o modelo com mais preditores) 
# é significativamente melhor que o modelo nulo (ou o modelo com menos 
# preditores). No exemplo apresentado, tanto o modelo completo (Model 1) 
# quanto o modelo com staff como preditor (Model 3) são significativamente 
# melhores do que o modelo nulo (Model 2), com valores de p < 0,05


# anova ( ) ---------------------------------------------------------------
# OPÇÃO 2: anova ( ) 
anova(modelo_poisson, modelo_poisson_nulo, test= "Chi")




# Todas as variáveis preditoras se mostraram estatisticamente diferentes de zero,
# considerando-se um nível de significância de 5%, ceteris paribus. Porém, já se
# pode afirmar que a estimação Poisson é a mais adequada?



# Pressuposto -------------------------------------------------------------
# Para avaliar se a distribuição de poisson é a mais adequada precisamos
# olhar se ocorre superdispersão. A superdispersão avalia se existe uma calda
# muito longa para a distribuição, o que fará com que a binomial negativa
# tenha que ser utilizada

# Teste Superdispersão ----------------------------------------------------
# TESTE DE SUPERDISPERSÃO DE CAMERON E TRIVEDI (1990) 
# CAMERON, A. C.; TRIVEDI, P. K. Regression-based tests for overdispersion in
# the Poisson model. Journal of Econometrics, v. 46, n. 3, p. 347-364, 1990.

#  1º Passo: estimar um modelo Poisson;
#  2º Passo: criar uma nova variável (Y*) utilizando os fitted values do modelo
#            Poisson estimado anteriormente;
#  3º Passo: estimar um modelo auxiliar OLS, com a variável Y* como variável
#            dependente, os fitted values do modelo Poisson como única variável
#            preditora e sem o intercepto;
#  4º Passo: Observar a significância do parâmetro beta.


# Passo 1: Estimar modelo
modelo_poisson <- glm(formula = violations ~ staff + post + corruption,
                      data = corruption,
                      family = "poisson")

# Passo 2: Fitted values
# Adicionando os fitted values do modelo Poisson (lambda_poisson) à base 
# de dados:
corruption$lambda_poisson <- modelo_poisson$fitted.values

# Criando a nova variável Y*:
attach(corruption)
corruption$ystar <- (((violations - lambda_poisson) ^ 2)
                     - violations) / lambda_poisson
detach(corruption)

# Estimando o modelo auxiliar OLS, sem o intercepto e usando o lambda como
# variável:
modelo_auxiliar <- lm(formula = ystar ~ 0 + lambda_poisson,
                      data = corruption)

# Observando os parâmetros do modelo_auxiliar
summary(modelo_auxiliar)

# Interpretação:
# p-value > 0.05 = existência de equidispersão nos dados
# p-value < 0.05 = existência de superdispersão nos dados. Logo,temos que usar
#                  o modelo binomial negativo porque existe calda longa demais


# Caso o p-value do parâmetro do lambda_poisson seja maior que 0.05,
# verifica-se a existência de equidispersão nos dados.
# Caso contrário, diagnostica-se a existência de superdispersão nos dados, fato
# que favorecerá a estimação de um modelo binomial negativo.



# Opção 2 para o teste de Superdispersão ---------------------------------
# Uma abordagem mais direta para a detecção da superdispersão pelo Teste de
# Cameron e Trivedi (1990) é por meio da utilização do algoritmo overdisp( )
# Neste caso, não precisamos seguir todos os passos do item anterior.
overdisp::overdisp(x = corruption,
         dependent.position = 3,    # posição da variável Y
         predictor.position = 4:6)  # posição das variáveis explicativas

# Interpretação:
# p < 0.05 existe superdiserpersão dos dados. devemos usar binomial negativa



# Predição ----------------------------------------------------------------
# Apenas para fins didáticos, caso considerássemos a estimação Poisson como a
# mais adequada, qual seria a quantidade média esperada de violações de trânsito
# para um país cujo corpo diplomático fosse composto por 23 membros, considerando
# o período anterior à vigência da lei e cujo índice de corrupção seja
# igual a 0.5?
predict(object = modelo_poisson, 
        newdata = data.frame(staff = 23,
                             post = "no",
                             corruption = 0.5),
        type = "response")

# outra opção
lambda <- 2.212+(0.0218*28)-(4.296*1)+(0.3417*1) #formula do modelo
exp(lambda)
# [1] 0.3224201


# Qual seria a quantidade média esperada de violações de trânsito para o mesmo
# país, porém agora considerando a vigência da lei?
predict(object = modelo_poisson, 
        newdata = data.frame(staff = 23, 
                             post = "yes", 
                             corruption = 0.5), 
        type = "response")

# Esse valor do predict já é o valor do lambda.


# Mas, neste caso, como a distribuição deu superdispersão. Faríamos um Modelo
# binomial negativo (conhecido também como poisson-gamma). 
# Ir para o outro código 2023_RegressaoGLM_BinomialNegativa

lambda <- 2.212+(0.0218*28)-(4.296*0)+(0.3417*1)
modelo <- (exp(-lambda) * lambda ^ m) / factorial(m)

