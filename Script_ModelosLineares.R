# Natália Freitas
# Modelos Lineares



# Modelos Lineares --------------------------------------------------------
# Os modelos supervisionados: estima modelos que embora sejam simplificações 
# da realidade, apresentam a melhor aderencia possível entre os valores reais
# e previstos.
# Existem dois problemas em modelagem supervisionada:
# 1. Regressão: quando Y é um variável contínua
# 2. Classificação: qunado Y é uma variável categórica

# Outcome -----------------------------------------------------------------
# A primeira coisa que devemos fazer para selecionar corretamente os modelos
# é olhar para a variavel resposta. O tipo da variável respostas nos dará um
# indício do tipo de distribuição que devemos escolher.



#     #     #     #       #      #      #     #       #       #     #       #   

# Tabela Modelos ---------------------------------------------------------------
#                                                                               
# MODELO REGRESSAO      VARIAVEL DEPENDENTE               DISTRIBUIÇÃO    FUNÇÃO LIGAÇÃO
# Linear                 Quantitativa                      Normal            Yhat             
# Com transf.BoxCox      Quantitativa              Normal após transf.    ((Y^lambda) - 1)/lambda   
# Logística Binaria      Qualitativa (2cat.)             Bernoulli         ln(p/1-p)          
# Logistica Multinomial  Qualitativa M(M>2) Categorias     Binomial         ln(pm/1-pm)       
# Poisson                Quantitativa com valores          Poisson          ln(lambda poisson)
#                        inteiros e ñ negativo                                                
# Binomial Negativa       Quantitativa com valores          Poisson-gamma    ln(lambda bneg)  
#                         inteiros e ñ negativos                                              


# 1. Regressão Linear -----------------------------------------------------
#    Olhar o script complementar de regressão linear simples e múltipla
#    2023_RegressaoLinear


# 2. Regressão com Transformação de BoxCox --------------------------------
#    Olhar o script complementar de regressão linear simples e múltipla
#    2023_RegressaoLinear


# 3. Modelos Lineares Generalizados (GLM) -------------------------------------

#     - Regressão Logistica Binária  --------------------------------------------
#       Y CATEGORICO COM 2 CATEGORIAS
#      Olhar o script complementar de regressão Logistica Binária
#      2023_RegressaoLogistica
#      > Utilizado quando temos Y Qual

#     - Regressão Logistica Multinomial  --------------------------------------------
#       Y CATEGÓRICO COM MAIS DE 2 CATEGORIAS
#      Olhar o script complementar de regressão Logistica Multinomial
#      2023_RegressaoLogisticaMultinomial

#     - Regressão GLM: Poisson  --------------------------------------------
#       CONTAGEM
#      Olhar o script complementar
#      2023_RegressaoGLM_Poisson

#     - Regressão GLM: Binomial Negativo  --------------------------------------------
#       CONTAGEM
#      Olhar o script complementar
#      2023_RegressaoGLM_Poisson











# x -----------------------------------------------------------------------
# Modelos Lineares Generalizados (GLMs) -----------------------------------


# 1. Contextualização --------------------------------------------------------
#> Os modelos lineares generalizados (GLMs) são uma ampliação do modelos 
#> lineares (lm; regressão simples e multipla). Os GLMs são usados quando 
#> os resíduos (erros) apresentam distribuição diferente da normal (gaussiana).

#> A natureza da variável resposta é uma boa indicação do tipo de distribuição
#> dos resíduos que iremos encontrar no modelo. Por exemplo, dados de contagem
#> possuem valores positivos e inteiros, em geral,  possuem uma distribuição de
#> erro assimétrica, com muitos valores baixos e poucos valores altos e uma 
#> variânica que aumenta com a média dos valores preditos, violando duas 
#> premissas dos modelos lineares gaussianos.  
#>    - Contagem simples
#>    - Contagem exepressa em proporção
#>    - Número de sucesso e tentativa
#>    - variáveis binárias
#>    - tempo para o evento ocorrer


# 2. Componentes --------------------------------------------------------------
#> Modelos generalizados são separados em dois componentes: a relação 
#> determinística entre as variáveis (resposta e preditora) e o componente 
#> aleatório dos resíduos (distribuição dos erros).
#> Em um modelo linear gaussiano, a relação entre as variáveis é constante, o q
#> define a relação funcional de uma reta. Quando temos uma contagem, essa 
#> relação pode ter uma estrutura funcional de uma exponencial. Para esses 
#> casos, os modelos usam uma função de ligação log para linearizar a relação
#> entre as variáve. Portanto, a estrutura dos GLMs é determinada por um 
#> preditor linear associada a uma função de ligação.

#> O componente aleatório dos resíduos (erros), no caso de uma vriável de 
#> contagem, segue a distribuição de poisson. Outros tipos de variáveis, 
#> seguirão outras distribuições.

#> NATUREZA VARIÁVEL     ESTRUTURA RESIDUOS   FUNÇÃO LIGAÇÃO  
#> Var. contínua              normal           identidade (=s/ transformação)
#> Var. contagem              poisson          log
#> Var. proporção             bionomial        logit



# 3. Dispersão e Acúmulo de Zeros --------------------------------------------
#> Os modelo GLM poisson e binomial apresentam a variância acoplada à média dos
#> valores,diferentemente dos modelos com distribuição normal onde a média e
#> a variância são independentes. Caso haja uma variação maior ou menor nos 
#> dados do que o previsto por essas distribuições, o modelo não consegue
#> dar conta. Essa sobre-dispersão ou sub-dispersão dos dados indica que
#> temos mais ou menos variação do que é predito pelos modelos.
     
#> A solução mais simples para lidar com a dispersão são os modelo quasipoisson
#> e quasibinomial, que estimam um parâmetro a mais, relacionando a média à 
#> variância, o parâmetro de dispersão. Entretanto, os modelos quasi dão conta 
#> apenas de dispersões moderadas e não indicam qual a fonte dela. Há algumas 
#> alternativas ao modelo quasi para a dispersão dos dados, alguns deles estão 
#> listados abaixo:

#> modelo binomial negativo
#> modelo de mistura, considerando dois processos distintos
#> modelos mistos, considerando a ausência de independência das observações
#> modelos com acúmulos de zeros (Zero Inflated Models) 


# 4. Maxima Verossimilhança --------------------------------------------------
#> O método classico para estimar os parâmetros de uma regressão consiste no
#> método de mínimos quadrados, onde a reta passa no ponto que minimiza
#> a soma dos desvios quadráticos. A verossimilhança é o valor proporcional a 
#> probabilidade de um modelo qualquer (uma das inclinações de retas simulados) 
#> gerar os nossos dados. Então podemos maximizar a verossimilhanca, busca o valor 
#> de inclinação (estimativa do modelo) que maximissa a verossimilhança. 
#> É usada tanto para estimar os parâmetros dos modelos, quanto para dar uma
#> ideia do ajuste do modelo.
#> Os valores de verossimilhanças são tão pequenos que costumam ser 
#> apresentados em log.


# 5. Deviance ----------------------------------------------------------------
#> São valores que mostram o ajuste do modelo. Está relacionado com a distancia
#> entre o nosso modelo e o modelo saturado (=modelo que explica completamente
#> os nossos dados; modelos mais complexo que o número de parâmetros é igual ao
#> número de observações)


#> 6. 
#> Family	         Link Function
# binomial	      (link = “logit”)
# gaussian	      (link = “identity”)
# Gamma	          (link = “inverse”)
# poisson	        (link = “log”)
# quasi	          (link = “identity”, variance = “constant”)
# quasibinomial	  (link = “logit”)
# quasipoisson	  (link = “log”)

# FAMÍLIAS --------------------------------------------------------------

# xxxxx -------------------------------------------------------------------
# Família: Poisson -----------------------------------------------------------------
# family = "poisson" 

#> A distribuição poisson é uma variável aleatória definida por apenas um 
#> parâmetro (λ), equivalente à média, chamada de lambda. A distribuição poisson
#> tem uma característica interessante, seu desvio padrão é igual à média.
#> Portanto, se a média aumenta, o desvio acompanha esse aumento e a distribuição
#> passa a ter um maior espalhamento.

# Podemos utilizar a distribuição de possion para substituir a distribuição
# normal para representar melhor as var. de contagens, principalmente quando a 
# distribuição dos resíduos não é normal. Embora a relação entre as variáveis
# não seja linear, ela é linearizávl. Pois em geral, tem formato exponencial

# Indicado:
# Variável de Contagem;
# Contagem é limitada ao zero (positivos)
# Valores discretos (Inteiros) 
# Quando temos muito zeros  

# Relação entre variável resposta e preditora:
# Não é linear. Tem formato exponencial. por isso, usamos a função de ligação log 

# Resíduos:
# Não tem distribuição normal

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

# Dados -----------------------------------------------------------------
dados <- read.table("raw_datas_exemplos/especies.txt",header = T)
head(dados)
str(dados)

# Explorar ----------------------------------------------------------------
hist(dados$Species)
hist(dados$Biomass)


# Analise glm -----------------------------------------------------------------
# Se estamos usando o GLM, então, usaremos a Maxima Verossimilhança para 
# calcular as estimativas
# Se o p-valor não aparecer, acrescentar o argumento test=“Chisq”
mod1 <- glm(Species ~ Biomass + pH, data= dados,family = poisson(link = "log"))
summary(mod1)


# Interpretação:
# TODOS OS VALORES DO RESULTADO ESTAO EM ESCALA EXPONENCIAL
# Então temos que tirar o exponencial para ver o efeito das var. preditoras
# na var. resposta

# Estimate: exp(-0.127)= 0.8780954 (efeito da Biomass na Species)
#-
# Capa p-valor, se o p-valos<0.05 então, a variável tem efeito na var. resposta
#-
# Residual deviance: Se o residual deviance é maior do que os graus de liberdade,
# então existe uma superdispersão. Isso significa que as estimativas estão corretas
# mas que os erros padrões (desvios padrões) estão errados e não são explicadas pelo modelo.
#-
# Null Deviance: mostra o quão bem a variável resposta é predita por um modelo 
# que inclui apenas o intercepto. A inclusão das variáveis reduz o desvio de 452
# para 99. Maior diferença de valores significa um ajuste ruim.


# Tentar outro modelo, pois parece com base na interpretação que esse modelo
# não está legal. Pois o Residual deviance está maior do que os graus de 
# liberdade e o Null deviance e o Residual Deviance estão muito diferentes

mod2 <- glm(Species ~ Biomass + pH, data= dados, family = quasipoisson(link = "log"))
summary(mod2)


# Comparação de Modelos ---------------------------------------------------
mod0 <- glm(Species ~ 1, data= dados, family = poisson(link = "log"))
mod1 <- glm(Species ~ Biomass + pH, data= dados, family = poisson(link = "log"))
mod2 <- glm(Species ~ Biomass + pH, data= dados, family = quasipoisson(link = "log"))



# Comparação usando ANOVA 
# Se os modelos são diferentes (p<0.05), escolher o modelo mais complexo
# Se os modelos não são diferentes (p>0.05), manter o modelo mais simples
anova(mod0,mod1, test= "Chi") # p <0.05 - sigo mod1
anova(mod1,mod2, test= "Chi") # p >0.05 - sigo mod1

# Null deviance: mede a variabiliade total na variável resposta
# Residual deviance: mede a variabiliade na variável resposta que permanece
# inexplicada pelo modelo



# Calcular o equivalente ao R²
# Extrair os desvios
dev.null <- mod1$null.deviance
dev.resid <- mod1$deviance
# Calcular o desvio explicado pelo modelo
dev.explained <- (dev.null - dev.resid)/dev.null
# Arredondar para 3 casas decimais
dev.explained <- round(dev.explained,3)
dev.explained


# Comparação modelos usando "arm"
library(arm)

# Extrair o coeficiente dos modelos
coef0 = coef(mod0)
coef1 = coef(mod1)
coef2 = coef(mod2)

# Extrair erro padrão
se.coef0 = se.coef(mod0)
se.coef1 = se.coef(mod1)
se.coef2 = se.coef(mod2)

# Colocando valores no mesmo dataframe
modelos <- cbind(coef0,coef1,coef2,se.coef0,se.coef1,se.coef2, exponent = exp(coef0))
modelos

# Interpretação:
# coef0      coef1      coef2   se.coef0   se.coef1   se.coef2 exponent
# (Intercept) 2.968133  3.8489420  3.8489420 0.02389738 0.05280817 0.05555538 19.45556
# Biomass     2.968133 -0.1275586 -0.1275586 0.02389738 0.01014076 0.01066831 19.45556
# pHlow       2.968133 -1.1363940 -1.1363940 0.02389738 0.06720061 0.07069655 19.45556
# pHmid       2.968133 -0.4451592 -0.4451592 0.02389738 0.05486022 0.05771419 19.45556
#

# Coef2: 
exp(-0.12755) # = 0.88. Significa que há uma redução (pq -0.12 é negativo) de 
# 0.88 unidades da var. Species.
exp(-1.1363940) # = 0.32. Significa que se mudamos do phhigh (categoria referencia)
# para o phlow, teremos uma redução de 32% na unidade de var. resposta. 


# Comparando o modelo AIC
# Selecionamos o modelo com menor AIC. Pois esse é o modelo com melhor desempenho
AIC(mod1, mod2)



# Checar Pressuposto ------------------------------------------------------
library(DHARMa)

# Simulação dos resíduos
simResids <- simulateResiduals(mod1)
# Gerar gráficos para comparar modelo residual com esperado
plot(simResids)

# Distribuição dos resíduos
# Homogeneidade das variâncias

# Neste caso, encontrou um problema de homogeneidade das variâncias
# Os dados em ecologia geralmente são overdispersed(superdispersado), onde
# a variância é maior do que a média. Isso viola o pressuposto do Poisson,
# e significa que qualquer estatistica calculada pode ser irreal.Podemos
# olhar a super dspersão pelo residual deviance.
# Esperamos que residual deviance seja aproximadamente igual aos graus de liberdade residuais. Se o deviance for muito maior que os graus de liberdade, isso indica superdispersão.

summary(mod1)

# Superdispersão ----------------------------------------------------------
library(AER)
dispersiontest(mod1)
# se p>0.05: o modelo não é significativamente superdisperso. Então o modelo
# de poisson parece ser válido

# Se o modelo está super disperso:
# - ajustamos um quasi-Poisson GLM
# - ajustamos um negative binomial GLM


# xxxxxx ------------------------------------------------------------------
# Quasi-Poisson GLMs ------------------------------------------------------
mod0 <- glm(Species ~ 1, data= dados, family = quasipoisson)
mod1 <- glm(Species ~ Biomass + pH, data= dados, family = quasipoisson)
summary(mod1)

# Comparação de modelos ---------------------------------------------------
anova(mod0,mod1,test="F")


# xxxxxx ----------------------------------------------------------------
# Negative Binomial GLMs --------------------------------------------------
library(MASS)
mod1 <- glm.nb(Species ~ Biomass + pH, data= dados)
summary(mod1)

mod2 <- glm.nb(Species ~ Biomass , data= dados)
summary(mod2)


# Comparação --------------------------------------------------------------
anova(mod1,mod2)

#Superdisposição
simResids3 <- simulateResiduals(mod1)
plot(simResids3)



glm <- glm(Species ~ Biomass + pH, data= dados, family = gaussian)
summary(glm)
plot(lm)  


# xxxxxxxxxxxxxxxxxxxxx ---------------------------------------------------
# Gráficos ----------------------------------------------------------------
# Gráfico de dispersão condicionado
library(lattice)
xyplot(Species ~ Biomass | pH, data= dados, type = c("p", "r"))

# Gráfico Diagnóstico
glm2 <- glm(Species ~ Biomass + pH, data= dados, family = poisson)
par(mfrow = c (2,2))
plot(glm2)
# Interpretação:
# 1 -  resíduos vs valores preditos: homogeneidade dos resíduos. espero não
#      observar padrões nos resíduos
# 2 - Q-Q plot:  verificar se os resíduos tem distribuição normal
# 4 - Resíduo vs Leverage: olhar os pontos influentes na análise pelo método de
#     cook distance.

# Funções úteis -----------------------------------------------------------
# glm () -----------------------------------------------------------------
glm2 <- glm(Species ~ Biomass + pH, data= dados, family = poisson)
summary(glm2)
plot(glm2)


# summary ( ) -------------------------------------------------------------
# Resumo resultado
summary(resultado)

# tidy ( ) --------------------------------------------------------------
# Resumo resultado
library(broom)
tidy(resultado)


# coef ( ) ----------------------------------------------------------------
# Extrair coeficientes 
coef(resultado)


# confint ( ) -------------------------------------------------------------
# Extrair Intervalo de Confiança: confint()
confint(resultado, level = 0.95)


# predict ( ) -------------------------------------------------------------
# Fazer predições com base no modelo ajustado
predict(modelo, novos_dados)

# predict(): retorna a predição para os dados originais (sem especificar novos dados)
# predict(modelo, novos_dados predição para novo conjunto de dados


# update ( ) --------------------------------------------------------------
# atualizar o modelo com outras variáveis
mod1 <- glm(Species ~ Biomass, data= dados, family = poisson(link = "log"))
mod2 <- update(mod1, ~ . +pH)

mod3 <-  glm(Species ~ ., data = dados) # todos os parâmetros


# anova ( ) ---------------------------------------------------------------
# Calcula a deviance residual de cada modelo e aplica um teste qui-quadrado para
# ver se a diferença entre as deviances dos modelos foi significativo e se causou
# algum ganho ou perda no modelo


# Modelo Nulo
mod0 <- glm(Species ~ 1, data = dados)
mod1 <- glm (Species ~ Biomass, data= dados)
anova(mod0, mod1, test = "Chi")
# Do modelo sem nenhuma variável, ao acrescentar Biomass, reduzimos a deviance
# e essa redução foi significativa. Então selecionamos esse modelo mais complexo
# Acrescentar essa variável foi significativo para o modelo

mod2 <- glm(Species ~1, dadta= dados)
anova(mod1, mod2, test = "Chi")
# Ao acrescentar todos os parâmetros, temos uma diferença significaiva entre
# as deviances. Então seguimos com o mais complexo.

# Neste casao, começando do modelo mais complexo e retirando as variáveis podemos
# interpretar que a retirada da variável compromete o modelo, se o p for significativo
anova(mod2, mod1, test = "Chi")
