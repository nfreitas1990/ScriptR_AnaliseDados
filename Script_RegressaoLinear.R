

# Análise: REGRESSAO LINEAR SIMPLES 
# Atualizado: Fevereiro 2023
# Autor: NATALIA F SOUZA


# Pacotes -----------------------------------------------------------------
library(ggplot2)
library(tidyverse)
library(plotly)
library(knitr)
library(kableExtra)

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


# Contextualização --------------------------------------------------------

# Modelos Supervisionados: estima modelos que embora sejam simplificações 
# da realidade, apresentam a melhor aderencia possível entre os valores reais
# e previstos.

# Quando modelos tentamos minimizar a distancia entre os valores reais e os
# valores preditos pelo modelo.

# Existem dois problemas em modelagem supervisionada:
# 1. Regressão: Y é um variável contínua
# 2. Classificação: Y é uma variável categórica

 
# Formula -----------------------------------------------------------------
# y = Bo + B1X1 +B2X2 + E

# Bo:  Intercepto (coeficiente linear) - ponto em que a reta projetada toca 
#      o eixo y, quando x = 0
# B1: Inclinação (Coeficiente angular) - quanto se aumenta em Y ao alterar 1 
#     unidade de X. 

# Objetivo ----------------------------------------------------------------
# Avaliar a relacao de causa e efeito a respeito de duas variaveis 
# quantitativas. Essa relacao serah descrita em uma equacao matematica.
# Ao olhar essa equacao vamos interpreta-la no mundo real.


# Pressuposto -------------------------------------------------------------
# 1. Funcao que descreve a relacao entre as variáveis eh uma reta. 
#    Portanto, temos um modelo linear;

# 2. Normalidade dos residuos;

# 3. Homocedasticidade dos residuos - ao longo da variacao dos dados, 
#    os resíduos variam de forma homogenea ao longo da regressao 
#    (no comeco, meio e final). 

# Hipóteses ---------------------------------------------------------------
# Ho: B1 = 0 - Não há relação entre a variável resposta e a explicativa
# Ha: B1 =!0 - Há relação entre a variável resposta e a explicativa

# Outcome -----------------------------------------------------------------
# A primeira coisa que devemos fazer para selecionar corretamente os modelos
# é olhar para a variavel resposta


# Minimos Quadrados -------------------------------------------------------
#> A reta da regressão é criada de modo que a Soma dos quadrados seja 
#> minimizada. Retas que passam longe da nuvem de pontos dá uma soma
#> dos quadrados sempre maior. 
#> Esse método seleciona o intercepto e inclinação que minimizam o somatório
#> dos erros ao quadrado. Com a  restrição de que o somatório dos erros tem 
# que ser zero.

# Pressupoe: Soma dos erros = zero
#            Soma dos erros ao quadrado= é o mínimo


# R² ----------------------------------------------------------------------
# O R² representa o quanto da variação de Y é explicada pelo modelo. O R² é
# a correlação de Pearson ao quadrado. O r de Pearson foi elevado ao quadrado
# para que a escala não dê valores negativos. Pois, a correlação entre valores
# preditos e observado nunca pode ser negativo, necessariamente, valores
# maiores de Y tenderao a estar associados a valores maiores preditos pela reta
# Mas em essencia são a mesma coisa r de pearson e R2 sao a mesma coisa  

# O valor de R² indica o percentual de variância da variável Y que é devido ao
# comportamento de variação conjunta da(s) variável(is) explicativa(s) X.
# Varia de 0 a 1 e, quanto maior o coeficiente, maior o poder
# preditivo do modelo de regressão, ou seja, maior o poder de
# explicação do comportamento da variável dependente frente ao
# comportamento da(s) variável(is) explicativa(s).

# O R² consiste na formula: 
#             (somatório dos desvios (obs-esp) ao quadrado) / 
# (somatório dos desvios (obs-esp) ao quadrado) + (somatório dos erros ao quadrado)



# R² Ajustado -------------------------------------------------------------
# Quando houver o intuito de se compararem os resultados das
# estimações de dois modelos com quantidades distintas de parâmetros
# e/ou obtidos a partir de amostras com tamanhos diferentes, faz-se
# necessário o uso do R² ajustado.

# O R² ajustado leva em consideração a quantidade de variaveis explicativas
# colocadas no modelos, penalizando o modelo pelo excesso de variáveis




# Teste F -----------------------------------------------------------------
# Permite analisar se pelo menos um dos betas é estatisticamente
# significante para a explicação do comportamento de Y no modelo. 
# É responsável pelo valor do p-valor do teste global no modelo.

# H0: 𝛽1 = 𝛽2 = 𝛽3 = ⋯ = 𝛽𝑘 = 0
# H1: pelo menos um 𝛽 ≠ 0

# Na rejeição da hipótese nula, pelo menos um dos b ’s será estatisticamente
# diferente de zero para explicar o comportamento de Y -> p-valor abaixo do
# nível crítico (0,05, usualmente).




# Teste t -----------------------------------------------------------------
# Permite analisar se cada um dos parâmetros (betas), individualmente, é
# estatisticamente diferente de zero (no caso de regressão simples, apresenta a
# mesma significância da estatística F).

# H0: 𝛽 = 0 
# H1: 𝛽 ≠ 0

# Distribuição F ----------------------------------------------------------
# Graus de liberdade do modelo (k): corresponde a quantidade de variável X
# Grasu de liberdade do erro: (n-k)-1

# F = (SomaQuadrados Regressão/"k"grausLiberdade modelo)/ (SomaQuadrados erros/ n-k-1)




# Problema de significancia do Intercepto -------------------------------------
# As vezes o valor do intercepto pode nao ser significativo. quando isso acontece
# é uma indicacao de amostra pequena. Apesar de nao ser indicado, nao 
# podemos fazer nada.



# Dummies: Variáveis Qualitativas ----------------------------------------------
# Para trabalhar com variáveis qualitativas, estas devem ser transformadas para 
# variáveis dummies (one hot incode). Dummies São variáveis categóricas que 
# representam um atributo por meio de combinação binária (0 para a ausência 
# ou 1 para presença).


# Transformação  de BoxCox -----------------------------------------------------
# Quando os resíduos não possuem distribuição normal. Podemos transformar a 
# variável Y na tentativa de normalizar a distribuição e atender a esse pressuposto
# Determina Qual o valor de lambda (lambda varia entre –∞ e +∞) que maximiza a 
# aderência da distribuição da nova variável Y* à normalidade

# Só podemos aplicar boxcox para variável positiva que não contem zero. Então
# caso tenha zero temos que somar 1 ou 0.001

# Script Complementar: 2023_Transformação_Normalizacao

# Então, podemos usar o lambda que a análise fornecer na formula para transformar
# o Y. 
# 1- DESCOBRIR O MELHOR LAMBDA
# lambda <- car::powerTransform(variavel Y)
# 2- CRIAR VARIÁVEL TRANSFORMADA NO DATASET
# Ytransformado = (Y^λ - 1)/ λ

# ou simplesmente usar uma transformação já conhecida para transformar o Y, 
# usando como base o valor de lambda obtido

# Tabela de Resultado
# lambda    Transformação
# -2        y^-2
# -1        y^-1    - inversa
# -0.5      y^-0.5  - raiz quadratica inversa
# 0         log(y)  - logaritma ln(x) - logaritmo natural
# 0.5       sqrt(y) - raiz quadrada
# 1         y       - linear
# 2         y^2     - quadratica
# 3         y^3     - cubica



#     #     #     #       #      #      #     #       #       #
# Exemplo: Analise lm ----------------------------------------------------------
dados <- read.table("raw_datas_exemplos/especies.txt",header = T)
head(dados)

# Modelo nulo
mod0 <- lm(Species ~ 1, data= dados)
# Modelo 1
mod1 <- lm(Species ~ Biomass, data= dados)
# Modelo 2
mod2 <- lm(Species ~ Biomass+pH, data= dados)
# Modelo 3
mod3 <- lm(Species ~ Biomass+pH+Biomass:pH, data= dados)

# Comparação de Modelos usando ANOVA
# Se os modelos são diferentes (p<0.05), escolher o modelo mais complexo
# Se os modelos não são diferentes (p>0.05), manter o modelo mais simples
anova(mod0,mod1)
anova(mod1,mod2)
anova(mod2,mod3)

# Selecionar o modelo 2
lm <- lm(Species ~ Biomass + pH, data= dados)
summary(lm)
plot(lm)  
summary(mod2)


#     #     #     #       #      #      #     #       #       #
# Exploratorio ------------------------------------------------------------
dplyr::glimpse(Boston) 


#x ------------------------------------------------------------------
# Análise de Regressão SIMPLES --------------------------------------------

  # dados Boston de Exemplo
  library(MASS) 
  # Dados
  data(Boston)
  
  # Analise
  mod_simples <- lm (medv ~ rm, Boston) 



# Resumo dos Resultados ---------------------------------------------------
  summary(mod_simples)
  
# Call: Mostra a fórmula que rodou a análise. Muito útil para olhar os resultados
#       depois da análise pronta.

# Residuals: Para olhar a distribuição dos resíduos. Conseguimos ver se os 
#            pressupostos da distribuição foram cumpridos.
#            A média dos resíduos deve estar próxima a ZERO e os valores de 
#            minimo e máximo devem estar próximos em valor absoluto.
  
# Coefficients: 
  # Intercept: O valor do intercepto tem importância no contexto matemático, 
  # mas pouca utilidade para a interpretação do problema. Significa que quando
  # a variável explicativa for igual a ZERO, esperamos que a variável resposta 
  # seja igual ao intercepto.
  # Slope: O aumento em 1 unidade de X (var. explicativa) provoca aumento/diminuição
  # do Y em "valor do slope" unidades de Y.

# Residual standard error: Variação residual. Expressa a variação das observações 
#                          em torno da linha de regressão.  

# R-square: Indica a quantidade de variação em Y (variável resposta) que é 
#           explicada pela variável explicativa (X). Ao multiplicar por 100,
#           temos o percentual. Ao tirar a raiz quadrada desse valor, temos
#           o coeficiente de correlação de Pearson.

# Adjusted R-square: O R-square tem um problema que ele sempre aumenta de valor
# conforme aumentamos o numero de preditores. Isso porque aumenta a quantidade
# de variação que é explicada. O R-ajustado tenta contornar esse problema, 
# compensanso o número de parâmetros usados. Portanto, pode ser usado para 
# comparar modelos. Ao adicionar mais uma variável podemos ver o quanto ela 
# interfere no R-ajustado, se ela diminuir muito o R-ajustado em comparação
# com o R-square, então não vale a pena colocar essa nova variável.
  
# F-statistic: é mais importante na regressão múltipla. Quanto maior 
# o valor de F, maior a inclinação da reta e menor é p-valor.

# p-value: Probabilidade de obter a razão F observada se a hipótese nula for
# verdadeira. Se a probabilidade de obter a razão observada for pequena (<0.05)
# a probabilidade de obter o nosso resultado ao acaso é improvável. Então,
# rejeitamos a hipótese nula (B1=0) e concluímos que o modelo de regressão 
# explica mais variação do que o esperado apenas ao acaso.   
  

# ALTERNATIVA AO SUMMARY
  jtools::summ(mod_simples, confint = T, digits = 4, ci.width = .95)
  jtools::export_summs(mod_simples, scale = F, digits = 4)
  

# Outras formas de visualizar os resultados --------------------------------

coef(mod_simples)                             # coeficientes
confint(mod_simples)                          # intervalo confianca
predict(mod_simples)                          # valores preditos
predict(mod_simples, interval = "confidence", level = 0.95) # valores preditos c/ intervalos
res <-  broom::augment(mod_simples)           # resíduos|cook distance|erro padrão|intervalos de confiança

# ou 
broom::augment(mod_simples)

# ou 
gtsummary::tbl_regression(mod_simples)

# Grafico Regressão ---------------------------------------------------------

# Grafico da regressao 
ggplot(Boston, mapping = aes(y = medv, x=rm))+
  geom_point()+
  geom_line(res, mapping = aes(y =.fitted , x=rm))+
  theme_classic()

# ou
plot(medv ~ rm, Boston)
abline(mod_simples)

# ou
ggplot(data = Boston, aes(x = rm, y = medv)) +
  geom_point() +
  stat_smooth(method = "lm", se = FALSE)+
  theme_classic()

# ou (interativo)
ggplotly(
  ggplot(Boston, aes(x = rm, y = medv)) +
    geom_point(color = "#39568CFF", size = 2.5) +
    geom_smooth(aes(color = "Fitted Values"),
                method = "lm", formula = y ~ x, se = F, size = 2) +
    labs(x = "Distância",
         y = "Tempo",
         title = paste("R²:",
                       round(((cor(Boston$rm, Boston$medv))^2),4))) +
    scale_color_manual("Legenda:",
                       values = "grey50") +
    theme_classic()
)

# com intervalo de confianca de 95%
ggplotly(
  ggplot(Boston, aes(x = rm, y = medv)) +
    geom_point(color = "#39568CFF") +
    geom_smooth(aes(color = "Fitted Values"),
                method = "lm", formula = y ~ x,
                level = 0.95) +
    labs(x = "Distância",
         y = "Tempo") +
    scale_color_manual("Legenda:",
                       values = "grey50") +
    theme_bw()
)


# Graficos Diagnóstico ----------------------------------------------------

plot(lm (medv ~ rm, Boston) )

# Residual vs Fitted: Queremos nesse gráfico não observar padrões dos pontos 
# no grafico. Quando mais disperso os valores dos resíduos estiverem em torno 
# da linha do Zero, melhor. ESSE GRÁFICO TESTA A HOMOCEDASTICIDADE DO RESÍDUO.

# Normal Q-Q: Queremos que os pontos estejam praticamente em cima da linha 
# pontilhada prevista. Se os pontos saem da reta prevista, significa que os 
# resíduos da regressão não são normais. ESSE GRÁFICO TESTA A NORMALIDADE DOS 
# RESÍDUOS. INDICIO DE TERMO UMA RELAÇÃO LINEAR ENTRE AS VARIÁVEIS.

# Scale-Location:

# Residuals vs Leverage: Distancia de cook mostra quais são os pontos influentes.
# Pontos com potencial para alavancar a reta da regressão. Os pontos que 
# aparecem com as linhas ao lado, são outliers. As linhas pontilhadas(0.5, 1) 
# mostram a gravidade de influencia desses pontos. A distancia de cook mede o 
# efeito de excluir uma observação. Ajusta uma reta a cada vez que roda, deixando 
# um ponto de fora.
# Valores podem ser acessados:
cooks.distance(mod_simples)

# PRESSUPOSTO: Normalidade dos resíduos
  # Exemplo
    library(statsr)
    library(ggplot2)
    data("mlb11")
    m1 <- lm(runs ~ at_bats, data = mlb11)
    

    
    # 1. Grafico qqplot
    plot(m1)
    #ou
    ggplot(data = m1, aes(sample = .resid)) +
          stat_qq()

  # 2. Historama: com a distribuição dos resíduos
    ggplot(data = m1, aes(x = .resid)) +
        geom_histogram(binwidth = 25) +
        xlab("Residuals")

# PRESSUPOSTO: Homocedasticidade dos resíduos
  
  ggplot(data = m1, aes(x = .fitted, y = .resid)) +
      geom_point() +
      geom_hline(yintercept = 0, linetype = "dashed") +
      xlab("Fitted values") +
      ylab("Residuals")

  
# Pontos de Influencia ----------------------------------------------------
# Outliers podem ser pontos de influencia. Dependendo de onde ele está localizado
# pode servir de ponto de alavanca e interferir na inclinação da reta. Se ele
# está nas extremidades da distribuição dos pontos, pode ser alavanca.




# Predição ----------------------------------------------------------------

# Modela
  model2 <- lm(runs ~ at_bats, data = mlb11)
  summary(model2)

# Valor Predito de runs quando at_bats = 5579
  predict_runsvalue <- -2789.2429 + (0.6305 * 5579)
  predict_runsvalue

# Valor Observado: Para 5579 foi observado o valor de 713 runs
observed_runsvalue <- mlb11 %>% 
                          select(runs, at_bats) %>% 
                          filter(at_bats == 5579)
# Resíduos
residualvalue <- observed_runsvalue$runs - predict_runsvalue 
residualvalue
  
# Grafico da regressão
ggplot(data = mlb11, aes(x = at_bats, y = runs)) +
  geom_point() +
  stat_smooth(method = "lm", se = FALSE)


# Comparação de Modelos ---------------------------------------------------
mod_1 <- lm (medv ~ rm, Boston) 
mod_2 <- lm (medv ~ ., Boston) #todas as variaveis
# funcao comparacao
jtools::export_summs(mod_1,mod_2, scale = F, digits = 4)



#x ------------------------------------------------------------------
# Analise Regressão Multipla ----------------------------------------------

# Dados Boston de Exemplo
library(MASS) 

# Analise


# METODO 1 - interpretacao direta. Uma unidade de X afeta Y de acordo com  a 
# estimativa de beta. Neste metodo, nao ocorreu padronizacao das variaveis. 
# Entao nao podemos inferir qual seria a variavel mais importante para o modelo
# porque a escala das variaveis nao e a mesma. Este caso nao-padronizado eh
# melhor para predicao e para ver o quanto que a alteracao de uma unidade varx
# altera em var.y, mas nao para comparar o efeito de cada variavel
mod_multiplo <- lm (medv ~ rm + crim, Boston) 
mod_multiplo <- lm (medv~ . -rm, Boston) # excluindo rm da analise


# METODO 2 - Neste caso, padronizamos todas as variaveis (inclusive a variavel
# resposta) para determinar qual e a magnitude que a variavel preditora modifica
# a variavel resposta. Para remover o efeito das diferencas de escala entre as 
# variaveis respostas padronizamos  (observacao-media/desvio padrao). Assim, 
# removemos o efeito da escala e podemos de maneira correta observar o efeito 
# da preditora na var. resposta em termos de desvio padrao. Temos o efeito na 
# mesma escala para todas as variaveis, o que permite fazer uma inferencia.
# Regressao padronizando todas as variaveis
library(arm)
mod_multiplo = standardize(lm(medv~ . , Boston),
            standardize.y = TRUE)
summary(mod_multiplo)

# ou 
broom::augment(mod_multiplo)

# ou 
gtsummary::tbl_regression(mod_multiplo)


# Graficos Diagnóstico
plot(mod_multiplo)


# Colinearidade -----------------------------------------------------------
# A colinearidade ocorre quando existe uma alta correlacao entre duas 
# variaveis preditoras que, normalmente, representam o mesmo fenomeno 
# ou muitos similares.



#------ Identificar a colinearidade

# - correlacao entre as variaveis preditoras
cor.test()


# - graficos de scatterplot entre as variaveis



# - Gráfico de correlação
#A função 'correlation' do pacote 'correlation' faz com que seja estruturado um
#diagrama interessante que mostra a inter-relação entre as variáveis e a
#magnitude das correlações entre elas
#Requer instalação e carregamento dos pacotes 'see' e 'ggraph' para a plotagem
library(see)
library(ggraph)
library(correlation)
library(MASS)
data("Boston")
Boston %>%
  correlation(method = "pearson") %>%
  plot()

#A função 'chart.Correlation' do pacote 'PerformanceAnalytics' apresenta as
#distribuições das variáveis, scatters, valores das correlações e suas
#respectivas significâncias
PerformanceAnalytics::chart.Correlation((Boston[2:4]), histogram = TRUE)






# Interacao como deve ser inserida
## uma forma de representar a interacao - faz todas as interacoes entre 
## as variaveis utilizadas no modelo
modelo2 <- lm(log_riqueza ~ log_area * precipitacao, data = ilhas)

## outra forma de representar a interacao
modelo2 <- lm(log_riqueza ~ log_area + precipitacao + log_area : precipitacao,
              data = ilhas)




# Reportar sempre R-square ajustado (leva em consideracao o numero de variaveis
# preditoras no modelo para estimar o coeficiente de determinacao);



# OBS: COLINEARIDADE
# Quando acontece isso temos um problema. Temos estimativas equivocadas.
# Nesses casos a regressão pode dar significativa, enquanto as variáveis em si,
# não dão significativas para as suas inclinações. Neste caso temos q usar apenas
# uma das variáveis.




# Tidymodels --------------------------------------------------------------
library(tidymodels)
library(MASS) 
Boston

receita <- 
  linear_reg() |> 
  set_engine("glm") |> 
  set_mode("regression") 

mod1 <- receita |> 
  fit(medv ~ rm, data = Boston)  
summary(mod1)
  


#x ------------------------------------------------------------------
# Exemplo1 -------------------------------------------------------------

# Linear Simples ----------------------------------------------------------


# Pacotes -----------------------------------------------------------------
#Pacotes utilizados
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

# Dados -------------------------------------------------------------------
#Listar os arquivos do nosso project
list.files()

#Carregando a base de dados
load(file = "raw_datas_exemplos/tempodist.RData")


# Exploratorio ------------------------------------------------------------
tempodist %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = F,
                font_size = 22)

#Visualizando as observações e as especificações referentes às variáveis do dataset
glimpse(tempodist) 

#Estatísticas univariadas
summary(tempodist)


# Grafico -----------------------------------------------------------------
ggplotly(
  ggplot(tempodist, aes(x = distancia, y = tempo)) +
    geom_point(color = "#39568CFF", size = 2.5) +
    geom_smooth(aes(color = "Fitted Values"),
                method = "lm", formula = y ~ x, se = F, size = 2) +
    labs(x = "Distância",
         y = "Tempo",
         title = paste("R²:",
                       round(((cor(tempodist$tempo, tempodist$distancia))^2),4))) +
    scale_color_manual("Legenda:",
                       values = "grey50") +
    theme_classic()
)

# Analise -----------------------------------------------------------------
#Estimando o modelo
modelo_tempodist <- lm(formula = tempo ~ distancia,
                       data = tempodist)




# Resultado ---------------------------------------------------------------

#Opção1
#Observando os parâmetros do modelo_tempodist
summary(modelo_tempodist)

#Opção2
# Outras maneiras de apresentar os outputs do modelo
# função 'summ' do pacote 'jtools'
summ(modelo_tempodist, confint = T, digits = 4, ci.width = .95)
export_summs(modelo_tempodist, scale = F, digits = 4)

#Visualização do modelo no ambiente Viewer
#função 'extract_eq' do pacote 'equatiomatic'
extract_eq(modelo_tempodist, use_coefs = T) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = F,
                font_size = 28)


# Salvar Fitted values ----------------------------------------------------
# Salvando fitted values (variável yhat) e residuals (variável erro) no dataset
tempodist$yhat <- modelo_tempodist$fitted.values
tempodist$erro <- modelo_tempodist$residuals

#Visualizando a base de dados com as variáveis yhat e erro
tempodist %>%
  select(tempo, distancia, yhat, erro) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 22)


# Grafico com R² ----------------------------------------------------------
# Gráfico didático para visualizar o conceito de R²
ggplotly(
  ggplot(tempodist, aes(x = distancia, y = tempo)) +
    geom_point(color = "#39568CFF", size = 2.5) +
    geom_smooth(aes(color = "Fitted Values"),
                method = "lm", formula = y ~ x, se = F, size = 2) +
    geom_hline(yintercept = 30, color = "grey50", size = .5) +
    geom_segment(aes(color = "Ychapéu - Ymédio", x = distancia, xend = distancia,
                     y = yhat, yend = mean(tempo)), size = 0.7, linetype = 2) +
    geom_segment(aes(color = "Erro = Y - Ychapéu", x = distancia, xend = distancia,
                     y = tempo, yend = yhat), size = 0.7, linetype = 3) +
    labs(x = "Distância",
         y = "Tempo") +
    scale_color_manual("Legenda:",
                       values = c("#55C667FF", "grey50", "#440154FF")) +
    theme_classic()
)

# Calculo Manual do R² -----------------------------------------------------------
R2 <- (sum((tempodist$yhat - mean(tempodist$tempo))^2))/
  ((sum((tempodist$yhat - mean(tempodist$tempo))^2)) + (sum((tempodist$erro)^2)))

round(R2, digits = 4)

# Coeficiente de ajuste (R²) é a correlação ao quadrado
cor(tempodist[1:2])

# Modelo auxiliar para mostrar R² igual a 100% (para fins didáticos)
# Note que aqui o yhat é a variável dependente
modelo_auxiliar <- lm(formula = yhat ~ distancia,
                      data = tempodist)
summary(modelo_auxiliar)

#Gráfico mostrando o perfect fit
my_plot <-
  ggplot(tempodist, aes(x = distancia, y = yhat)) +
  geom_point(color = "#39568CFF", size = 5) +
  geom_smooth(aes(color = "Fitted Values"),
              method = "lm", formula = y ~ x, se = F, size = 2) +
  labs(x = "Distância",
       y = "Tempo") +
  scale_color_manual("Legenda:",
                     values = "grey50") +
  theme_cowplot()
my_plot

#Com figuras JPEG e PNG
ggdraw() + #funções 'ggdraw', 'draw_image' e 'draw_plot' do pacote 'cowplot'
  draw_image("https://cdn.pixabay.com/photo/2021/12/14/16/32/harry-potter-6870854_960_720.png",
             x = 0.075, y = -0.15, scale = .44) +
  draw_image("https://img.freepik.com/fotos-premium/agulha-de-trico-isolada_93675-25968.jpg?w=1380",
             x = -0.235, y = 0.25, scale = .37) +
  draw_plot(my_plot)


# Grafico com intervalo confianca ----------------------------------------
##Voltando ao nosso modelo original:
#Plotando o Intervalo de Confiança de 95%
ggplotly(
  ggplot(tempodist, aes(x = distancia, y = tempo)) +
    geom_point(color = "#39568CFF") +
    geom_smooth(aes(color = "Fitted Values"),
                method = "lm", formula = y ~ x,
                level = 0.95) +
    labs(x = "Distância",
         y = "Tempo") +
    scale_color_manual("Legenda:",
                       values = "grey50") +
    theme_bw()
)


#Calculando os intervalos de confiança

confint(modelo_tempodist, level = 0.90) # siginificância 10%
confint(modelo_tempodist, level = 0.95) # siginificância 5%
confint(modelo_tempodist, level = 0.99) # siginificância 1%
confint(modelo_tempodist, level = 0.99999) # siginificância 0,001%

#Fazendo predições em modelos OLS - e.g.: qual seria o tempo gasto, em média, para
#percorrer a distância de 25km?
predict(object = modelo_tempodist,
        data.frame(distancia = 25))

#Caso se queira obter as predições com os IC
predict(object = modelo_tempodist,
        data.frame(distancia = 25),
        interval = "confidence", level = 0.95)


# x ------------------------------------------------------------------
# Exemplo 2 ---------------------------------------------------------------
# Linear Multipla ----------------------------------------------------------------


# dados
load('scripts/paises.RData')


#Estatísticas univariadas
summary(paises)

#Gráfico 3D com scatter
scatter3d(cpi ~ idade + horas,
          data = paises,
          surface = F,
          point.col = "#440154FF",
          axis.col = rep(x = "black",
                         times = 3))
library(car)
library(rgl)

#Estimando a Regressão Múltipla
modelo_paises <- lm(formula = cpi ~ . - pais,
                    data = paises)

#Parâmetros do modelo
summary(modelo_paises)
confint(modelo_paises, level = 0.95) # siginificância de 5%

# obs: se o intervalo de confiança não engloba o ZERO, temos um
# valor significativo para os parâmetros betas

#Outro modo de apresentar os outputs do modelo - função 'summ' do pacote 'jtools'
summ(modelo_paises, confint = T, digits = 3, ci.width = .95)
jtools::export_summs(modelo_paises, scale = F, digits = 5)

#Salvando os fitted values na base de dados
paises$cpifit <- modelo_paises$fitted.values

#Gráfico 3D com scatter e fitted values
scatter3d(cpi ~ idade + horas,
          data = paises,
          surface = T, fit = "linear",
          point.col = "#440154FF",
          axis.col = rep(x = "black",
                         times = 3))


# x ----------------------------------------------------------------
# Exemplo 3 ---------------------------------------------------------------
# Var.Qualitativa ---------------------------------------------------------

# Com variável qualitativa precisamos transformar a variável categorica 
# em variáveis dummies. Nao podemos simplesmente usar como categorias

#dados
load(file = "scripts/corrupcao.RData")
#Visualização das observações e das especificações referentes
#às variáveis da base de dados
glimpse(corrupcao) 

#Observando os rótulos da variável regiao
levels(glimpse(corrupcao$regiao)) 

#Tabela de frequências da variável regiao
table(corrupcao$regiao) 


#Estatísticas univariadas
summary(corrupcao)

#Exploração visual do Corruption Perception Index para cada um dos países
corrupcao %>%
  group_by(regiao) %>%
  mutate(rotulo = paste(pais, cpi)) %>%
  ggplot(aes(x = as.numeric(regiao), y = cpi, label = rotulo)) +
  geom_point(aes(x = regiao, y = cpi), color = "#FDE725FF", alpha = 0.5, size = 5) +
  scale_color_manual("Legenda:",
                     values = "#440154FF") +
  labs(x = "Região",
       y = "Corruption Perception Index") +
  geom_text_repel() +
  theme_bw()

#Exploração visual do Corruption Perception Index para cada um dos países, com
#valores médios por região
corrupcao %>%
  group_by(regiao) %>%
  mutate(cpi_medio = mean(cpi, na.rm = TRUE)) %>%
  mutate(rotulo = paste(pais, cpi)) %>%
  ggplot(aes(x = as.numeric(regiao), y = cpi, label = rotulo)) +
  geom_point(aes(x = regiao, y = cpi), color = "#FDE725FF", alpha = 0.5, size = 5) +
  geom_line(aes(x = regiao, y = cpi_medio, 
                group = 1, color = "CPI Médio"), linewidth = 1.5) +
  scale_color_manual("Legenda:",
                     values = "#440154FF") +
  labs(x = "Região",
       y = "Corruption Perception Index") +
  geom_text_repel() +
  theme_bw() +
  theme(legend.position = "bottom")

#Estimando um modelo ERRADO, com o problema da ponderação arbitrária
modelo_corrupcao <- lm(formula = cpi ~ as.numeric(regiao), 
                       data = corrupcao)

#Observando os parâmetros do modelo_corrupcao
summary(modelo_corrupcao)

#Calculando os intervalos de confiança
confint(modelo_corrupcao, level = 0.95) # siginificância 5%

#Plotando os fitted values do modelo_corrupcao considerando, PROPOSITALMENTE, a
#ponderação arbitrária, isto é, assumindo que a América do Sul vale 1; que a 
#Oceania vale 2; a Europa, 3; EUA e Canadá, 4; e Ásia, 5.
corrupcao %>%
  mutate(rotulo = paste(pais, cpi)) %>%
  ggplot(aes(x = as.numeric(regiao), y = cpi, label = rotulo)) +
  geom_point(color = "#FDE725FF", alpha = 0.5, size = 4) +
  stat_smooth(aes(color = "Fitted Values"),
              method = "lm", 
              formula = y ~ x,
              se = T) +
  labs(x = "Região",
       y = "Corruption Perception Index") +
  scale_x_discrete(labels = c("1" = "América do Sul", 
                              "2" = "Oceania", 
                              "3" = "Europa", 
                              "4" = "EUA e Canadá", 
                              "5" = "Ásia")) +
  scale_color_manual("Legenda:",
                     values = "#440154FF") +
  geom_text_repel() +
  theme_bw() +
  theme(legend.position = "bottom")


# Dummy -------------------------------------------------------------------
#Dummizando a variável regiao. O código abaixo, automaticamente, fará: a) o
#estabelecimento de dummies que representarão cada uma das regiões da base de 
#dados; b)removerá a variável dummizada original; c) estabelecerá como categoria 
#de referência a dummy mais frequente.
library(fastDummies)
corrupcao_dummies <- dummy_columns(.data = corrupcao,
                                   select_columns = "regiao",
                                   remove_selected_columns = T,
                                   remove_most_frequent_dummy = T) #categoria de referencia sera a mais frequente

#Visualizando a base de dados dummizada
corrupcao_dummies %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 16)

# 

# Analise -----------------------------------------------------------------
#Modelagem com todas as variáveis
modelo_corrupcao_dummies <- lm(cpi ~ . - pais, corrupcao_dummies)

#OBSERVAÇÕES
# note que se fizermos com a formula comum, o R automaticamente cria var.dummies 
# só que a categoria de referencia muda para ordem alfabetica 
modelo_corrupcao <- lm(cpi ~ . - pais, corrupcao)
summary(modelo_corrupcao)

#Parâmetros do modelo_corrupcao_dummies
summary(modelo_corrupcao_dummies)

#Plotando o modelo_corrupcao_dummies de forma interpolada
library(ggrepel)
my_plot3 <- 
  corrupcao %>%
  mutate(rotulo = paste(pais, cpi)) %>%
  ggplot(aes(x = as.numeric(regiao), y = cpi, label = rotulo)) +
  geom_point(color = "#FDE725FF", alpha = 0.5, size = 4) +
  stat_smooth(aes(color = "Fitted Values"),
              method = "lm", 
              formula = y ~ bs(x, df = 4),
              se = T) +
  labs(x = "Região",
       y = "Corruption Perception Index") +
  scale_x_discrete(labels = c("1" = "América do Sul", 
                              "2" = "Oceania", 
                              "3" = "Europa", 
                              "4" = "EUA e Canadá", 
                              "5" = "Ásia")) +
  scale_color_manual("Legenda:",
                     values = "#440154FF") +
  geom_text_repel() +
  theme_bw() +
  theme(legend.position = "bottom")
my_plot3

# salvar
ggsave("my_plot3.png")


# x ----------------------------------------------------------------
# Pressuposto: Normalidade -----------------------------------------------------

# Pacotes utilizados 
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


# Dados 
load(file = "bebes.RData")


# Exploratorio 
#Estatísticas univariadas
summary(bebes)

# Grafico Dispersão 
ggplotly(
  bebes %>% 
    ggplot() +
    geom_point(aes(x = idade, y = comprimento),
               color = "grey20", alpha = 0.6, size = 2) +
    labs(x = "Idade em semanas",
         y = "Comprimento em cm") +
    theme_bw()
)

# Grafico Dispersão com emoji 
ggplotly(
  bebes %>%
    ggplot(aes(x = idade, y = comprimento, label = emoji("baby_bottle"))) +
    geom_text(family = "EmojiOne", size = 5, color = "black") +
    labs(x = "Idade em semanas",
         y = "Comprimento em cm") +
    theme_bw()
)

# Grafico Dispersão com ajustes (fits) linear e não-linear 
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

#Estimação do modelo OLS linear
modelo_linear <- lm(formula = comprimento ~ idade,
                    data = bebes)

summary(modelo_linear)


# Normalidade -------------------------------------------------------------
# Para os modelos lineares precisaremos realizar um teste para testar os 
# erros em relação aos fitted values. Porque se não houver normalidade
# dos erros (resíduos) os betas estimados pelo modelo náo podem ser usados
# para fins preditivos. Os betas do modelo, os intervalos de confiança não
# é adequado para predição.

# Se não houver normalidade temos que investigar a forma funcional da relação
# entre as variáveis para saber se haverá outra relação que melhor representa
# a relação.


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
nortest::sf.test(modelo_linear$residuals) #função 'sf.test' do pacote 'nortest'


# Histograma dos resíduos -------------------------------------------------
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

# Visualização do comportamento dos resíduos em função dos fitted values do
# do modelo linear, com destaque para as distribuições das variáveis
# (pacote 'ggside')
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



# x -----------------------------------------------------------------------
# x -----------------------------------------------------------------------

# Regressao Multipla: Passo-a-Passo ---------------------------------------

# Dados -------------------------------------------------------------------
load(file = "raw_datas_exemplos/empresas.RData")

# Exploratorio ------------------------------------------------------------
summary(empresas)


# 1. Avaliar Correlações --------------------------------------------------

# Opcao 1 
#A função 'correlation' do pacote 'correlation' faz com que seja estruturado um
#diagrama interessante que mostra a inter-relação entre as variáveis e a
#magnitude das correlações entre elas
#Requer instalação e carregamento dos pacotes 'see' e 'ggraph' para a plotagem
empresas %>%
  correlation(method = "pearson") %>%
  plot()

# Opcao 2
#A função 'chart.Correlation' do pacote 'PerformanceAnalytics' apresenta as
#distribuições das variáveis, scatters, valores das correlações e suas
#respectivas significâncias
chart.Correlation((empresas[2:6]), histogram = TRUE)

# Opcao 3
#A função 'pairs.panels' do pacote 'psych' também apresenta as distribuições
#das variáveis, scatters, valores das correlações e suas respectivas
#significâncias
pairs.panels(empresas[2:6],
             smooth = TRUE,
             lm = TRUE,
             scale = FALSE,
             density = TRUE,
             ellipses = FALSE,
             method = "pearson",
             pch = 1,
             cor = TRUE,
             hist.col = "aquamarine",
             breaks = 12,
             stars = TRUE,       # If TRUE, adds significance level with stars
             ci = TRUE, alpha = 0.05)

# Opcao 4
#A função 'corr_plot' do pacote 'metan' também apresenta as distribuições
#das variáveis, scatters, valores das correlações e suas respectivas
#significâncias
install.packages("metan")
library(metan)
empresas %>%
  corr_plot(retorno, disclosure, endividamento, ativos, liquidez,
            shape.point = 21,
            col.point = "black",
            fill.point = "#FDE725FF",
            size.point = 2,
            alpha.point = 0.6,
            maxsize = 4,
            minsize = 2,
            smooth = TRUE,
            col.smooth = "black",
            col.sign = "#440154FF",
            upper = "corr",
            lower = "scatter",
            diag.type = "density",
            col.diag = "#440154FF",
            pan.spacing = 0,
            lab.position = "bl")


# 2. Modelo Multiplo ---------------------------------------------------------
#Visualizando a base de dados
empresas %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 22)

# Estimando a Regressão Múltipla
modelo_empresas <- lm(formula = retorno ~ . - empresa,
                      data = empresas)
# Parâmetros do modelo
summary(modelo_empresas)

# Endividamento nao foi significativo
modelo_empresas2 <- lm(retorno ~ . -empresa -endividamento, 
                       data = empresas)
#Parâmetros do modelo
summary(modelo_empresas2)

# agora disclousure perdeu a significancia, vamos retirar tb
modelo_empresas3 <- lm(retorno ~ . -empresa -endividamento -disclosure, 
                       data = empresas)
#Parâmetros do modelo
summary(modelo_empresas3)

# 3. Stepwise: Selecao de variáveis ----------------------------------------------------

# 1. Definir o argumento k. Este argumento serve para selecionar apenas os 
# betas significativos ao nível de confianca de 95% (0.05)
qchisq(p = 0.05, df = 1, lower.tail = F)
# resultado: [1] 3.841459 (valor que será usado na funcao do step())
# Checando o nível de significancia (0.05) do valor de K
round(pchisq(3.841459, df = 1, lower.tail = F), 7)

# 2. Stepwise automatico
step_empresas <- step(modelo_empresas, k = 3.841459)
summary(step_empresas)
# Este procedimento no R removeu a variável 'endividamento'. Note que a variável
# 'disclosure' também acabou sendo excluída após o procedimento Stepwise, nesta
# forma funcional linear!


# 3. Extrair resultado do Modelo ---------------------------------------------
jtools::export_summs(step_empresas, scale = F, digits = 5)

# Comparar modelos
jtools::export_summs(step_empresas, modelo_empresas3, scale = F, digits = 5)

#Parâmetros reais do modelo com procedimento Stepwise
confint(step_empresas, level = 0.95) # siginificância 5%
plot_summs(step_empresas, colors = "#440154FF") #função 'plot_summs' do pacote 'ggstance'



# 4. Comparar Importancia das variáveis -----------------------------------
# Para comparar importancia das variáveis temos q padronizar as variáveis 
# para ter betas variando na mesma escala.
# Parâmetros reais
ggstance::plot_summs(step_empresas, colors = "#440154FF")
# Parâmetros padronizados
ggstance::plot_summs(step_empresas, scale = TRUE, colors = "#440154FF")

# Adicionando a caracterização da distribição normal no IC de cada parâmetro beta
plot_summs(step_empresas, scale = TRUE, plot.distributions = TRUE,
           inner_ci_level = .95, colors = "#440154FF")

# Comparando os Intervalos de Confianca dos betas dos modelos sem e com procedimento Stepwise
plot_summs(modelo_empresas, step_empresas, scale = TRUE, plot.distributions = TRUE,
           inner_ci_level = .95, colors = c("#FDE725FF", "#440154FF"))



# 5. Teste de Aderencia: Normalidade --------------------------------------
# Shapiro-Francia: n > 30
nortest::sf.test(step_empresas$residuals) 

# Resultado p<0.05 - não existe normalidade

# Plotando os resíduos do modelo step_empresas
empresas %>%
  mutate(residuos = step_empresas$residuals) %>%
  ggplot(aes(x = residuos)) +
  geom_histogram(color = "white", 
                 fill = "#440154FF", 
                 bins = 30,
                 alpha = 0.6) +
  labs(x = "Resíduos",
       y = "Frequência") + 
  theme_bw()

# Acrescentando uma curva normal teórica para comparação entre as distribuições
empresas %>%
  mutate(residuos = step_empresas$residuals) %>%
  ggplot(aes(x = residuos)) +
  geom_histogram(aes(y = ..density..), 
                 color = "white", 
                 fill = "#440154FF", 
                 bins = 30,
                 alpha = 0.6) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(step_empresas$residuals),
                            sd = sd(step_empresas$residuals)),
                linewidth = 2, color = "grey30") +
  scale_color_manual(values = "grey50") +
  labs(x = "Resíduos",
       y = "Frequência") +
  theme_bw()



# 6. Transformação de Box Cox ---------------------------------------------
# Como os resíduos do modelo não foram aderentes a normalidade, vamos fazer
# a transformação de box cox na variável Y para tentar normalizar, e teremos 
# que rodar novamente o modelo stepwise.
# Nao funciona com zero

# 1. Calcular o Lambda de Box-Cox
lambda_BC <- car::powerTransform(empresas$retorno)
lambda_BC


# 2. Inserir o lambda do Box-Cox na base de dados para a estimação de um novo 
# modelo
# Nova coluna com o Y (retorno) recalculado pela formula de transformação de 
# box-cox usando o lambda obtido
empresas$bcretorno <- (((empresas$retorno ^ lambda_BC$lambda) - 1) / 
                         lambda_BC$lambda)

# 3. Visualizando a nova variável na base de dados
empresas %>%
  select(empresa, retorno, bcretorno, everything()) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 18)

# 7. Após Box-Cox:Estimar Novamente o Modelo  -----------------------------
# Estimando um novo modelo múltiplo com variável dependente transformada 
# por Box-Cox
modelo_bc <- lm(formula = bcretorno ~ . -empresa -retorno, 
                data = empresas)

# Parâmetros do modelo
summary(modelo_bc)

# Aplicando o procedimento Stepwise
qchisq(p = 0.05, df = 1, lower.tail = F)
step_modelo_bc <- step(modelo_bc, k = 3.841459)

# Resultado
summary(step_modelo_bc)
# Note que a variável 'disclosure' acaba voltando ao modelo na forma
# funcional não linear!


# 8. Verificar Normalidade Novamente --------------------------------------
# Verificando a normalidade dos resíduos do modelo step_modelo_bc
nortest::sf.test(step_modelo_bc$residuals)

# Plotando os novos resíduos do step_modelo_bc
empresas %>%
  mutate(residuos = step_modelo_bc$residuals) %>%
  ggplot(aes(x = residuos)) +
  geom_histogram(aes(y = ..density..),
                 color = "white",
                 fill = "#287D8EFF",
                 bins = 30,
                 alpha = 0.6) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(step_modelo_bc$residuals),
                            sd = sd(step_modelo_bc$residuals)),
                size = 2, color = "grey30") +
  scale_color_manual(values = "grey50") +
  labs(x = "Resíduos",
       y = "Frequência") +
  theme_bw()

# 9. Comparação Modelos: Com e Sem Box-Cox -----------------------------------
# Resumo dos dois modelos obtidos pelo procedimento Stepwise (linear e com Box-Cox)
jtools::export_summs(step_empresas, step_modelo_bc,
                     model.names = c("Modelo Linear","Modelo Box-Cox"),
                     scale = F, digits = 6)

# Parâmetros reais do modelo com procedimento Stepwise e Box-Cox
confint(step_modelo_bc, level = 0.95) # siginificância 5%
ggstance::plot_summs(step_modelo_bc, colors = "#287D8EFF")

# Parâmetros padronizados
# Neste caso, percebemos que o disclosure tem uma importancia relativa maior
# do que as demais variáveis no modelo
plot_summs(step_modelo_bc, scale = TRUE, colors = "#287D8EFF")

# Adicionando caracterização da distribição normal no IC de cada parâmetro beta
plot_summs(step_modelo_bc, scale = TRUE, plot.distributions = TRUE,
           inner_ci_level = .95, colors = "#287D8EFF")

# Comparando os ICs do betas dos modelos sem e com Transformação de Box-Cox
plot_summs(step_empresas, step_modelo_bc, scale = T, plot.distributions = TRUE,
           inner_ci_level = .95, colors = c("#440154FF", "#287D8EFF"))



# 7. Predição do Modelo ---------------------------------------------------
# Exemplo: qual é o valor do retorno, em média, para 
# disclosure igual a 50, liquidez igual a 14 e ativo igual a 4000
predict(object = step_modelo_bc, 
        data.frame(disclosure = 50, 
                   liquidez = 14, 
                   ativos = 4000),
        interval = "confidence", level = 0.95)
# fit      lwr      upr
# 1 3.702015 3.665555 3.738476

# ATENÇÃO !!!!
# Não podemos nos esquecer de fazer o cálculo para a obtenção do fitted
# value de Y (retorno a escala original)
# (((Y * Lambda) + 1 ))^ (1 / lambda))
(((3.702015 * -0.02256414) + 1)) ^ (1 / -0.02256414)
# [1] 47.74258


# Salvando os fitted values dos modelos step_empresas e step_modelo_bc no
# dataset empresas
empresas$yhat_step_empresas <- step_empresas$fitted.values
empresas$yhat_step_modelo_bc <- (((step_modelo_bc$fitted.values*(lambda_BC$lambda))+
                                    1))^(1/(lambda_BC$lambda))

# Visualizando os dois fitted values no dataset
# modelos step_empresas e step_modelo_bc
empresas %>%
  select(empresa, retorno, yhat_step_empresas, yhat_step_modelo_bc) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 22)

# Ajustes dos modelos: valores previstos (fitted values) X valores reais
# Atenção! Aqui não temos X. Temos o valor predito vs valor real. o 
# traçado é a relação perfeita (quando acertamos todOs Os valores). 
# Atenção !! o tracejado não é o modelo linear!!!O linear é o verde e o 
# não linear é o roxo
empresas %>%
  ggplot() +
  geom_smooth(aes(x = retorno, y = yhat_step_empresas, color = "Stepwise"),
              method = "lm", se = F, formula = y ~ splines::bs(x, df = 5), size = 1.5) +
  geom_point(aes(x = retorno, y = yhat_step_empresas),
             color = "#440154FF", alpha = 0.6, size = 2) +
  geom_smooth(aes(x = retorno, y = yhat_step_modelo_bc, color = "Stepwise Box-Cox"),
              method = "lm", se = F, formula = y ~ splines::bs(x, df = 5), size = 1.5) +
  geom_point(aes(x = retorno, y = yhat_step_modelo_bc),
             color = "#287D8EFF", alpha = 0.6, size = 2) +
  geom_smooth(aes(x = retorno, y = retorno), method = "lm", formula = y ~ x,
              color = "grey30", size = 1.05,
              linetype = "longdash") +
  scale_color_manual("Modelos:", 
                     values = c("#287D8EFF", "#440154FF")) +
  labs(x = "Retorno", y = "Fitted Values") +
  theme(panel.background = element_rect("white"),
        panel.grid = element_line("grey95"),
        panel.border = element_rect(NA),
        legend.position = "bottom")


#x------------------------------------------------------------------
#x------------------------------------------------------------------

# Pressuposto: Multicolinearidade -----------------------------------------
# A colinearidade ocorre quando existe uma alta correlacao entre duas 
# variaveis preditoras que, normalmente, representam o mesmo fenomeno 
# ou muitos similares.A colinearidade ocorre quando duas variáveis carregam
# a mesma informação. A multicolinearidade pode ser responsável pela 
# exclusão de variáveis do modelo. Isso não significa que a variável 
# excluída não explica a variação nos dados, mas que explica a mesma 
# porção da variável que já foi explicada por outra variável na qual 
# ela é correlacionada.

# Consequencias -----------------------------------------------------------
# - possibilidade de interpretações erradas pela eventual distorção 
#   dos sinais dos parâmetros
# - erros nas predições




# Pacotes
library(dplyr)
library(correlation)

# Dados
load("raw_datas_exemplos/salarios.RData")

# Estatísticas univariadas
summary(salarios)
glimpse(salarios)

salarios |> 
  kableExtra::kable() |> 
  kableExtra::kable_styling(bootstrap_options = "striped",
                            full_width = F, font_size = 22)

## Exemplo 1
## CORRELAÇÃO PERFEITA:
# Quando temos uma correlação perfeita (ou quase perfeita)
# não conseguiremos estimar dois betas, só conseguiremos 
# estimar 1 beta, pq de uma das variáveis o beta virá vazio

# Opcao1
cor(salarios$rh1, salarios$econometria1)
# Opcao2
salarios|> select(2:4)|>  
  correlation::correlation(method = "pearson")|> 
  plot()
# Opcao3
PerformanceAnalytics::chart.Correlation(salarios[2:8], histogram = T)

# Modelo 1
modelo1 <- lm(formula = salario ~ rh1 + econometria1,
              data = salarios)
summary(modelo1)

# Call:
#   lm(formula = salario ~ rh1 + econometria1, data = salarios)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -149.53  -89.98  -63.85  118.46  261.84 
# 
# Coefficients: (1 not defined because of singularities)
# Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   1213.80     109.75  11.060 5.53e-08 ***
#   rh1            127.87      16.26   7.865 2.69e-06 ***
#   econometria1       NA         NA      NA       NA    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 135.2 on 13 degrees of freedom
# Multiple R-squared:  0.8263,	Adjusted R-squared:  0.813 
# F-statistic: 61.85 on 1 and 13 DF,  p-value: 2.695e-06

# Conclusão: O beta da econometria1 (perfeitamente correlacionada com a 
# rh1) vem como NA. Isso acontece porque elas são perfeitamente correlacionadas. 
# Nas variáveis quase perfeitamente correlacionada ocorre a mesma coisa, mas 
# resguardando a quantidade de correlação, como nem sempre é perfeitamente correlacionado
# vem um valor bem pequeno de explciação, não vai vir NA (zerado)


# se Utilizar o procedimento stepwise obteríamos o mesmo modelo que consideramos
# ideal acima, o que conta apenas com uma das var. correlacionadas. Pois, o 
# modelo step já toma esse cuidado de eliminar var. altamente correlacionadas
modelo1_step <- step(lm(formula = salario ~ rh1 + econometria1, data = salarios),
                     k = (qchisq(p = 0.05, df = 1, lower.tail = F)))
summary(modelo1_step)


## Exemplo 2
## CORRELAÇÃO BAIXA:
cor(salarios$rh3, salarios$econometria3)

salarios %>% select(2,7,8) %>% 
  correlation(method = "pearson") %>%
  plot()

modelo3 <- lm(formula = salario ~ rh3 + econometria3,
              data = salarios)
summary(modelo3)


# Diagnóstico -------------------------------------------------------------
# A tolerancia = 1 - R² da variavel preditora (de uma variavel explicativa contra
# a outra) para avaliar a magnitude. Tolerancia quanto mais perto de 1, significa uma 
# multicolinearidade baixa. A tolerancia próximo de zero significa alta colinearidade
# A tolerÂnica é zero quando a correlação for perfeita

# VIF: Quando VIF próximo a 1 significa baixa colinearidade. Quando VIF tende
# ao infinito, temos multicolinearidade (ex. modelo1)
# VIF = 1/Tolerance


# Diagnóstico de multicolinearidade (Variance Inflation Factor e Tolerance)
olsrr::ols_vif_tol(modelo3) # sem colinearidade
olsrr::ols_vif_tol(modelo1) # alta colinearidade (correlação perfeita)


# Modelo Auxiliar: Uma variável preditora contra a outra
modelo_aux3 <- lm(rh3 ~ econometria3, data= salarios)
summary(modelo_aux3)

# Podemos calcular manualmente a Tolerancia e o VIF
# Tolerancia
tolerance = 1 - 0.07027 # (valor do R-squared)
tolerance
# 0.92973 - como a Tolerancia deu próximo a 1, deu indicio de baixa colinearidade
# VIF
VIF <- 1/tolerance
VIF
# 1.075581 - Como VIF proximo a 1, deu indicio de baixa colinearidade

# Opção 2 - rodar lm com as variáveis q estamos testando a colinearidade
olsrr::ols_vif_tol(modelo_aux3)


# Como Detectar? -----------------------------------

# 1. Teste-t não significativo e Teste F significativo (p<0.05 no global)
#     As vezes o p é menor do que 0.05 no teste global (teste F)
#     Mas os valores de beta não dão significativo (teste t)


# Call:
#   lm(formula = salario ~ rh2 + econometria2, data = salarios)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -153.00  -97.61  -58.55  107.97  261.88 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)    1241.9      130.2   9.540 5.94e-07 ***
#   rh2             194.1      152.1   1.276    0.226    
# econometria2   -139.5      318.3  -0.438    0.669    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 139.6 on 12 degrees of freedom
# Multiple R-squared:  0.8291,	Adjusted R-squared:  0.8006 
# F-statistic:  29.1 on 2 and 12 DF,  p-value: 2.495e-05


# Isso pode acontecer quando temos variáveis muito correlacionadas entre si
# no modelo.

# Vamos olhar:
## CORRELAÇÃO MUITO ALTA, PORÉM NÃO PERFEITA:
cor(salarios$rh2, salarios$econometria2)
salarios %>% select(2,5,6) %>% 
  correlation::correlation(method = "pearson") %>%
  plot()
# MOdelo
modelo2 <- lm(formula = salario ~ rh2 + econometria2,
              data = salarios)
summary(modelo2)
ols_vif_tol(modelo2)

# Conclusão: Olha o valor do VIF (82.06), significa que temos uma correlação
# alta entre essas variáveis. Elas explicam praticamente a mesma coisa que a 
# outra.

# Se refizer o modelo só com uma variável melhora: o p continua significativo
# o beta passa a ser significativo e o R² se mantém com 82% de explicação
modelo2 <- lm(formula = salario ~ rh2,
              data = salarios)
summary(modelo2)

# 2. Sinais inesperados dos coeficientes


# Fontes --------------------------------------------
# Fontes de Multicolinearidade
#> 1 - Existencia de variáveis que apresentam a mesma tendência durante alguns
#>     períodos, em decorrência da seleção de uma amostra que inclua apenas
#>     observações referentes a estes períodos
#> 2 - Utilização de amostras com reduzido número de observações
#> 3 - Utilização de valores defasados em algumas das variáveis explicativas
#>     como "novas" explicativas



# x -----------------------------------------------------------------------
# x -----------------------------------------------------------------------

# Pressuposto: Heterocedasticidade ----------------------------------------
# Heterocedasticidade: significa que existe correlação entre o valor de X
# e o termo de erro. Neste caso, quanto maior o valor de X, maior o erro
# se a correlação entre o termo de erro e o valor de X for significativamente
# diferente de zero, temos heterocedasticidade ocorrendo. Variação dos termos
# de erro não são constantes ao longo de X.

# Isso mostra a omissão de variáveis X importantes para explicar Y. Alguma
# variável importante pode não ter entrado no modelo. 


# Indícios ----------------------------------------------------------------
# - os erros não estão distribuídos de forma aleatória. Formam um cone.
# Indicando que quanto maior o valor de X, maior o erro. Isso pode afetar a 
# predição na parte mais aberta do cone
# - forma inadequada do modelo (o linear não seria o adequado).



# Dados
# Fatores que podem interferir no aprendizado dos estudantes
load(file = "raw_datas_exemplos/saeb_rend.RData")

glimpse(saeb_rend)
saeb_rend$codigo <- as.character(saeb_rend$codigo)

# Analise exploratória
summary(saeb_rend)

# Tabela de frequências absolutas das variáveis 'uf' e rede'
table(saeb_rend$uf)
table(saeb_rend$rede)


# Exemplo de Heterocedasticidade - aumento do erro, com o aumento de X
# Plotando saeb em função de rendimento, com linear fit
ggplotly(
  ggplot(saeb_rend, aes(x = rendimento, y = saeb)) +
    geom_point(size = 1, color = "#FDE725FF") +
    geom_smooth(method = "lm", formula = y ~ x,
                color = "grey40", se = F) +
    xlab("rendimento") +
    ylab("saeb") +
    theme_classic()
)

# Plotando saeb em função de rendimento, com destaque para rede escolar 
ggplotly(
  ggplot(saeb_rend, aes(x = rendimento, y = saeb, color = rede, shape = rede)) +
    geom_point(size = 1) +
    xlab("rendimento") +
    ylab("saeb") +
    scale_colour_viridis_d() +
    theme_classic()
)

# Plotando saeb em função de rendimento, com destaque para rede escolar e linear fits
ggplotly(
  ggplot(saeb_rend, aes(x = rendimento, y = saeb, color = rede, shape = rede)) +
    geom_point(size = 1) +
    geom_smooth(method = "lm", formula = y ~ x, se = F) +
    xlab("rendimento") +
    ylab("saeb") +
    scale_colour_viridis_d() +
    theme_classic()
)


# Diagnóstico -------------------------------------------------------------

#Estimação do modelo
modelosaeb <- lm(formula = saeb ~ rendimento,
                 data = saeb_rend)

summary(modelosaeb)

# Call:
#   lm(formula = saeb ~ rendimento, data = saeb_rend)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -3.0566 -0.4593  0.0189  0.4762  3.3058 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  3.24246    0.03941   82.28   <2e-16 ***
#   rendimento   2.06646    0.04481   46.11   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.7241 on 25528 degrees of freedom
# (18077 observations deleted due to missingness)
# Multiple R-squared:  0.07689,	Adjusted R-squared:  0.07685 
# F-statistic:  2126 on 1 and 25528 DF,  p-value: < 2.2e-16


# Temos o p significativo para o teste F e para o teste t (beta).
# Mas o R² foi muito pequeno.


# Teste Breusch-Pagan  ------------------------------------------------------------------
# Usado para diagnóstico de heterocedasticida
#     H0 do teste: ausência de heterocedasticidade.
#     H1 do teste: heterocedasticidade, ou seja, correlação entre resíduos e uma ou mais
#                  var. explicativas
olsrr::ols_test_breusch_pagan(modelosaeb)

# Conclusão: Se Prob > Chi2 for <0.05, existe heterocedasticidade. Que pode ter
# ocorrido pela omissão de variável(is) explicativa(s) relevante(s)


#     #     #     #       #      #      #     #       #       #
# Abrindo a caixa preta do teste

# Extrair os valores preditos
saeb_rend$yhat <- modelosaeb$fitted.values # Não funciona pq tem NAs
saeb_rend$yhat <- predict(object = modelosaeb, newdata = saeb_rend) #fazer assim para enganar o r

#extrair os resíduos
saeb_rend$resid <- modelosaeb$residuals # Não funciona pq tem NAs
saeb_rend$resid <- saeb_rend$saeb - saeb_rend$yhat  # observado - esperado -> fazer assim para enganar o r

# formula do teste:
saeb_rend$up <- ((saeb_rend$resid)^2)/((sum(saeb_rend$resid^2, na.rm =TRUE))/(25530))
modelo_aux <- lm(up ~ yhat, data = saeb_rend)
# pegar a soma dos quadrados da regressao e dividir por 2
anova(modelo_aux)
pchisq(33.441/2,df=1,lower.tail = F)

saeb_rend$yhat <- NULL
saeb_rend$resid <- NULL
saeb_rend$up <- NULL

#     #     #     #       #      #      #     #       #       #





#     #     #     #       #      #      #     #       #       #

# Heterocedasticidade para Variáveis Dummies -------------------------------------
saeb_rend_dummies_uf <- dummy_columns(.data = saeb_rend,
                                      select_columns = "uf",    # selecionar coluna uf para virar dummie
                                      remove_selected_columns = T,    # remover coluna original sem ser dummie
                                      remove_most_frequent_dummy = T) # categoria mais frequente como referencia


# Colocando a primeira variável dummie que aparece como referencia
saeb_rend_dummies_uf <- dummy_columns(.data = saeb_rend,
                                      select_columns = "uf",    
                                      remove_selected_columns = T,    
                                      remove_first_dummy = T) # Primeira categoria como referencia


# Escolhendo manualmente a categoria de referencia
saeb_rend_dummies_uf <- dummy_columns(.data = saeb_rend,
                                      select_columns = "uf",    
                                      remove_selected_columns = T) 
saeb_rend_dummies_uf <- saeb_rend_dummies_uf[,-11] # a 11 coluna sera referencia




# Diagnóstico -------------------------------------------------------------
modelosaeb_dummies_uf <- lm(formula = saeb ~ . -municipio -codigo -escola -rede,
                            data = saeb_rend_dummies_uf)

summary(modelosaeb_dummies_uf)


# Teste de Breusch-Pagan para diagnóstico de heterocedasticidade
ols_test_breusch_pagan(modelosaeb_dummies_uf)

# Resultado: 
# DF            =    1 
# Chi2          =    1.075624 
# Prob > Chi2   =    0.2996785 

# Conclusão: Como H0 do teste significa ausência de heterocedasticidade, 
# não existe heterocedasticidade nesses dados


# Plotando saeb em função de rendimento, com destaque para UFs e linear fits
ggplotly(
  ggplot(saeb_rend, aes(x = rendimento, y = saeb, color = uf, shape = uf)) +
    geom_point(size = 1) +
    geom_smooth(method = "lm", formula = y ~ x, se = F) +
    xlab("rendimento") +
    ylab("saeb") +
    scale_colour_viridis_d() +
    theme_classic()
)

ggplot(saeb_rend, aes(x = rendimento, y = saeb, color = uf, shape = uf)) +
  geom_point(size = 1) +
  geom_smooth(method = "lm", formula = y ~ x, se = F) +
  xlab("rendimento") +
  ylab("saeb") +
  scale_colour_viridis_d() +
  theme_classic()

#     #     #     #       #      #      #     #       #       #




# x -----------------------------------------------------------------------
# x -----------------------------------------------------------------------
# Regressão Não-Linear Multipla com Dummies -------------------------------

# Dados
load(file = "raw_datas_exemplos/planosaude.RData")

# Exploratorio
glimpse(planosaude)
summary(planosaude)
levels(factor(planosaude$plano))

# Acertar tipos de variaveis
planosaude$id <- as.character(planosaude$id) 
planosaude$plano <- as.factor(planosaude$plano)


# Tabela de frequências absolutas da variável 'plano'
table(planosaude$plano)


# Correlações - somente variáveis quantitativas
chart.Correlation((planosaude[2:5]), histogram = TRUE)


# Dummies -----------------------------------------------------------------
# Transformar a variavel qualitativa em dummie. Salvar nova base de dados
# para não interferir na base de dados original
planosaude_dummies <- dummy_columns(.data = planosaude,
                                    select_columns = "plano",
                                    remove_selected_columns = T,
                                    remove_most_frequent_dummy = T)
# Visualizando a base de dados dummizada
planosaude_dummies %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 23)


# Estimar Modelo Linear ---------------------------------------------------

# Modelagem com todas as variáveis
modelo_planosaude <- lm(despmed ~ . - id, planosaude_dummies)

# Parâmetros do modelo_planosaude
summary(modelo_planosaude)


# Stepwise ----------------------------------------------------------------
# antes de interpretar, vamos tirar as variáveis que não passaram
step_planosaude <- step(modelo_planosaude, k = 3.841459)
# ou
step_planosaude <- step(modelo_planosaude, k = qchisq(p = 0.05, df = 1, lower.tail = F))

summary(step_planosaude)



# Teste dos Pressupostos --------------------------------------------------



# Testar: ADERÊNCIA DOS RESÍDUOS À NORMALIDADE ----------------------------
# Lembrando que se tiver missing values, a extração do resíduo não vai rodar e teriamos
# que criar um predict com o modelo para ele aceitar o missing value e retornar os resíduos
# object = modelo de regressao
# newdata= dados usados para gerar o modelo
# planosaude_dummies$yhat <- predict( object = step_planosaude (=modelo), newdata= planosaude_dummies)
# planosaude_dummies$resid <- planosaude_dummies$despmed - planosaude_dummies$yhat   (=obs - esp)

# Teste de Shapiro-Francia
# p<0.05 = não existe normalidade
nortest::sf.test(step_planosaude$residuals)


# Plotando os resíduos do modelo step_planosaude 
planosaude %>%
  mutate(residuos = step_planosaude$residuals) %>%
  ggplot(aes(x = residuos)) +
  geom_histogram(color = "white", 
                 fill = "#55C667FF", 
                 bins = 15,
                 alpha = 0.6) +
  labs(x = "Resíduos",
       y = "Frequências") + 
  theme_bw()

# Acrescentando uma curva normal teórica para comparação entre as distribuições
planosaude %>%
  mutate(residuos = step_planosaude$residuals) %>%
  ggplot(aes(x = residuos)) +
  geom_histogram(aes(y = ..density..), 
                 color = "white", 
                 fill = "#55C667FF", 
                 bins = 15,
                 alpha = 0.6) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(step_planosaude$residuals),
                            sd = sd(step_planosaude$residuals)),
                size = 2, color = "grey30") +
  scale_color_manual(values = "grey50") +
  labs(x = "Resíduos",
       y = "Frequência") +
  theme_bw()

# Conclusão: este histograma mostra que não houve aderencia dos valores de 
# resíduo a normalidade. A curva normal teórica é signif. diferente das barras que 
# representam os resíduos.


# Kernel density estimation (KDE) - forma não-paramétrica para estimar a
# função densidade de probabilidade de uma variável aleatória
planosaude_dummies %>%
  ggplot() +
  geom_density(aes(x = step_planosaude$residuals), fill = "#55C667FF") +
  labs(x = "Resíduos do Modelo Stepwise",
       y = "Densidade") +
  theme_bw()


# Testar: HETEROCEDASTICIDADE ---------------------------------------------

# Teste de Breusch-Pagan para diagnóstico de heterocedasticidade
olsrr::ols_test_breusch_pagan(step_planosaude)

# Conclusão: p<0.05, não passa. Os dados são heterocedasticos, ou seja, pode
# ter ocorrido omissão de alguma variável explicativa relevante e/ou forma
# inadequada do modelo (o linear não seria o adequado).


# Adicionando fitted values e resíduos do modelo 'step_planosaude'
# no dataset 'planosaude_dummies'
planosaude_dummies$fitted_step <- step_planosaude$fitted.values
planosaude_dummies$residuos_step <- step_planosaude$residuals

# Gráfico que relaciona resíduos e fitted values do modelo 'step_planosaude'
planosaude_dummies %>%
  ggplot() +
  geom_point(aes(x = fitted_step, y = residuos_step),
             color = "#55C667FF", size = 3) +
  labs(x = "Fitted Values do Modelo Stepwise",
       y = "Resíduos do Modelo Stepwise") +
  theme_bw()


# Transformação de BoxCox -------------------------------------------------

#Para calcular o lambda de Box-Cox
lambda_BC <- powerTransform(planosaude$despmed)
lambda_BC

# Inserindo o lambda de Box-Cox na nova base de dados para a estimação de um
# novo modelo
planosaude_dummies$bcdespmed <- (((planosaude$despmed ^ lambda_BC$lambda) - 1) / 
                                   lambda_BC$lambda)

# Visualizando a nova variável na base de dados
planosaude_dummies %>%
  select(id, despmed, bcdespmed, everything()) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 14)

# Estimando um novo modelo múltiplo com dummies
modelo_bc_planosaude <- lm(formula = bcdespmed ~ . -id -despmed -fitted_step
                           -residuos_step, 
                           data = planosaude_dummies)

# Parâmetros do modelo
summary(modelo_bc_planosaude)

# Aplicando o procedimento Stepwise
step_bc_planosaude <- step(modelo_bc_planosaude, k = qchisq(p = 0.05, df = 1, lower.tail = F))
summary(step_bc_planosaude)

# Verificando a normalidade dos resíduos do modelo step_bc_planosaude
# Teste de Shapiro-Francia
nortest::sf.test(step_bc_planosaude$residuals) # normal

# Plotando os novos resíduos do modelo step_bc_planosaude com curva normal teórica
planosaude_dummies %>%
  mutate(residuos = step_bc_planosaude$residuals) %>%
  ggplot(aes(x = residuos)) +
  geom_histogram(aes(y = ..density..), 
                 color = "white", 
                 fill = "#440154FF", 
                 bins = 15,
                 alpha = 0.6) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(step_bc_planosaude$residuals),
                            sd = sd(step_bc_planosaude$residuals)),
                size = 2, color = "grey30") +
  scale_color_manual(values = "grey50") +
  labs(x = "Resíduos",
       y = "Frequência") +
  theme_bw()

# Kernel density estimation (KDE)
planosaude_dummies %>%
  ggplot() +
  geom_density(aes(x = step_bc_planosaude$residuals), fill = "#440154FF") +
  labs(x = "Resíduos do Modelo Stepwise com Transformação de Box-Cox",
       y = "Densidade") +
  theme_bw()

# Diagnóstico de Heterocedasticidade para o Modelo Stepwise com Box-Cox
ols_test_breusch_pagan(step_bc_planosaude) # não heterocedástico

# Adicionando fitted values e resíduos do modelo 'step_bc_planosaude'
# no dataset 'planosaude_dummies'
planosaude_dummies$fitted_step_novo <- step_bc_planosaude$fitted.values
planosaude_dummies$residuos_step_novo <- step_bc_planosaude$residuals

# Gráfico que relaciona resíduos e fitted values do modelo 'step_bc_planosaude'
planosaude_dummies %>%
  ggplot() +
  geom_point(aes(x = fitted_step_novo, y = residuos_step_novo),
             color = "#440154FF", size = 3) +
  labs(x = "Fitted Values do Modelo Stepwise com Transformação de Box-Cox",
       y = "Resíduos do Modelo Stepwise com Transformação de Box-Cox") +
  theme_bw()


