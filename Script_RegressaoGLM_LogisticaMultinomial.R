
# Natália Freitas
# Modelo Logistico Multinomial



# Contextualização --------------------------------------------------------
# Olhar todo o script 2023_RegressaoLogistica


# Objetivo:  Tecnica supervisionada de Machine Learning utilizada para explicar
# ou predizer a probabilidade de ocorrência de determinado evento em função
# de uma ou mais variáveis explicativas. 

# Variável dependente: possui mais de uma categoria. O que difere esta regressão
# da regressão logistica binomial (na qual a variável dependente tem apenas duas
# categorias)

# No modelo logistico multinomial como temos mais de 2 categorias (1 categoria
# de referencia e as demais categorias alternativas). Vamos precisar de mais 
# de um logito (1 para cada categoria) para conseguir capturar o comportamento
# da variável.

# Interpretação:
# - Como na regressão logística binári, deve-se avaliar o resultado do teste
#   qui-quadrado para o modelo de regressão logística multinomial, bem como
#   os resultados dos testes z para os parâmetros estimados das variáveis 
#   preditoras.
# - Os parâmetros das variáveis devem ser analisados em relação à categoria de
#   de referencia da variável dependente
# - Eficiencia do modelo: a classificação das observações deve ser realizada a
#   partir da maior probabilidade estimada para cada observação (aqui, ao 
#   contrário da regressão logística binária, não faz sentido a definição de
#   um cutoff).


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


# Dados -------------------------------------------------------------------
load(file = "raw_datas_exemplos/AtrasadoMultinomial.RData")

# Exploratorio ------------------------------------------------------------

# Checando variáveis
glimpse(AtrasadoMultinomial)

# Visualizaçao
AtrasadoMultinomial %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 22)

#Estatísticas descritivas univariadas da base de dados
summary(AtrasadoMultinomial)

# Definir a categoria de referencia ---------------------------------------
# Se não definir a categria de referencia, ele vai pegar por ordem alfabetica
AtrasadoMultinomial$atrasado <- relevel(AtrasadoMultinomial$atrasado, 
                                        ref = "nao chegou atrasado")


# Estimar Modelo Logistico Multinomial ------------------------------------
# A função da logistica multinomial muda comparado aos demais glm. 
modelo_atrasado <- nnet::multinom(formula = atrasado ~ dist + sem, 
                            data = AtrasadoMultinomial)


# Outputs -----------------------------------------------------------------
summary(modelo_atrasado)


# Coefficients:
#                                  (Intercept)      dist      sem
# chegou atrasado à primeira aula   -33.06853 0.5574586 1.666924    # parametros primeiro logito
# chegou atrasado à segunda aula    -62.21623 1.0766952 2.891689    # parametros segundo logito
# Residual Deviance: 49.02364 
# AIC: 61.02364

# Probabilidade de náo chegar atrasado



# Output: opção 2 ---------------------------------------------------------
# Outra maneira de apresentar os outputs do modelo
# Mais adequado do que a função export_summs
stargazer::stargazer (modelo_atrasado, nobs=T, type="text")


# LogLik  ---------------------------------------------------
# LL do modelo_atrasado
# É o valor que maximiza os acertos do modelo
# Quanto maior o valor, melhor o modelo
logLik(modelo_atrasado)



# Função equivalente ao jtools::sum ---------------------------------------
# função para ver se pelo menos 1 beta é diferente de 1.
# Como a função summ do pacote jtools não funciona para objetos de classe
# 'multinom'. 
# Logo, vamos definir uma função Qui2 (equivale ao teste F) para se extrair a 
# estatística geral do modelo:
Qui2 <- function(x) {
  maximo <- logLik(x)
  minimo <- logLik(update(x, ~1, trace = F))
  Qui.Quadrado <- -2*(minimo - maximo)
  pvalue <- pchisq(Qui.Quadrado, df = 1, lower.tail = F)
  df <- data.frame()
  df <- cbind.data.frame(Qui.Quadrado, pvalue)
  return(df)
}

# Estatística geral do modelo_atrasado
# p<0.05 - existe pelo menos um beta com valor significativo
Qui2(modelo_atrasado)


# Na Regressão Logística Multinomial, o R quebra a lógica de relatórios que, 
# normalmente, oferece para os GLM. Também é preciso notar que a linguagem 
# básica  não consegue rodar esse tipo de regressão, sendo necessário o pacote
# nnet. Além do mais, não são fornecidas as estatísticas z de Wald, nem os
# p-values das variáveis da modelagem.

# Explicando a lógica do R para a Logística Multinomial:

# 1 - Foram estabelecidas *labels* para as categorias da variável dependente: 
# 'nao chegou atrasado', 'chegou atrasado à primeira aula' e 'chegou atrasado à
# segunda aula';

# 2 - Foi comandado que a categoria de referência seria a categoria 'nao chegou
# atrasado', e isso explica o porquê dela não aparecer no relatório gerado;

# 3 - O relatório é dividido em duas partes: 'Coefficients' e 'Std. Errors'. 
# Cada linha da seção 'Coefficients' informa um logito para cada categoria da
# variável dependente, com exceção da categoria de referência. Já a seção 
# 'Std. Errors' informa o erro-padrão de cada parâmetro em cada logito.

# Para calcular as estatísticas z de Wald, há que se dividir os valores da 
# seção 'Coefficients' pelos valores da seção 'Std. Errors.' Assim, temos que:  
# Assim teremos a estatistica z:
zWald_modelo_atrasado <- (summary(modelo_atrasado)$coefficients / 
                            summary(modelo_atrasado)$standard.errors)

zWald_modelo_atrasado

# Porém, ainda faltam os respectivos p-values. Assim, os valores das probabilidades 
# associadas às abscissas de uma distribuição normal-padrão é dada pela função
# pnorm(), considerando os valores em módulo - abs(). Após isso, multiplicamos 
# por dois os valores obtidos para considerar os dois lados da distribuição
# normal padronizada (distribuição bicaudal). Desta forma, temos que os p-valores sao:
round((pnorm(abs(zWald_modelo_atrasado), lower.tail = F) * 2), 4)



# Predição ----------------------------------------------------------------
# Fazendo predições para o modelo_atrasado. Exemplo: qual a probabilidade média
# de atraso para cada categoria da variável dependente, se o indivíduo tiver 
# que percorrer 22km e passar por 12 semáforos?
predict(modelo_atrasado, 
        data.frame(dist = 22, sem = 12), 
        type = "probs")    # type= probs: calculas as probabilidades
# nao chegou atrasado chegou atrasado à primeira aula  chegou atrasado à segunda aula 
# 0.68005964                      0.30515681                      0.01478355 

# Como a maior probabilidade foi de não chegou atrasado. A
# classificação dessa observação é não chegou atrasado

predict(modelo_atrasado, 
        data.frame(dist = 22, sem = 12), 
        type = "class") # type= class: já faz a classificação 
# [1] nao chegou atrasado
# Levels: nao chegou atrasado chegou atrasado à primeira aula chegou atrasado à segunda aula



# Efetividade Geral do Modelo ---------------------------------------------------

# Adicionando as prováveis ocorrências de evento apontadas pela modelagem à 
# base de dados. Sem definir cutoffs
AtrasadoMultinomial$predicao <- predict(modelo_atrasado, 
                                        newdata = AtrasadoMultinomial, 
                                        type = "class")

# Visualizando a nova base de dados AtrasadoMultinomial com a variável 'predicao'
AtrasadoMultinomial %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 22)

attach(AtrasadoMultinomial)

# Dataframe da eficiencia global do modelo
# Criando uma tabela para comparar as ocorrências reais com as predições
EGM <- as.data.frame.matrix(table(predicao, atrasado))

# Visualizando a tabela EGM
EGM %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 22)

# Eficiência global do modelo: Acurácia Total do Modelo
# somatorio da diagonal principal/ somatorio total da tabela
acuracia <- (round((sum(diag(table(atrasado, predicao))) /
                      sum(table(atrasado, predicao))), 2))

acuracia


# Plotar as Probabilidades ------------------------------------------------
# Adicionando à base de dados as probabilidades em razão de cada categoria:
levels(AtrasadoMultinomial$atrasado)

AtrasadoMultinomial[c("nao chegou atrasado",
                      "chegou atrasado à primeira aula",
                      "chegou atrasado à segunda aula")] <- modelo_atrasado$fitted.values

# Plotagem das smooth probability lines para a variável 'dist'
ggplotly(
  AtrasadoMultinomial %>% 
    dplyr::select(-predicao, - estudante) %>% 
    rename(y = 1) %>% 
    melt(id.vars = c("y","dist","sem"),
         value.name = "probabilidades") %>% 
    rename(categorias = variable) %>%
    mutate(categorias = factor(categorias,
                               levels = c("nao chegou atrasado",
                                          "chegou atrasado à primeira aula",
                                          "chegou atrasado à segunda aula"))) %>% 
    ggplot() +
    geom_smooth(aes(x = dist, y = probabilidades, color = categorias), 
                method = "loess", formula = y ~ x, se = T) +
    labs(x = "Distância Percorrida",
         y = "Probabilidades",
         color = "Legenda:") +
    scale_color_viridis_d() +
    theme_bw()
)

# Plotagem das smooth probability lines para a variável 'sem'
ggplotly(
  AtrasadoMultinomial %>% 
    dplyr::select(-predicao, - estudante) %>% 
    rename(y = 1) %>% 
    melt(id.vars = c("y","dist","sem"),
         value.name = "probabilidades") %>% 
    rename(categorias = variable) %>%
    mutate(categorias = factor(categorias,
                               levels = c("nao chegou atrasado",
                                          "chegou atrasado à primeira aula",
                                          "chegou atrasado à segunda aula"))) %>% 
    ggplot() +
    geom_smooth(aes(x = sem, y = probabilidades, color = categorias), 
                method = "loess", formula = y ~ x, se = T) +
    labs(x = "Semáforos no Percurso",
         y = "Probabilidades",
         color = "Legenda:") +
    scale_color_viridis_d() +
    theme_bw()
)

# Plotagem tridimensional para cada probabilidade de ocorrência de cada
# categoria da variável dependente

AtrasadoMultinomial$p0 <- AtrasadoMultinomial$`nao chegou atrasado`
AtrasadoMultinomial$p1 <- AtrasadoMultinomial$`chegou atrasado à primeira aula`
AtrasadoMultinomial$p2 <- AtrasadoMultinomial$`chegou atrasado à segunda aula`


# p0 - Probabilidades de não chegar atrasado (função scatter3d do pacote car):
scatter3d(AtrasadoMultinomial$dist,AtrasadoMultinomial$p0,
          AtrasadoMultinomial$sem,
          data = AtrasadoMultinomial,
          fit = "smooth")

# Outro modo:
plot_ly(x = AtrasadoMultinomial$dist, 
        y = AtrasadoMultinomial$sem, 
        z = AtrasadoMultinomial$`nao chegou atrasado`,
        type = "mesh3d",
        name = "ótimo",
        intensity = AtrasadoMultinomial$`nao chegou atrasado`,
        colors = colorRamp(c("red","yellow","chartreuse3","lightblue","blue"))) %>% 
  layout(showlegend = T,
         scene = list(
           xaxis = list(title = "Distância"),
           yaxis = list(title = "Semáforos"),
           zaxis = list(title = "Probabilidade")),
         title = "Categoria nao chegou Atrasado")


#p1 - Probabilidades de chegar atrasado à primeira aula:
scatter3d(AtrasadoMultinomial$dist,AtrasadoMultinomial$p1,
          AtrasadoMultinomial$sem,
          data = AtrasadoMultinomial,
          fit = "smooth")

#Outro modo:
plot_ly(x = AtrasadoMultinomial$dist, 
        y = AtrasadoMultinomial$sem, 
        z = AtrasadoMultinomial$`chegou atrasado à primeira aula`,
        type = "mesh3d",
        name = "ótimo",
        intensity = AtrasadoMultinomial$`chegou atrasado à primeira aula`,
        colors = colorRamp(c("red", "yellow", "chartreuse3", "lightblue", "blue"))) %>% 
  layout(showlegend = T,
         scene = list(
           xaxis = list(title = "Distância"),
           yaxis = list(title = "Semáforos"),
           zaxis = list(title = "Probabilidade")),
         title = "Categoria Chegou Atrasado à Primeira Aula")


#p2 - Probabilidades de chegar atrasado à segunda aula:
scatter3d(AtrasadoMultinomial$dist,AtrasadoMultinomial$p2,
          AtrasadoMultinomial$sem,
          data = AtrasadoMultinomial,
          fit = "smooth")

#Outro modo:
plot_ly(x = AtrasadoMultinomial$dist, 
        y = AtrasadoMultinomial$sem, 
        z = AtrasadoMultinomial$`chegou atrasado à segunda aula`,
        type = "mesh3d",
        name = "ótimo",
        intensity = AtrasadoMultinomial$`chegou atrasado à segunda aula`,
        colors = colorRamp(c("red", "yellow", "chartreuse3", "lightblue", "blue"))) %>% 
  layout(showlegend = T,
         scene = list(
           xaxis = list(title = "Distância"),
           yaxis = list(title = "Semáforos"),
           zaxis = list(title = "Probabilidade")),
         title = "Categoria Chegou Atrasado à Segunda Aula")


#Visualização das sigmóides tridimensionais em um único gráfico:
naoatrasado <- plot_ly(x = AtrasadoMultinomial$dist, 
                       y = AtrasadoMultinomial$sem, 
                       z = AtrasadoMultinomial$`nao chegou atrasado`,
                       type = "mesh3d",
                       name = "nao chegou atrasado") %>%
  layout(showlegend = T,
         scene = list(
           xaxis = list(title = "Distância"),
           yaxis = list(title = "Semáforos"),
           zaxis = list(title = "Probabilidade")))

atrasadoprimeira <- plot_ly(x = AtrasadoMultinomial$dist, 
                            y = AtrasadoMultinomial$sem, 
                            z = AtrasadoMultinomial$`chegou atrasado à primeira aula`,
                            type = "mesh3d",
                            name = "chegou atrasado à primeira aula") %>%
  layout(showlegend = T,
         scene = list(
           xaxis = list(title = "Distância"),
           yaxis = list(title = "Semáforos"),
           zaxis = list(title = "Probabilidade")))

atrasadosegunda <- plot_ly(x = AtrasadoMultinomial$dist,
                           y = AtrasadoMultinomial$sem,
                           z = AtrasadoMultinomial$`chegou atrasado à segunda aula`,
                           type = "mesh3d",
                           name = "chegou atrasado à segunda aula") %>%
  layout(showlegend = T,
         scene = list(
           xaxis = list(title = "Distância"),
           yaxis = list(title = "Semáforos"),
           zaxis = list(title = "Probabilidade")))

subplot(naoatrasado, atrasadoprimeira, atrasadosegunda)


######################################FIM#####################################