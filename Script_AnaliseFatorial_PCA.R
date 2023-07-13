
# PCA - Análise de Componentes Principais
# novembro 2022
# Natália Freitas


# Quando aplicar a análise fatorial?
# Diferente do cluster que agrupa as observações (linhas). As análises
# fatoriais agrupam as variáveis (coluna). Trata-se de uma grupamento
# das variáveis em fatores (por isso o nome análise fatorial). 
# objetivo:
# - Obter o comportamento do conjunto de variáveis, combinando-as para 
#   a redução estrutural;
# - Elaboração de rankings para classificação de desempenho por meio dos
#   fatores;
# - Criação de fatores ortogonais entre eles e posterior uso em modelos
#   supervisionado;

# Destinado APENAS para variáveis MÉTRICAS, quantitativas.Se tiver 
# variáveis categóricas usaremos Análise de correspondencia.Porque
# precisamos usar matrizes de correlação para a análise (correlação
# de pearson) por isso só podemos fazer em variáveis quantitativas.



# PCA: Análise de Componentes Principais é um algoritmo de redução
# de dimensionalidade, embora seus usos possam ser dos mais variados.

# Objetivo: Redução de Dimensionalidade. Utilizado quando existe 
#           um número muito grande de colunas nos dados. A PCA quer 
#           encontrar uma matriz Y com k linhas e d colunas. Onde d é
#           menor do que k, que capte características importantes de X.

# O processo de encontrar Y consiste em usar essa matriz para tentar 
# "reconstruir" X. Se a reconstrção for bem-sucedida (tiver erro baixo)
# então podemos usar Y ao invés de X para várias coisas
# como modelagem, visualização, interpretação, etc.

# A PCA usa um processo de linearização para reduzir a dimensionalidade e 
# achar o Y e A que melhor estima o X. (X = Y * A + mu). Ou seja, a 
# a apromiximação busca icognitas para plugar em uma equação que faz soma
# e multiplicação. Isso é bom em varios contexto. Mas as vezes não funciona.
# Alternativa para isso são os métodos Stochastic Neighbor Embedding (SNE)
# (Olhar no Script SNE)

# Pressupostos ------------------------------------------------------------
# As análises multivariadas assumem que todas as variáveis possuem distribuição 
# normal multivariada (similar a distribuição normal gaussiana), mas são 
# robustas a dados não normais. Então a PCA funciona com dados não normais,
# mas pode ficar ainda melhor com dados normais.


# Adequação de Fatores ----------------------------------------------------
# Para determinar se a análise fatorial é adequada, podemos investigar a 
# adequação global da análise fatorial através do teste de esfericidade de
# Bartlett (Ho:matriz de correlação é igual a matriz identidade). Para
# formar fatores precisamos de correlações diferentes de zero, logo rejeitar
# Ho.


# Matriz de Correlação ----------------------------------------------------
# A PCA fundamentada na existência de correlação entre variáveis originais
# para a criaão dos fatores (eixos).
# o procedimento consiste em realizar uma matriz de correlação paras
# as k variáveis e calcular o coeficiente de correlação de pearson. Quando
# os coeficientes são valores extremos (+1 -1) propiciam a extração de um
# único fator, ou seja, indica existênci de realção entre as variáveis. Já
# os coeficientes mais próximos de zero propiciam a extração de diferentes
# fatores, ou seja, indica q a relação entre as variáveis é quase inexistente


# Definição ---------------------------------------------------------------
#> Autovalor (eigenvalue): valor que indica o quanto o eixo (fator) explica da 
#>                         variância dos dados. Cada eixo possui 
#>                         um autovalor.quantidade de autovalor é em função
#>                         da quantidade de variáveis na amostra.

#> Autovetor: Cada variável ambiental possui um autovetor (setas) que diz
#>            respeito o quanto a variável contribui para cada eixo. 
#>            Pode ser obtido através dos Loadings.

#> Scores: são coordenadas que determinam a posição dos pontos no plano do
#>         gráfico. Esses scores estão em desvio padrão. Logo um valor 
#>         de 3,4 indica que o ponto está 3,4 desvios padrão da média.
#>         Os scores são criadas a partir dos autovalores e autovetores da
#>         matriz de correlação.
#>
#> Fatores (Eixos): o valor dos fatores são definidos com as variáveis 
#>                  tranformadas pelo z-score. Tais fatores são ortogonais 
#>                  entre si. Ou seja, não são correlacionadas, são indepen
#>                  dentes.Nem todos os fatores serão utilizados, faremos
#>                  seleção dos eixos (fatores)

# Loadings: consiste na correlação entre os fatores (eixos) e cada uma das
#           variáveis. e neste caso, conseguimos usar essa informação para 
#           saber quais variáveis estão contidas na análise. Essas cargas 
#           fatoriais nos dá as coordenadas dos pontos

# Etapas ------------------------------------------------------------------

# Passo 1: Análise descritiva das variáveis 

# Passo 2: Verificar normalidade das variáveis

# Passo 3: Normalizar as variáveis que não possuem distribuição normal (tranformação log ou raiz quadrada)

# Passo 4: Padronizar as variáveis (Z-scores): valor-média/desvio padrão
#          Neste caso, faremos todas as variáveis variar dentro da mesma faixa
#          (compensar o fato de que as variáveis possuem unidades diferentes).
#          Então, retiramos as unidades de todas as variáveis e transformamos
#          os valores para desvios padrões (o quanto aquela observação se
#          distância da normal em termos de desvios padrão).

#> Obs: A qualidade da PCA será avaliada através do %de variância explicada 


# Quantidade de Fatores ---------------------------------------------------
# O critério de Kaiser (ou critério da raiz latente) pode ser utilizada para
# a escolha da quantidade de fatores (eixos) a serem utizados. A anaálise
# indica que seja utilizada apenas fatores correspondentes a autovalores>1




# Referencias -------------------------------------------------------------
# Site para olhar depois; usa predict ( ) com a PCA
# http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/118-principal-component-analysis-in-r-prcomp-vs-princomp/#:~:text=The%20function%20princomp()%20uses,preferred%20compared%20to%20princomp().



# Pacotes -----------------------------------------------------------------
library(tidyverse)
library(ggfortify)


# Dados -------------------------------------------------------------------
# sortear dados para teste

set.seed(01092022)

Xx <- rnorm(1000, sd =1)
hist(Xx)
X <- tibble(
  Xx,
  Y=Xx +rnorm(length(Xx), sd = .5))



# Funcionamento da PCA ----------------------------------------------------
# Analise da PCA utiliza a seguinte fórmula para reduzir a dimensionalidade 
# dos dados:    x´= YA + Mi

# xxxxxxxxxxxxxxxxxxx
# Função para Análise
# princomp( )
PCA <- princomp(X)
PCA

# xxxxxxxxxxxxxxxxxxx
# Resultado: 
# center: é o Mi da fórmula. É o centróide dos pontos
# scores: é o Y da fórmula
# loadings: é o A da fórmula. Fornece as coordenadas da ponta da seta.
mu <- PCA$center
Y <-  PCA$scores
A <-  PCA$loadings


# xxxxxxxxxxxxxxxxxxx
# Vendo a PCA trabalhar

# Multiplicando Matrizes:
#> Y e A aproximam X
X_aprox <- set_names(as_tibble(Y %*% A + mu), c("X", "Y"))

X_aprox[1,]
X[1,]

X_aprox[2,]
X[2,]

X_aprox[3,]
X[3,]

#> O que é a matriz A? 
#> Fornece as coordenadas da ponta da seta.
#> A[,1] do eixo 1
#> A[,2] do eixo 2

plot(X)
points(matrix(mu, nrow = 1), col = "blue", lwd = 10)
points(
  matrix(A[,1], nrow = 1),
  col = 'red', lwd = 10
)
points(
  matrix(A[,2], nrow = 1),
  col = 'green', lwd = 10
)
lines(
  rbind(
    mu,
    A[,1]
  ), col = 'red', lwd = 10
)
lines(
  rbind(
    mu,
    A[,2]
  ), col = 'green', lwd = 10
)

# Aproximação de dimensão menor
# reduzindo as dimensões
Y_1d = matrix(Y[,1], ncol = 1)

X_aprox_1d = as_tibble(Y_1d %*% A[1,] + mu) |>
  set_names(c("X", "Y"))

X[1,]
X_aprox_1d[1,]

plot(X)
points(matrix(mu, nrow = 1), col = "blue", lwd = 10)
points(
  matrix(A[,1], nrow = 1),
  col = 'red', lwd = 10
)
points(
  matrix(A[,2], nrow = 1),
  col = 'green', lwd = 10
)
lines(
  rbind(
    mu,
    A[,1]
  ), col = 'red', lwd = 10
)
lines(
  rbind(
    mu,
    A[,2]
  ), col = 'green', lwd = 10
)
points(X_aprox_1d, col = "red", lwd = 10)


# xxxxxxxxxxxxxxxxxxx
# Erro 
# A PCA está minimizando o erro; Encontrando o Y e A que melhor se aproxiam
# do X.

sqrt(sum((X-X_aprox_1d)^2)/(2000))

# O PCA produz uma matriz de variância
# Variância do X:
  var(X)
# Variância aproximada do X, feita pela PCA:
  var(X_aprox_1d)

# Podemos verificar o quanto a variância da aproximação do PCA é parecida
# com a variância real do X. Essa métrica a ser utilizada para saber se está
# bom.
# 1. Pegamos a soma das diagonais
# var(x)-> 0.95 + 1.19 para variância total
# var(X_aprox_1d) -> 0.88 + 1.146773
# 2. Dividimos uma pela outra. Quanto mais próximo de 1. Melhor a aproxição 
(0.8873892+1.146773)/(0.9546543+1.1988115)

# Mais automatizado:pode montar função com essa fórmula  
(var(X_aprox_1d[,1]) + var(X_aprox_1d[,2]))/(var(X[,1]) + var(X[,2]))
# 0.9445993
# Significa que a aproximação é razoável. 

# Mostra a variância associada a cada componente
# Neste caso, quase toda a variância é captada pelo primeiro componente.
# e um pouco pelo segundo
plot(PCA)
PCA$sdev

#> CONCLUSÃO: Podemos concluir que o Y é uma boa aproximação do X. Então, 
#> podemos usar o Y para reduzir a dimensão do X. Pois ele está captando
#> a variãncia que estava contida no X. Embora perca um pouco da variância
#> já que a divisão (métrica) deu 0.94, não deu 1. Mas ainda assim, capta 
#> boa parte da variância. 






# Analise PCA -------------------------------------------------------------
# Pode ser realizada utilizando:

#> princomp( ):
#> cor = TRUE: Se True, os dados serão centered and scaled antes da análise.

#> prcomp ( ): parece ser preferível em relação ao princomp.
#> scale. = TRUE: Se True, os dados serão padronizados antes da análise.

 



# xxxxxxxxxxxxxxxx
# EXEMPLO 1 xxxxxxxx

# Analise no Iris
# Dados
iris_sem_species <- iris[,-5]
# Análise:
PCA_iris <- princomp(iris_sem_species)
PCA_iris
summary(PCA_iris)
biplot(PCA_iris,scale = T)




# Resultado: Gráficos -----------------------------------------------------

#> plot ( ): mostra as amostras no espaço de ordenação, dos componentes 1 e 2.
#           Usamos esses por serem o que mais explicam e pq não conseguimos ver
#           mais do que duas dimensões
plot(PCA_iris$scores[,1:2])

#> biplot ( ): mostra as amostras indivíduais (scores) junto com os vetores que 
#             corresponde aos loadings. Neste gráfico podemos ver o quanto o 
#             aumento em uma variável influencia onde as amostras estão no espaço
biplot(PCA_iris)

#> library(factoextra): podemos reproduzir e ajeitar as coisas com o ggplot
# element = "ind" : plotar as amostras.
# element = "var" : plotar o autovetor (setas) que mostra o quanto cada 
#                   variável contribui para a formação do eixo.
factoextra::fviz(PCA_iris, element = "ind")
factoextra::fviz(PCA_iris, element = "var")


#> barplot(): mostrar a importancia dos componentes
princomp(PCA_iris$scores) |>
  plot()


#> Gráfico Manual:
# scaling variable: controla o tamanho do vetor
# textNudge variable: controla aonde os labels aparecerão

#quadro novo
dev.new(height=7, width=7)     

#eixos
plot(PCA_iris$scores[, 1],
     PCA_iris$scores[, 2],
     xlab='PCA 1', ylab='PCA 2',
     type='p', asp=1, las=1)

#especificações
scaling <- 3
textNudge <- 1.1

#setas
arrows(0, 0, PCA_iris$loadings[, 1]* scaling,
       PCA_iris$loadings[, 2]* scaling,
       length=0.1, angle=20, col='red')

#texto:variáveis
text(PCA_iris$loadings[, 1]*scaling*textNudge,
     PCA_iris$loadings[, 2]*scaling*textNudge,
     rownames(PCA_iris$loadings),
     col='red', cex=0.7)

#amostra
text(PCA_iris$scores[, 1],
     PCA_iris$scores[, 2],
     rownames(PCA_iris$scores),
     col='blue', cex=0.7)

dev.off()

# Visualizar eigenvalues: explicação dos componentes
fviz_eig(PCA_iris)

# GRÁFICOS COM fviz
# Biplot
fviz_pca_biplot(PCA_iris2, repel = TRUE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969"  # Individuals color
)

# Amostras
PCA_iris2 <- prcomp(iris_sem_species, scale = TRUE)
fviz_pca_ind(PCA_iris2,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

# Variaveis
fviz_pca_var(PCA_iris2,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)



# Resultado: Interpretação -----------------------------------------------

# xxxxxxxxxxXXXXXXXXXXXXXXX
# Importance of components: dá a importância de cada eixo (eigenvalue ou
#                           autovalor). Que é uma medida de variância. Dá
#                           a proporção de variabilidade explicada por cada
#                           eixo (Proportion of Variance Explained). E dá a
#                           proporção cumulativa ao acrescentar a explicação
#                           do próximo eixo. 
summary(PCA_iris)



# xxxxxxxxxxXXXXXXXXX
# Standard deviation: medidade de cada componente.
PCA_iris$sdev



# xxxxxxxxx
#> Loadings: medida utilizada para cada variável. 
#>          -> Informa como as variáveis contribuem para cada componente.
#>             Loadings positivos indicam que a variável e o componente
#>             estão correlacionados positivamente.Enquanto valores 
#>             negativos, indicam correlação negativa.
#>          -> Valores altos (positivo ou negativo) indicam que a variável
#>             tem efeito forte no componente principal.Precisamos de
#>             critérios para definir o que são valores altos de loading. A
#>             soma dos loadings ao quadrado para cada componente dará 1.
#>             Podemos calcular qual seria a contribuição se cada variável 
#>             contribuisse igualmente para usar como base de comparação.
#>             Esse valor aparece no resultado como "Proportion Var".
#>          -> Sempre reportado nos resultados os loadings (contribuição de 
#>             cada variável) para o componente.
PCA_iris$loadings



# xxxxxxxxx
#> scores: Esses valores informam o posicionamento das amostras no espaço de
#>         ordenação criado.
#>         -> Podem ser usados para serem plotados como scatterplot
#>            (Componente 1 vs Componente 2) mostrando os eixos mais 
#>            explicativos.
#>         -> Podem ser usados como variáveis independentes dentro de um 
#>            modelo supervisionado ou para correlações com índices.
PCA_iris$scores


# xxxxxxxxx
# center: 
PCA_iris$center

# xxxxxxxxx
# scale: 
PCA_iris$scale

# xxxxxxxxx
# n.obs: número de observações 
PCA_iris$n.obs

# xxxxxxxxx
# call: fórmula utilizada para a análise
PCA_iris$call

# xxxxxxxxx
# eigenvalues: (autovalores):A partir dos autovalores conseguimos ver o % de 
#                            variância compartilhada das variáveis originais em
#                            cada fator. O primeiro fator (componente) explica 
#                            maior percentual da variancia e depois vai diminuindo
PCA_iris$values
PCA_iris$Vaccounted # Identificação da variância compartilhada em cada fator



# Resultado: Critérios -----------------------------------------------


# 1 . Quantos eixos vamos utilizar?
#     Devemos olhar os componentes principais e excluir aqueles que explicam
#     pouca variância dos dados. Em geral, ficamos apenas com o primeiro ou o
#     primeiro e segundo
      summary(PCA_iris)

# 2. Reportar os Loadings que demonstra a contribuição da cada variável para 
#    cada um dos componentes selecionados.
     PCA_iris$loadings

# 3. Visualizar a distribuição das amostras no espaço de ordenação criado. 
#    O que é apenas a rotação do espaço das nossas variáveis originais. 
     PCA_iris$scores
     
     
     

     

# Exemplo Hitters ---------------------------------------------------------
  library(ISLR)
     

# Dados
dados_batedores <- Hitters |>
       drop_na() |>
       select(-League, -Division, -NewLeague) |>   # somente numericos
       mutate(
         across(.fns = function(x){(x-mean(x))/sd(x)}   # padronizar
         )
       )
     
# Analise
princomp(dados_batedores) |>
       fviz("var")
     
princomp(dados_batedores) |>
       fviz("ind")

# Importancia dos componentes
princomp(dados_batedores) |>
       plot()

# Resultados
PCA_batedores <- princomp(dados_batedores)
summary(PCA_batedores)

# Variancia Acumulada (= "Cumulative Proportion" do summary)
cumsum(PCA_batedores$sdev^2)/sum(PCA_batedores$sdev^2)

# cluster
fviz_cluster(kmeans(dados_batedores, centers = 3), data = dados_batedores)
fviz_cluster(hcut(dados_batedores, k = 3), data = dados_batedores)
     

# PCA na matriz de distancias ---------------------------------------------
# Corresponde a técnica: escalonamento multidimensional clássico/simples (NMDS)


# EXEMPLO 1
# Matriz de 
# Podemos ir trocando o método para ver o que vai acontecendo, quais pontos
# vão se destacando
matriz_de_distancias_batedores <- dados_batedores |>
  dist(method = "manhattan") |>
  # dist(method = "euclidian") |>
  # dist(method = "max") |>
  as.matrix()

# Fazendo a PCA na matriz de distrância. 
# Enquanto a PCA procura a melhor dimensão e traz para 2 componentes (ordenando
# dos componentes que ajudam mais a prever a variância)
# Awui significa: eu gostaria que a matriz de distancia em duas dimensões
# (dois componentes) seja parecida com a distancia que eu tenho entre todos
# os pontos usando todas as dimensões
pca_matriz_de_distancias <- princomp(matriz_de_distancias_batedores)
fviz(pca_matriz_de_distancias, "ind")


# EXEMPLO 2
#Dados
dados_corredores <- factoextra::decathlon2 |>
  drop_na() |>
  select_if(is.numeric) |>
  mutate(
    across(
      .fns = function(x){(x-mean(x))/sd(x)}
    )
  )

# Importancia dos componentes
dados_corredores |>
  princomp() |>
  plot()

# Amostras
dados_corredores |>
  princomp() |>
  fviz("ind") 

dados_corredores |>
  princomp() |>
  fviz("var")

dados_corredores |>
  princomp() |>
  biplot()

dados_corredores |>
  dist() |>
  as.matrix() |>
  princomp() |>
  fviz("ind")

summary (princomp(dados_corredores))

princomp(dados_corredores)$loadings



#  xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx ---------------------------------
# USP/Esalq ---------------------------------------------------------------
# EXEMPLO APLICAÇÃO -------------------------------------------------------

# Fonte dos dados: https://www.kaggle.com/datasets/vipulgohel/clustering-pca-assignment?resource=download&select=Country-data.csv

pacotes <- c("plotly",
             "tidyverse",
             "ggrepel",
             "knitr", "kableExtra",
             "reshape2",
             "PerformanceAnalytics",
             "psych",
             "Hmisc",
             "readxl",
             "cluster",
             "factoextra") 
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
paises <- read.csv("raw_datas_exemplos/Países PCA Cluster.csv", sep = ",", dec = ".")


# Estatistica Descritiva --------------------------------------------------
summary(paises)

# Scatter e ajuste entre as variáveis 'renda' e 'expectativa de vida'
paises %>%
  ggplot() +
  geom_point(aes(x = income, y = life_expec),
             color = "darkorchid",
             size = 3) +
  geom_smooth(aes(x = income, y = life_expec),
              color = "orange", 
              method = "loess", 
              formula = y ~ x, 
              se = FALSE,
              size = 1.3) +
  labs(x = "income",
       y = "life_expec") +
  theme_bw()

# Scatter e ajuste entre as variáveis 'exports' e 'gdpp'
paises %>%
  ggplot() +
  geom_point(aes(x = exports, y = gdpp),
             color = "darkorchid",
             size = 3) +
  geom_smooth(aes(x = exports, y = gdpp),
              color = "orange", 
              method = "loess", 
              formula = y ~ x, 
              se = FALSE,
              size = 1.3) +
  labs(x = "exports",
       y = "gdpp") +
  theme_bw()

# Correlacao --------------------------------------------------------------
# Coeficientes de correlação de Pearson para cada par de variáveis
rho <- rcorr(as.matrix(paises[,2:10]), type="pearson")

correl <- rho$r # Matriz de correlações
sig_correl <- round(rho$P, 4) # Matriz com p-valor dos coeficientes

# Elaboração de um mapa de calor das correlações de Pearson entre as variáveis
ggplotly(
  paises[,2:10] %>%
    cor() %>%
    melt() %>%
    rename(Correlação = value) %>%
    ggplot() +
    geom_tile(aes(x = Var1, y = Var2, fill = Correlação)) +
    geom_text(aes(x = Var1, y = Var2, label = format(Correlação, digits = 1)),
              size = 5) +
    scale_fill_viridis_b() +
    labs(x = NULL, y = NULL) +
    theme_bw())

# Visualização das distribuições das variáveis, scatters, valores das correlações
chart.Correlation(paises[,2:10], histogram = TRUE, pch = "+")





# PCA -- Análise Fatorial Por Componentes Principais ----------------------




# 1. Testar se a PCA pode ser feita:

# Teste de Esfericidade de Barlett ----------------------------------------
# Esse teste compara sua matriz de correlações com a matriz identidade (valor 1
# na diagonal e zero fora da diagonal). Significa que se a matriz de correla
# ção é igual ou diferente da matriz identidade. 
# Se p<0.05: Matriz de correlação != da Matriz identidade (desejado para a PCA)
cortest.bartlett(paises[,2:10])

# Conclusão: p<0.05. Logo, a matriz correlação difere da matriz identidade. Ou
#            seja, de fato, podemos prosseguir com a análise de PCA pq a matriz
#            de correlação é importante.





# 2. Análise PCA:
# Elaboração da análise fatorial por componentes principais
fatorial <- principal(paises[,2:10],
                      nfactors = length(paises[,2:10]),
                      rotate = "none",
                      scores = TRUE)
fatorial

# Eigenvalues (autovalores): Quantidade de variáveis na análise. Se temos 9 
#                            variáveis, teremos 9 eigenvalues. Também representa
#                            a quantidade máxima de possíveis fatores na análise
#                            A partir dos autovalores conseguimos ver o % de 
#                            variância compartilhada das variáveis originais em
#                            cada fator.
eigenvalues <- round(fatorial$values, 5)
eigenvalues
round(sum(eigenvalues), 2)

# Identificação da variância compartilhada em cada fator
variancia_compartilhada <- as.data.frame(fatorial$Vaccounted) %>% 
  slice(1:3)

rownames(variancia_compartilhada) <- c("Autovalores",
                                       "Prop. da Variância",
                                       "Prop. da Variância Acumulada")


# O que fornecer no Resultado: ---------------------------------------------
# Variância compartilhada pelas variáveis originais para a formação de cada fator
round(variancia_compartilhada, 3) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE, 
                font_size = 20)

# Cálculo dos scores fatoriais: pesos que geram a relação entre as variáveis 
# originais e os fatores. O quanto cada variável está contribuindo para a 
# formação de cada fator (componente)
scores_fatoriais <- as.data.frame(fatorial$weights)

# Visualização dos scores fatoriais
round(scores_fatoriais, 3) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE, 
                font_size = 20)

# Cálculo dos fatores propriamente ditos (calculado a partir do fatorial$weights)
fatores <- as.data.frame(fatorial$scores)
View(fatores)

# Coeficientes de correlação de Pearson para cada par de fatores (ortogonais)
rho <- rcorr(as.matrix(fatores), type="pearson")
round(rho$r, 4)



# Quais são os componentes importantes para cada fator? -------------------
# Variáveis com maiores valores contribuem mais para o fator (eixos/componentes).
# Valores negativos (correlação negativa) indica que a variável contribui com
# negativamente (inversamente proporcional) para o fator. O valor positivo é
# ao contrario

# Cálculo das cargas fatoriais
cargas_fatoriais <- as.data.frame(unclass(fatorial$loadings))

# Visualização das cargas fatoriais
round(cargas_fatoriais, 3) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE, 
                font_size = 20)


# Comunalidades -----------------------------------------------------------
# Indica a variancia das variáveis originais nos fatores extraídos

# Cálculo das comunalidades
comunalidades <- as.data.frame(unclass(fatorial$communality)) %>%
  rename(comunalidades = 1)

# Como extraímos todos os fatores, temos valor 1 (100%). Pq neste caso estamos
# extraindo todos os fatores possíveis (os 9) então não estamos perdendo explica
# ção de variancia das variáveis. Mas quando selecionarmos 1 ou 2 fatores (eixos)
# esse valor vai deixar de ser 1. e vai representar o % de variancia explicada
# com os fatores (eixos) selecionados.

# Visualização das comunalidades (aqui são iguais a 1 para todas as variáveis)
# Foram extraídos 9 fatores neste primeiro momento
round(comunalidades, 3) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = FALSE,
                font_size = 20)


# Selecão dos fatores -----------------------------------------------------

# Critério para a redução:
# Critério Kaiser: a gnt fica apenas com fatores com autovalores acima de 1.
#                  que são potencialmente onde estão os maiores percentuais de
#                  variância das variáveis originais.


# Refazer a PCA extraindo autovalores maiores que 1:

# Definição da quantidade de fatores com eigenvalues maiores que 1
k <- sum(eigenvalues > 1)
print(k)
# PCA com quantidade 'k' de fatores com eigenvalues maiores que 1
fatorial2 <- principal(paises[,2:10],
                       nfactors = k,
                       rotate = "none",
                       scores = TRUE)
fatorial2

# Conclusão: Neste caso, ao definir que só queremos eigenvalues (autovalores)
# acima de 1 ficamos apenas com 3 fatores (eixos)



# COMUNALIDADE - agora ela difere de 1:
# Cálculo das comunalidades com apenas os 'k' ('k' = 3) primeiros fatores
comunalidades2 <- as.data.frame(unclass(fatorial2$communality)) %>%
  rename(comunalidades = 1)

# Visualização das comunalidades com apenas os 'k' ('k' = 3) primeiros fatores
round(comunalidades2, 3) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = FALSE,
                font_size = 20)

# Conclusão: podemos ver o quanto resta de variancia explicada em cada variável
#            quanto menor o valor mais perda (em termos de explicação da 
#            variancia) a variável sofreu


# Gráficos ----------------------------------------------------------------


# Loading plot com as cargas dos dois primeiros fatores
cargas_fatoriais[, 1:2] %>% 
  data.frame() %>%
  rownames_to_column("variáveis") %>%
  ggplot(aes(x = PC1, y = PC2, label = variáveis)) +
  geom_point(color = "darkorchid",
             size = 3) +
  geom_text_repel() +
  geom_vline(aes(xintercept = 0), linetype = "dashed", color = "orange") +
  geom_hline(aes(yintercept = 0), linetype = "dashed", color = "orange") +
  expand_limits(x= c(-1.25, 0.25), y=c(-0.25, 1)) +
  theme_bw()



# Cluster com resultado PCA -----------------------------------------------

# Adicionando os fatores extraídos no banco de dados original
paises <- bind_cols(paises,
                    "fator_1" = fatores$PC1, 
                    "fator_2" = fatores$PC2,
                    "fator_3" = fatores$PC3)

# Análise de Cluster Utilizando os 3 Fatores

# Análise dos fatores (média e desvio padrão)
summary(paises[,11:13])
sd(paises[,11])
sd(paises[,12])
sd(paises[,13])

## ATENÇÃO: os clusters serão formados a partir dos 3 fatores
## Não aplicaremos o Z-Score, pois os fatores já são padronizados


# Cluster -----------------------------------------------------------------


# 1.Matriz de dissimilaridades (com os fatores da PCA)
matriz_D <- paises[,11:13] %>% 
  dist(method = "euclidean")

# 2. Elaboração da clusterização hierárquica
# single: privilegia os vizinhos próximos
# complete: privilegia os vizinhos distantes
# average: pega a média
cluster_hier <- agnes(x = matriz_D, method = "complete")



# 3. Definição do esquema hierárquico de aglomeração
# As distâncias para as combinações em cada estágio
coeficientes <- sort(cluster_hier$height, decreasing = FALSE) 
coeficientes

# Tabela com o esquema de aglomeração. Interpretação do output:

## As linhas são os estágios de aglomeração
## Nas colunas Cluster1 e Cluster2, observa-se como ocorreu a junção
## Quando for número negativo, indica observação isolada
## Quando for número positivo, indica cluster formado anteriormente (estágio)
## Coeficientes: as distâncias para as combinações em cada estágio 
## (distancia q ocorreu o agrupamento)
esquema <- as.data.frame(cbind(cluster_hier$merge, coeficientes))
names(esquema) <- c("Cluster1", "Cluster2", "Coeficientes")
esquema


# 4. Dendograma
# Construção do dendrograma
dev.off()
fviz_dend(x = cluster_hier, show_labels = FALSE)

# Dendrograma com visualização dos clusters (definição de 10 clusters)
fviz_dend(x = cluster_hier,
          h = 3.0,             # fixou o corte dos cluster em 3
          show_labels = FALSE,
          color_labels_by_k = F,
          rect = F,
          rect_fill = F,
          ggtheme = theme_bw())

# Criando variável categórica para indicação do cluster no banco de dados
## O argumento 'k' indica a quantidade de clusters
paises$cluster_H <- factor(cutree(tree = cluster_hier, k = 10))


# 5. Verificar se as variáveis estão discriminando bem os grupos
# Análise de variância de um fator (ANOVA). 

# Interpretação do output:
## Mean Sq do cluster_H: indica a variabilidade entre grupos
## Mean Sq dos Residuals: indica a variabilidade dentro dos grupos
## F value: estatística de teste (Sum Sq do cluster_H / Sum Sq dos Residuals)
## Pr(>F): p-valor da estatística 
## p-valor < 0.05: pelo menos um cluster apresenta média estatisticamente diferente dos demais

## A variável mais discriminante dos grupos contém maior estatística F (e significativa)

# ANOVA da variável 'fator 1'
summary(anova_fator_1 <- aov(formula = fator_1 ~ cluster_H,
                             data = paises))

# ANOVA da variável 'fator 2'
summary(anova_fator_2 <- aov(formula = fator_2 ~ cluster_H,
                             data = paises))

# ANOVA da variável 'fator 3'
summary(anova_fator_3 <- aov(formula = fator_3 ~ cluster_H,
                             data = paises))

# Algumas estatísticas descritivas entre clusters
# Podemos nesse caso observar a média de PIB para cada um dos grupos do 
# cluster. O mesmo com as outras variáveis. "indicador de saúde, expectativa de vida"...

# PIB
group_by(paises, cluster_H) %>%
  summarise(
    mean = mean(gdpp, na.rm = TRUE),
    sd = sd(gdpp, na.rm = TRUE),
    min = min(gdpp, na.rm = TRUE),
    max = max(gdpp, na.rm = TRUE),
    obs = n())

# Indicador de saúde
group_by(paises, cluster_H) %>%
  summarise(
    mean = mean(health, na.rm = TRUE),
    sd = sd(health, na.rm = TRUE),
    min = min(health, na.rm = TRUE),
    max = max(health, na.rm = TRUE),
    obs = n())

# Expectativa de vida
group_by(paises, cluster_H) %>%
  summarise(
    mean = mean(life_expec, na.rm = TRUE),
    sd = sd(life_expec, na.rm = TRUE),
    min = min(life_expec, na.rm = TRUE),
    max = max(life_expec, na.rm = TRUE),
    obs = n())






