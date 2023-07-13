
# Modelos Não-Supervisionados

# Em geral resolvem tres problemas
# 1. Clusterização
# 2. Redução de Dimensionalidade
# 3. Detecção de anomalias

# Pacotes -----------------------------------------------------------------
library(tidyverse)
library(tidymodels)
library(cluster)
library(factoextra)


# Teoria ------------------------------------------------------------------

#> Diferença entre as técnicas:
# Cluster: Consiste no agrupamento de amostras (observções; linhas do banco de dados)
# segundo algumas variáveis;

# Analise Fatorial (PCA): consiste na união de variáveis (colunas do banco de dados)
# exclusivo para análises quantitativas. 

# Analise Correspondencia: associação entre duas os mais variáveis qualitativas.

#> Objetivo: 
#> Agrupar as observações em grupos homogêneos internamente e heterogênios
#> entre si. Porntanto, dentro do grupo temos observações semelhantes com
#> base nas variáveis utilizadas na análise. E entre grupos distintos, temos
#> observações diferentes com base nas variáveis utilizadas na análise.

#> Quando usar?
#> Tecnica exploratória. Sem caráter preditivo para observaações fora da amostra.
#> Portanto, se novas observações são adicionadas, temos que refazer as 
#> análises.

#> Cluster depende de:
#> 1. Escolha do método para medir matriz de distancia
#> 2. Escolha do método de encadeamento (simple, average, complete, ward...)
 
# Cluster -----------------------------------------------------------------
# Um modelo de clusterização presume que existem grupos escondidos dando origem 
# aos dados observados. Um algoritmo de clusterização é uma estratégia para 
# encontrar esses grupos. 


#> 1. Métodos Hierárquicos Aglumerativos
#> 2. Métodos Baseados em Centroides  
#> 3. Métodos baseados em Distribuições
 
#> Obs: No método Hierárquico Aglomerativo, a quantidade de cluster é definida
#> ao longo da análise. Nos métodos não-hierarquicos (kmeans), defini-se a priori 
#> quantos clusters serão formados.




# Passo 0: Padronização dos dados -------------------------------------------
# Padronização é importante para que todas as variáveis variem dentro da mesma
# magnitude de valores - com média 0 e desvio padrão 1. Não precisamos padronizar
# quando as variáveis possuem a mesma unidade. Não altera em nada, neste caso.

# Comumente se aplica o z-score (torna variáveis com média zero e desvio padrão 1)
# formula: valor - média/desvio padrão. Isso retira as unidades da variáveis.


# Preparação dos dados
# Dados: espera receber apenas numeros
iris_tibble <- tibble(iris) |> 
  select(-Species)

# Padronização
# Opção 1:
var <- vegan::decostand(iris_tibble, method= "standardize")


# Opção 2:
# Função própria

padroniza <- function(x){(x-mean(x))/sd(x)}

iris_tibble |>
  mutate(
    Sepal.Length = padroniza(Sepal.Length),
    Sepal.Width = padroniza(Sepal.Width),
    Petal.Length = padroniza(Petal.Length),
    Petal.Width = padroniza(Petal.Width))

# Opção 3:
# Função scale()
iris_padronizado <- as.data.frame(scale(iris_tibble[,1:4]))
rownames(iris_padronizado) <- iris_padronizado$traits


# xxxxxxxxxxxxxxxxxxxxxx --------------------------------------------------


# XX. Cluster: Métodos Hierárquicos -------------------------------------------
# As observações são agrupadas devido a sua proximidade (distância entre observações).
# Vai agrupando observações mais próximas até que comece a parecer uniões de 
# elementos diferentes demais.


# Passo 1: MATRIZ DE DISTANCIA --------------------------------------------
  # Escolhemos o método de distância que será utilizado:
  # Os métodos criam matriz de dissimilaridade, quanto menor o valor, mais
  # próximo estão as observações, mais parecidas elas são.

  ## "euclidean": distância euclidiana
  ## "euclidiana quadrática": elevar ao quadrado matriz_D (matriz_D^2)
  ## "maximum": distância de Chebychev;
  ## "manhattan": distância de Manhattan (ou distância absoluta ou bloco);
  ## "canberra": distância de Canberra;
  ## "minkowski": distância de Minkowski

  #> Distância Euclidiana: geométrica mais usada. Baseada na medida em um 
  #>                       sistema de coordenadas cartesianas.Podemos usá-la 
  #>                       para expressar a distância entre duas amostras de 
  #>                       vegetação ao registramos as abundâncias de cada 
  #>                       espécie. 
                        

  #> Distância Manhattan:  distancia do taxista. pois os seguimentos que 
  #>                       constituem o seu comprimento só podem ser verticais 
  #>                       ou horizontais;
 
                        
  #> Distância de Bray-Curtis: proporção de similaridade ou dissimilaridade 
  #>                           (distância) na abundância das espécies.
  
                          
  #> Jaccard: indica a proporção de espécies compartilhadas entre duas amostras
  #>          em relação ao total de espécies.


  #> Distancia Baseada na correlação de Pearson: Esse método é diferente dos 
  #> demais, pois não é uma matriz de dissimilaridade como os outros. Mas uma
  #> matriz de similaridade. Então quanto maior o valor, mais similares são as
  #> observações, então mais próximas estão as amostras. 
                                     

#> Em ECOLOGIA: Euclidiana, Bray-Curtis e Jaccard são as mais usadas



# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# CALCULAR A MATRIZ DE DISSIMILARIDADE

# Opção 1:
# dist () - Calcular as distâncias. O resultado dessa função é especial e foi 
#           feito para colocar dentro da função hclust(). Só aceita números, 
#           não aceita factors.

objeto_distancias <- dist(iris_tibble, method =  "euclidean") 
objeto_manh <- dist(iris_tibble, method =  "manhattan") 
objeto_1 <- dist(iris_tibble, method =  "maximum") 
objeto_2 <- dist(iris_tibble, method =  "canberra") 
objeto_3 <- dist(iris_tibble, method =  "binary")
objeto_4 <- dist(iris_tibble, method =  "minkowski")

# outra forma de escrita
iris_tibble |> 
  select(where(is.numeric)) |> 
  dist(method = "euclidean")
  

# Opção 2:
# vegdist () - Índices de dissimilaridade para Community Ecologists
objeto5 <- vegan::vegdist (iris_tibble, method = "bray", diag=FALSE, upper=FALSE)
objeto6 <- vegan::vegdist (iris_tibble, method = "cao", diag=FALSE, upper=FALSE)

# Visualizar a matriz de Distância
# tanto do dist()
as.matrix(objeto_distancias) |> 
  View()
# quanto do vegdist()
as.matrix(objeto5) |> 
  View()


# xxxxxxxxxxxxxxxxxxxxxx
# Visualizar a matriz de dissimilaridades
data.matrix(objeto_distancias)  |>  
  kable() |> 
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE, 
                font_size = 20)



# Passo 2 DENDROGRAMA-----------------------------------------------------------------
# Escolhemos o método de união/agrupamento (encadeamento)

# Method é o tipo de encadeamento:
#> "complete": encadeamento completo (furthest neighbor ou complete linkage)
#> "single": encadeamento único (nearest neighbor ou single linkage)
#> "average": encadeamento médio (between groups ou average linkage)
#> "median": mais usado| menos influenciado por outlier
#>  Ward:
#>  UPGMA: "average"
#> "binary": permite comparar vetores binários. Faz uma contagem de concordancia entre sim e nao
#> "complete": penaliza mais o cluster, pq ele sempre compara com o pior ponto do grupo 
#>             (o ponto mais distante). Então temos saltos maiores no dendrograma.


# XXXXXXXXXXXXXXXXXX
# CALCULAR o cluster
# Opção 1: hclust()
dendro <- hclust(objeto_distancias, method = "median") 
dendro <- hclust(objeto_distancias, method = "average")


# opção 2: agnes()
cluster_hier <- cluster::agnes(x = objeto_distancias, method = "single")




#XXXXXXXXXXXXXXXXXXXxxx
# Plotando o DENDOGRAMA
plot(dendro)
    
    # Melhorando o gráfico
      # hang = -1: coloca todos os ramos do mesmo tamanho
    plot(dendro, hang=-1) 

      # Nomear eixos
    plot(dendro, xlab = "Similarity", ylab = "Bray-Curtis - método UPGMA")
    
      # Cluster na Horizontal
    plot(as.dendrogram(dendro), horiz=T, xlab = "Similarity", ylab = "Bray-Curtis - método UPGMA")
    
      # Colocando nomes das unidades amostrais no gráfico
      # Ler os dados novamente com os nomes na primeira coluna
      dados = read.table ("insetos.txt", h=T, row.names = 1)
      
      # Cortar na quantidade de grupos
    rect.hclust(dendro, k=3)

      # Cortar na linha que deseja. 
      # Nesse caso o corte dos grupos ocorrerá na distancia 2
    rect.hclust(dendro, h=2) 

     # Renomear as linhas para 
    row.names(tabela) <- c("1 PACFMO","2 PAF","3 P","4PAC") 
    
      # Ver em uma lista quem pertence a cada grupo
      # Neste caso, a Area 1 pertence ao grupo 1. Assim como a Area 2.
      # Já a Area 3 pertence ao grupo 2. 
    grupos = cutree(dendro, k=3)
    grupos
    # Area1  Area2  Area3  Area4  Area5  Area6  
    # 1      1      2      3      1      1      
    
    
      # Colocando nomes no cluster
    plot(as.dendrogram(dendro), horiz=T) # cluster na horizontal
    labels(as.dendrogram(dendro))        #os nomes atuais que estao aparecendo
    row.names(dendro) <- c("ds","ds")
    
      # Codigos para colorir os locais de acordo com o grupo ao qual pertencem
    
    #> Olhar as categorias
    levels(iris$Species)
    #[1] "setosa"     "versicolor" "virginica" 
    
    #> Fazer um código para as categorias
    codigos=ifelse(iris$Species=="setosa", "set", ifelse(iris$Species == "versicolor","ver","vir")) 
    # Interpretação: se a spp for setosa colocar o codigo "set", se for 
    # versicolor colocar codigo ver, caso contrario colocar vir
    # Se tiver mais categoria pode colocar no lugar de "vir" outro "ifelse"
    
    #> Unir nomes das linhas com categorias
    novos.nomes = paste(row.names(iris), codigos,sep="-")
    novos.nomes
    
    #>alterar as lables do dendrograma
    dendro$labels = novos.nomes
    
    #> plotar com novo nome
    plot(dendro, horiz=T)
    dendro2 = as.dendrogram(dendro)
    par(mar=c(3,2,2,4)) # aumentar a margem
    plot(dendro2, horiz=T)


  
    
# Definir Quantidade de grupos --------------------------------------------
  # Verificar o salto no gráfico
  # Onde está o maior salto, nos indica o número de grupos que vamos optar.
  # Se o maior salto ocorre na primeira barra (direita para esquerda), indica 
  # apenas 2 grupos, se ocorre na segunda, indica três e assim por diante.

  # Montamos o barplot
  # barplot completo é mto dificil de visualizar
    barplot(dendro$height)

  # Pegar só os 10 primeiros grupos
  # Colocamos o rev() para poder interpretar da esquerda para direita
  # Aqui, neste exemplo, seriam portanto 2 grupos.
  # Pq do primeiro para o segundo dá o maior salto 
    alturas_ao_contrario <- rev(dendro$height)
    barplot(alturas_ao_contrario[1:10])


# Extrair as Distâncias ---------------------------------------------------
# As distâncias onde ocorreu a junção dos grupos

cluster_hier <- cluster::agnes(x = objeto_distancias, method = "single")    
coeficientes <- sort(cluster_hier$height, decreasing = FALSE) 
coeficientes      
    

# Tabela com o Esquema de Aglomeração -------------------------------------
# Interpretação do output:

## As linhas são os estágios de aglomeração
## Nas colunas Cluster1 e Cluster2, observa-se como ocorreu a junção
## Quando for número negativo, indica observação isolada
## Quando for número positivo, indica cluster formado anteriormente (estágio)
## Coeficientes: as distâncias para as combinações em cada estágio
## Então -1 (obs1) -2(obs2) -3(obs 3)
## Então 1 (cluster formado entre a obs.1 (-1) e obs 4 (-4) indicado na primeira
## linha). Então nesse caso o cluster 1 se juntou com a obs 5 (-5).
## e nos coeficientes podemos ver a posição que aconteceu a junção
esquema <- as.data.frame(cbind(cluster_hier$merge, coeficientes))
names(esquema) <- c("Cluster1", "Cluster2", "Coeficientes")
esquema
# Cluster1 Cluster2 Coeficientes
# 1       -1       -4     3.713489   
# 2        1       -5     4.170132
# 3        2       -3     6.044832
# 4        3       -2     7.186793

# ex2. 
esquema <- as.data.frame(cbind(cluster_hier$merge, coeficientes))
names(esquema) <- c("Cluster1", "Cluster2", "Coeficientes")
esquema

# Visualização do esquema hierárquico de aglomeração
esquema %>%
  kable(row.names = T) %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE, 
                font_size = 20)


# Gráficos de distâncias: Calor ---------------------------------
# A distância através de um mapa de calor

# Com esse gráfico podemos ver q temos um grupo azul escuro (mto mais distante), 
# um azul claro (distante) e uma vermelho (mais proximos)    
    
# com iris
distancia_alternativa <- get_dist(iris_tibble)
    fviz_dist(distancia_alternativa)
# outros dados
# em vermelho quem é mais próximo entre si.
# em azul quem é distante entre si.
    fviz_dist(dist(USArrests))    
    
# outros dados
    dados::pinguins |>
      select(comprimento_bico, profundidade_bico,
             comprimento_nadadeira, massa_corporal) |>
      drop_na() |>
      dist() |>
      fviz_dist()
    

    
# Gráfico Dendrograma: com ggplot2 ----------------------------------------
# Pode alterar como no ggplot, temas entre outros
library(ggdendro)
ggdendrogram(hclust(objeto_distancias))+
  theme_dendro()



# Grafico Dendograma: fviz ------------------------------------------------------
fviz_dend(x = cluster_hier)



# Dendrograma com visualização dos clusters -------------------------------
# Definição de 3 clusters
fviz_dend(x = cluster_hier,
          k = 3,
          k_colors = c("deeppink4", "darkviolet", "deeppink"),
          color_labels_by_k = F,
          rect = T,
          rect_fill = T,
          lwd = 1,
          ggtheme = theme_bw())

# Junções do esquema
coeficientes <- cluster_hier$height
coeficientes



# Exemplo 1: USA ------------------------------------------------------------
us_dist_euclid <- dist(USArrests)
us_dist_manht <- dist(USArrests, method = "manhattan")

us_dendro_e <- hclust(us_dist_euclid)
us_dendro_m <- hclust(us_dist_manht)
us_dendro_m_m <- hclust(us_dist_manht, method = "median")

alturas_ao_contrario_e <- rev(us_dendro_e$height)
alturas_ao_contrario_m <- rev(us_dendro_m$height)
alturas_ao_contrario_m_m <- rev(us_dendro_m_m$height)

barplot(alturas_ao_contrario_e[1:10])
barplot(alturas_ao_contrario_m[1:10])
barplot(alturas_ao_contrario_m_m[1:10])

barplot(alturas_ao_contrario_e[1:10]) # quantos grupos necessários?
plot(us_dendro_e)                     # plot dendrograma
rect.hclust(us_dendro_e, k = 2)       # corto na quant. de grupos necessarios

barplot(alturas_ao_contrario_m[1:10])
plot(us_dendro_m)
rect.hclust(us_dendro_m, k = 2)

barplot(alturas_ao_contrario_m_m[1:10])
plot(us_dendro_m_m)
rect.hclust(us_dendro_m_m, k = 3)


# Gráfico 3D --------------------------------------------------------------

rownames(iris_tibble) <- iris_tibble$traits

plot3D::scatter3D(x=iris_tibble$Sepal.Length,
          y=iris_tibble$Sepal.Width,
          z=iris_tibble$Petal.Length,
          phi = 0, bty = "g", pch = 20, cex = 2,
          xlab = "Tamanho Sepala",
          ylab = "Largura Sépala",
          zlab = "Tamanho pétala",
          main = "Traços Biológicos",
          clab = "Ecologia")>
  text3D(x=iris_tibble$Sepal.Length,
         y=iris_tibble$Sepal.Width,
         z=iris_tibble$Petal.Length,
         labels = rownames(iris_tibble),
         add = TRUE, cex = 1)




# xxxxxxxxxxxxxxxxxxxxxx --------------------------------------------------


# XX. Cluster: Métodos Baseados Centroides ----------------------------------

# Estes metodos procuram os melhores centros de massa dentro de cada nuvem de 
# observações.Normalmente são métodos iterativos que procuram o melhor ponto
# de referência para cada cluster. O número de cluster é fixado no começo do 
# algoritmo e então os pontos de referência (centróides) são atualizados até
# encontrar grupos tão diferentes quanto possível. A clusterização hierárquica 
# (anterior) após medir as distancias entre os pontos, pegamos as observações 
# mais próximas e vamos agrupando, até que os clusters surjam naturalmente.


# Existem Vários Métodos:


# 1. Kmeans: o mais usado

# Algoritmo se preocupa em construir centróides que estejam o mais próximo possível
# dos pontos em sua órbita (ou cluster). Existem várias estratégias para medir
# essa variabilidade, aqui usamos o kmeans.No K-means medimos o quão longo os
# pontos estão com relação ao centróide somando todas as distâncias. 
# Soma todas as distâncias euclidianas até o centróide do grupo e o K-means tenta minimizar 
# essa distância nos n grupos simultâneamente.

# Soma do Quadrado dos Grupos (SSW) - essa métrica pode ser usada para comparar o 
# resultado do algoritmo com qualquer cluster. Essa métrica mede a qualidade
# dos clusters (podemos mudar o número de grupo e ver qual tem esse valor minizado.


# Passo 0: Dados ----------------------------------------------------------
#          Selecionando somente dados numéricos
dados_k_medias <- iris |>
  tibble() |>
  select(-Species)

dados_k_medias


# K-means -----------------------------------------------------------------
modelo_k_medias <- kmeans(dados_k_medias, centers = 3)
modelo_k_medias


#> Interpretando resultados:

#> K-means clustering with 3 clusters: Quantidade de grupos
#> of sizes 50, 53, 47: tamanho de cada grupo
#> Cluster means: fornece as coordenadas do centroide de cada grupo
#> Clustering vector: a qual grupo pertence cada ponto
#> Within cluster sum of squares by cluster: é o SSW 
#> (between_SS / total_SS =  79.0 %): os cluster são capazes de explicar 79%
#> da variação total. Tem gnt que não gosta de olhar para esse numero




# Plotando os centroides --------------------------------------------------
# Separando apenas 2 variáveis para poder visualizar no plano os centroides
dados_k_medias2 <-iris |> 
  tibble() |> 
  select(Sepal.Length, Sepal.Width)

dados_k_medias2

# Plotando os centroides: esses numeros x e y, aparecem no resultado
# "cluster means"
dados_k_medias2 |>
  ggplot(aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_point() +
  geom_point(aes(x = 5.006, y = 3.428), color = "red", size = 10)+ 
  geom_point(aes(x = 5.901, y = 2.748), color = "red", size = 10)+
  geom_point(aes(x = 6.85, y = 3.074), color = "red", size = 10)



# Métrica SSW -------------------------------------------------------------
# SSW: é a soma de todas as distâncias até o centróide do grupo.
# K-means tentará minimizar essa soma
# Pegando o SSW. 1 pra cada grupo (ou seja, k)


#> Resultado do K-means mostra "Within cluster sum of squares by cluster:"
#> onde observamos o SSW para cada grupo. O centroide de cada grupo. No item
#> "total_SS =  88.4 %" temos que 88.4% da variabilidade nos dados foi explicado 
#> pela separação destes três grupos. Esse número, quanto maior, melhor. Se forem
#> grupos bastante separados, esse valor dará próximo a 100. Esse valor tem 
#> interpretação parecida com o r-square da regressão.
   modelo_k_medias

# Ou podemos obter através de:
modelo_k_medias$withinss

# soma dos SSW anteriores: sum(modelo_k_medias$withinss)
modelo_k_medias$tot.withinss

# distancia percorrida até o cluster mais próximo
modelo_k_medias$betweenss

# Se tivesse 1 centroide só para todos os grupos o quanto teriam
# que se deslocar. ou seja, qual a distancia para o centroide unico
modelo_k_medias$totss


# Escolher o melhor numero de clusters ------------------------------------
# Testando com várias qauntidade de clusters (com 1, com 2, com 3 ...)
# Identificamos quantos cluster a partir de uma avaliação visual.
# Tentamos identificar o "cotovelo", ponto com o maior angulo, mais aberto

# iter.max : é o numero de iterações que o algoritmo pode fazer para 
# conseguir chegar no ideal. 

todos_os_twss <- c(
  (dados_k_medias |> kmeans(centers = 1, iter.max = 400))$tot.withinss,
  (dados_k_medias |> kmeans(centers = 2, iter.max = 400))$tot.withinss,
  (dados_k_medias |> kmeans(centers = 3, iter.max = 400))$tot.withinss,
  (dados_k_medias |> kmeans(centers = 4, iter.max = 400))$tot.withinss,
  (dados_k_medias |> kmeans(centers = 5, iter.max = 400))$tot.withinss,
  (dados_k_medias |> kmeans(centers = 6, iter.max = 400))$tot.withinss,
  (dados_k_medias |> kmeans(centers = 7, iter.max = 400))$tot.withinss,
  (dados_k_medias |> kmeans(centers = 8, iter.max = 400))$tot.withinss,
  (dados_k_medias |> kmeans(centers = 9, iter.max = 400))$tot.withinss,
  (dados_k_medias |> kmeans(centers = 10, iter.max = 400))$tot.withinss,
  (dados_k_medias |> kmeans(centers = 11, iter.max = 400))$tot.withinss,
  (dados_k_medias |> kmeans(centers = 12, iter.max = 400))$tot.withinss
)

# Plotar os SSW com diferentes quantidades de cluster
# Aqui, no gráfico de linha, tentamos identificar o ponto cotovelo (90º)
# com o angulo aberto. Pq essa será a maior quantidade
# o último ponto antes de ficar linear, parece ser o 3
qplot(1:12, todos_os_twss, geom = "line")
qplot(1:12, todos_os_twss, geom = "col")

# Esses valores tendem a ir caindo, na medida em que aumentamos o número de 
# cluster. Então se o valor não cair, significa q estamos usando poucas iterações
# podemos voltar e mudar o parâmetro iter.max.



# Seleção com Método menos manual ----------------------------------------------
# a função fviz ( ):é uma função pra visualizar objetos de análises não supervisionadas
# específicas - cluster, pca, ca ... muitas outras

# fviz_ são funções especializadas:
# faz a mesma coisa que fizemos no item "Escolher o melhor numero de clusters"
# só que mais bonito, e com formato ggplot para alterações.

# kmeans: método para cluster baseado em centroide
# dados_k_medias: tabela de dados
fviz_nbclust(dados_k_medias, kmeans, method = "wss")

# ex. add elementos do ggplot
fviz_nbclust(dados_k_medias, kmeans, method = "wss")+
  geom_col()

# argumento hcut: método para cluster hierárquico
# dados_k_medias: tabela de dados
fviz_nbclust(dados_k_medias, hcut, method = "wss")


#> Podemos fazer com os dois métodos "centroides" e "hierarquico" para ver 
#> qual seria o melhor método.


# Com outros dados
fviz_nbclust(USArrests, kmeans, method = "wss")
fviz_nbclust(USArrests, hcut, method = "wss")


# Pegar o Grupo de cada observação ----------------------------------------
# Ao fazer o cluster cada observação pertencerá a um grupo. Podemos observar
# o grupo no item "Clustering vector:" do resultado. Mas uma maneira mais fácil
# é usar a função augment(). Ele cria uma coluna nova ".cluster" com o ID do 
# grupo ao qual a observação pertence.
dados_com_cluster <- modelo_k_medias |> 
  augment(dados_k_medias)


# Exemplo 2: ---------------------------------------------------------------
# Explicando o resultado

# Dados Iris
data("iris")

# Objetivo: Achar padrão nos dados. Existe grupos mais parecidos entre si
# no que se refere as pétalas e sépalas? 
# Para investigar, vamos usar o dados iris para simular esse objetivo, por isso
# vamos excluir as espécies, pq essa separação já é nosso resultado, então vamos 
# imaginar que não temos essa informação;

# 1. Ajeitar dados - o algoritmo espera receber apenas numeros
dados_modelo <- iris |> 
  select(-Species)

# 2. Modelar - kmeans () : aceita somente números na planilha. 
# No item "centers" colocamos a quantidade de grupos que queremos identificar.

modelo_k_medias <- kmeans(dados_modelo, centers = 3)
modelo_k_medias

# 3. Resultados: 

# K-means clustering with 3 clusters of sizes 38, 62, 50

#> SIGNIFICADO:
#> NOS DÁ QUANTIDADE DE GRUPOS (3) E TAMANHO DE CADA GRUPO IDENTIFICADO (38, 62, 50)

# Cluster means:
#   Sepal.Length Sepal.Width Petal.Length Petal.Width
# 1     6.850000    3.073684     5.742105    2.071053
# 2     5.901613    2.748387     4.393548    1.433871
# 3     5.006000    3.428000     1.462000    0.246000

#> SIGNIFICADO:
#> São as distancias obtidas.

# Clustering vector:
# [1] 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 
#     3 3 3 3 3 3 3 3 3 3 3 3
# [49] 3 3 2 2 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 2 2 2 2 2 2 
#      2 2 2 2 2 2 2 2 2 2 2 2
# [97] 2 2 2 2 1 2 1 1 1 1 2 1 1 1 1 1 1 2 2 1 1 1 1 2 1 2 1 2 1 1 2 2 1 1 1 1 
#      1 2 1 1 1 1 2 1 1 1 2 1
# [145] 1 1 2 1 1 2

#> SIGNIFICADO:
#> NOS INFORMA QUAL OBSERVAÇÃO PERTENCE A CADA GRUPO.

# 4. Analisar os resultados:
# Incluir na base de dados os resultados do modelo
# augment(): 

base_final <- modelo_k_medias |> 
  broom::augment(iris)

# # A tibble: 150 x 6
# Sepal.Length Sepal.Width Petal.Length Petal.Width Species .cluster
# <dbl>       <dbl>        <dbl>       <dbl> <fct>   <fct>   
#   1          5.1         3.5          1.4         0.2 setosa  3      
#   2          4.9         3            1.4         0.2 setosa  3       
#   3          4.7         3.2          1.3         0.2 setosa  3       

# 5. Grafico 
p1 <- base_final |> 
  ggplot(aes(x= Petal.Length, y = Petal.Width, color = .cluster))+
  geom_point()

#> Nesses resultados podemos ver a formação de 3 grupos. Se plotarmos as spp
#> que foram excluídas da análise, veremos que o cluster praticamente separou 
#> as spp, quando formou esse agrupamento.

# Vamos olhar os grupos originais dos dados
p2 <- iris |> 
  ggplot(aes(x= Petal.Length, y = Petal.Width, color = Species))+
  geom_point()

# juntando gráficos no mesmo grid
gridExtra::grid.arrange(p1, p2, ncol=2)



# xxxxxxxxxxxxxxxxxxxxxx --------------------------------------------------


# ERROS COMUNS QUE PODEM ACONTECER COM K-MEANS----------------------------------------
# 1. Distancia euclidiana simples pode não ser a melhor
# 2. Variáveis com escalas diferentes pode influenciar nos resultados
# 3. Os dados não são adequados para encontrar grupos



# Dados não adequados -----------------------------------------------------
# 3. Dados não adequados para encontrar grupos
# Simular dados com um grupo no círculo e outro no anel
dados_circulo <- tibble(
  X = runif(5000, -1, 1),
  Y = runif(5000, -1, 1)
) |>
  filter(X^2 + Y^2 <= 0.2 | (X^2 + Y^2 <= 0.8 & X^2 + Y^2 >= 0.6))
qplot(dados_circulo$X, dados_circulo$Y)

# Podemos ver que todos os angulos são abertos, nunca chega uma hora com 
# angulo próximo a 90º para poder parar de clusterizar. 
fviz_nbclust(dados_circulo, kmeans, method = "wss")
fviz_nbclust(dados_circulo, hcut, method = "wss", hc_method = "single")

# Rodar o cluster hierárquico hcut() funciona como o hclust() só que já corta os 
# grupos
modelo_cluster_h = hcut(dados_circulo, k = 2, hc_method = "single")  

# Olhar como método hierarquico separou os dados
dados_com_clusters_h <- dados_circulo |>
  mutate(
    .cluster = factor(modelo_cluster_h$cluster)
  )

dados_com_clusters_h |>
  ggplot(aes(x = X, y = Y, color = .cluster)) +
  geom_point()

# Olhar como método centroide (kmeans) separou os dados
dados_com_clusters_k <- dados_circulo |>
  mutate(
    .cluster = factor(kmeans(dados_circulo, centers = 2)$cluster)
  )
dados_com_clusters_k |>
  ggplot(aes(x = X, y = Y, color = .cluster)) +
  geom_point()

#> Interpretação: podemos observar que o hierarquico (dados_com_clusters_h ) ao 
#> mudar o método para "single" ela consegue identificar os grupos adequadamente
#> enquanto o K-means, ele não consegue identificar o grupo (circulo e anel). ele
#> acaba sempre separando o anel em dois e o circulo em 2 tb. Esse método não funciona
#> bem nessa situação, pq tem como pressuposto que os cluster q se quer achar, são 
#> contínuos.


# Padronizar --------------------------------------------------------------
# Variáveis com escalas diferentes pode influenciar nos resultados, tanto no 
# metodo hierarquico, quanto no metodo centroide (k-means).
# Quando padronizamos garantimos que todas as variáveis tem a mesma média (0)
# e o mesmo desvio-padrão (1). Isso quer dizer que a importancia das variáveis
# serão as mesma, naõ vai ter uma variável mais importante do que a outra por 
# conta da escala.

# Se quiser dar mais peso para alguma variável. Podemos multiplicar por 10, por 
# qualquer número e dar mais peso para aquela variável. Se queremos encontrar
# grupos honestos, padronizamos.

# Função para padronizar
padroniza <- function(x){(x-mean(x))/sd(x)}

# Alterando as colunas originais para a versão padronizada
dados_k_medias_padr <- dados_k_medias |>
  mutate(
    Sepal.Length = padroniza(Sepal.Length),
    Sepal.Width = padroniza(Sepal.Width),
    Petal.Length = padroniza(Petal.Length),
    Petal.Width = padroniza(Petal.Width))

# sem padronizar
dados_k_medias |>
  ggplot(aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_point()

# padronizando
dados_k_medias_padr |>
  ggplot(aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_point()

#> Interpretação: são bem parecidos os gráficos para quando as variaveis tem a 
#> mesma escala ou parecidas.

#vamos tentar com outros dados
USArrests_padr <- USArrests |>
  mutate(
    Murder = padroniza(Murder),
    Assault = padroniza(Assault),
    UrbanPop = padroniza(UrbanPop),
    Rape = padroniza(Rape)
  )

# sem padronizar
USArrests |>
  ggplot(aes(x = Rape, y = Assault)) +
  geom_point()

# padronizando
USArrests_padr |>
  ggplot(aes(x = Rape, y = Assault)) +
  geom_point()

# clusters
fviz_nbclust(dados_k_medias, kmeans, method = "wss")
fviz_nbclust(dados_k_medias_padr, kmeans, method = "wss")

fviz_nbclust(USArrests, kmeans, method = "wss")
fviz_nbclust(USArrests_padr, kmeans, method = "wss")



# Seleção de numero de clusters -------------------------------------------
# Outros métodos de seleção mais recentes


# xxxxxxxxxxxxxxxxx
# MÉTODO "gap_stat"
# No método anterior "WSS" usávamos a soma dos quadrados para verificar se os
# grupos eram diferentes entre si. Quanto maior, mais os grupos se separam.

# Já o método "gap_stat" funciona diferente. Quanto maior o numero, significa
# que tem muito espaço entre os clusters. E quando menor, significa que tem 
# pouco espaço entre os cluster.

# Faz um boostrap para fazer um intervalo de confiança e marca no gráfico
# a primeira vez em que não teve uma diferença significativa para separar os
# grupos. Assim, informa a quantidade de grupos encontrados.

# Com esse método, fazemos um teste de hipótese. Esse é um jeito menos 
# subjetivo. Para não ter que fazer inspeção visual.

# encontra 4 grupos
fviz_nbclust(dados_k_medias, kmeans, method = "gap_stat")

# ao padronizar os dados, encontra 2 grupos
fviz_nbclust(dados_k_medias_padr, kmeans, method = "gap_stat")

# Com esse método, inclusive, podemos ser informado de que é melhor não clusterizar
# p.ex. nesse resultado abaixo a quebra acontece em 1 grupo. ou seja, todos os
# dados fazem parte do mesmo grupo. Não tem pq seprarar grupos.
fviz_nbclust(dados_circulo, kmeans, method = "gap_stat")

# Aqui encontra dois grupos
fviz_nbclust(dados_circulo, hcut, method = "gap_stat", hc_method = "single")
modelo_cluster_h = hcut(dados_circulo, k = 2, hc_method = "single")
dados_com_clusters_h <- dados_circulo |>
  mutate(
    .cluster = factor(modelo_cluster_h$cluster))
dados_com_clusters_h |>
  ggplot(aes(x = X, y = Y, color = .cluster)) +
  geom_point()

#> MAIS RECOMENDADO ! Se for usar um método. Escolha esse !!!!!!!!!!!!!!!!!


# xxxxxxxxxxxxxxxxx
# MÉTODO Silhueta 

# Objetivo: 
# Aqui estamos procurando o pico.Porque queremos ficar o mais próximo possível
# de 1. Nesse local, os pontos não querem trocar para o outro grupo. outros cluster.

# method: "sil"
fviz_nbclust(dados_k_medias, hcut, method = "sil", hc_method = "single")
fviz_nbclust(dados_k_medias_padr, hcut, method = "sil", hc_method = "single")




# Importância da Variável  -------------------------------------------
# Esta etapa pode ser feita com a saída tanto do hclust quanto do kmeans

# #> Fazer um método supervisionado
library(randomForest)
glm_importancia_das_variaveis <- dados_com_cluster |>
  randomForest(.cluster ~ ., data = _)

# #> Função: importance () e varImpPlot ()
# Maior o número, maior importancia da variavel
importance(glm_importancia_das_variaveis)
varImpPlot(glm_importancia_das_variaveis)

#> Atenção: se tiver variável correlacionada, elas podem estar dividindo 
#> importancia


# A mesma coisa com o hclust
glm_importancia_das_variaveis <- dados_com_cluster |> 
  mutate(.cluster = factor(hcut(dados_k_medias, k=3)$cluster)) |> 
  randomForest(.cluster ~ ., data = _)
varImpPlot(glm_importancia_das_variaveis)


# Mostrar a importancia de padronizar

# sem padronizar: assault foi mais importante
glm_importancia_das_variaveis <- USArrests |> 
  mutate(.cluster = factor(hcut(USArrests, k=2)$cluster)) |> 
  randomForest(.cluster ~ ., data = _)
varImpPlot(glm_importancia_das_variaveis)

# padronizado: assault continuo sendo mais importante, mas a importancia das 
# outras variaveis aumentou mto
glm_importancia_das_variaveis <- USArrests_padr |> 
  mutate(.cluster = factor(hcut(USArrests_padr, k=2)$cluster)) |> 
  randomForest(.cluster ~ ., data = _)
varImpPlot(glm_importancia_das_variaveis)








# XXX----------------------------------------------------------------------
# XXX----------------------------------------------------------------------
# #XXX---  CURSO USP ESALQ-------------------------------------------------

# EXEMPLO DE APLICAÇÃO

## Análise de Clusters
## Fonte: Fávero e Belfiore, MANUAL DE ANÁLISE DE DADOS, Capítulo 09


# Pacotes -----------------------------------------------------------------
# Instalação e carregamento dos pacotes utilizados

pacotes <- c("plotly", #plataforma gráfica
             "tidyverse", #carregar outros pacotes do R
             "ggrepel", #geoms de texto e rótulo para 'ggplot2' que ajudam a
             #evitar sobreposição de textos
             "knitr", "kableExtra", #formatação de tabelas
             "reshape2", #função 'melt'
             "misc3d", #gráficos 3D
             "plot3D", #gráficos 3D
             "cluster", #função 'agnes' para elaboração de clusters hierárquicos
             "factoextra", #função 'fviz_dend' para construção de dendrogramas
             "ade4") #função 'ade4' para matriz de distâncias em var. binárias

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}


# Pacotes
library(plotly)
library(tidyverse)
library(ggrepel)  
library(kableExtra)  
library(knitr)  
library(reshape2)  
library(misc3d)  
library(plot3D)  
library(cluster)  
library(factoextra)  
library(ade4)  

# Dados -------------------------------------------------------------------
load(file = "Vestibular.RData")


# Exploratorio ------------------------------------------------------------
# Visualização da base de dados
Vestibular %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE,
                font_size = 20)


# Gráfico 3D --------------------------------------------------------------
rownames(Vestibular) <- Vestibular$estudante
scatter3D(x=Vestibular$fisica,
          y=Vestibular$matematica,
          z=Vestibular$quimica,
          phi = 0, bty = "g", pch = 20, cex = 2,
          xlab = "Fisica",
          ylab = "Matematica",
          zlab = "Quimica",
          main = "Vestibular",
          clab = "Nota de Quimica")>
  text3D(x=Vestibular$fisica,
         y=Vestibular$matematica,
         z=Vestibular$quimica,
         labels = rownames(Vestibular),
         add = TRUE, cex = 1)


# Estatistica Descritiva --------------------------------------------------
summary(Vestibular)

# Boxplots por variável
ggplotly(
  Vestibular %>%
    melt() %>%
    ggplot(aes(label = estudante)) +
    geom_boxplot(aes(x = variable, y = value, fill = variable)) +
    geom_point(aes(x = variable, y = value), alpha = 0.5) +
    labs(x = "Variável",
         y = "Nota") +
    scale_fill_manual("Legenda:",
                      values = c("orange", "purple", "bisque4")) +
    theme_bw()
)

# Padronização ------------------------------------------------------------
## Como as variáveis estão na mesma unidade de medida, não vamos padronizar
# Se for necessário padronizar, é possível utilizar a função scale()
vest_padronizado <- as.data.frame(scale(Vestibular[,2:4]))
rownames(vest_padronizado) <- Vestibular$estudante



# -------------------------------------------------------------------------
# XXX--- Esquema de aglomeração hierárquico ---------------------------------



# Matriz de Dissimilaridade -----------------------------------------------
matriz_D <- Vestibular %>% 
  select(matematica, fisica, quimica) %>% 
  dist(method = "euclidean")


# Escolha do Método -------------------------------------------------------
# Method: parametrização da distância a ser utilizada

## "euclidean": distância euclidiana
## "euclidiana quadrática": elevar ao quadrado matriz_D (matriz_D^2)
## "maximum": distância de Chebychev;
## "manhattan": distância de Manhattan (ou distância absoluta ou bloco);
## "canberra": distância de Canberra;
## "minkowski": distância de Minkowski


# Visualização da Matriz de Dissimilaridade -------------------------------
data.matrix(matriz_D) %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE, 
                font_size = 20)


# Elaborar Cluster Hierarquico --------------------------------------------
# O input é a matriz de distâncias obtida anteriormente
# Method é o tipo de encadeamento:

## "complete": encadeamento completo (furthest neighbor ou complete linkage)
## "single": encadeamento único (nearest neighbor ou single linkage)
## "average": encadeamento médio (between groups ou average linkage)

# Definição do esquema hierárquico de aglomeração
cluster_hier <- agnes(x = matriz_D, method = "single")


# As distâncias que ocorre a junção dos grupos
coeficientes <- sort(cluster_hier$height, decreasing = FALSE) 
coeficientes
# [1] 3.713489 4.170132 6.044832 7.186793


# Tabela com o esquema de aglomeração. 

# Interpretação do output:
## As linhas são os estágios de aglomeração
## Nas colunas Cluster1 e Cluster2, observa-se como ocorreu a junção
## Quando for número negativo, indica observação isolada (duas obs. se juntando)
## Quando for número positivo, indica cluster formado anteriormente (estágio)
## Coeficientes: as distâncias para as combinações em cada estágio
## Então -1 (obs1) -2(obs2) -3(obs 3)

## Então, linha 1: (cluster formado entre a obs.1 (-1) e obs 4 (-4) forma
# o primeiro grupo. 
# linha 2: o cluster 1 (formado anteriormente pela obs 1 e 4) se junta com a
# obs 5 (-5). Assim por diante.
## Com os coeficientes podemos ver a posição que aconteceu essas junções
esquema <- as.data.frame(cbind(cluster_hier$merge, coeficientes))
names(esquema) <- c("Cluster1", "Cluster2", "Coeficientes")
esquema
# Cluster1 Cluster2 Coeficientes
# 1       -1       -4     3.713489   
# 2        1       -5     4.170132
# 3        2       -3     6.044832
# 4        3       -2     7.186793



# Visualização do Esquema Hierárquico de Aglomeração ---------------------------
esquema %>%
  kable(row.names = T) %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE, 
                font_size = 20)


# Construção do Dendograma ------------------------------------------------
dev.off()
fviz_dend(x = cluster_hier)


# Dendograma com visualização dos clusters -------------------------------------
# Definição de 3 clusters
fviz_dend(x = cluster_hier,
          k = 3,
          k_colors = c("deeppink4", "darkviolet", "deeppink"),
          color_labels_by_k = F,
          rect = T,
          rect_fill = T,
          lwd = 1,
          ggtheme = theme_bw())


# Criando Variável Categorica ---------------------------------------------
# Criando variável categórica para indicação do cluster no banco de dados
## O argumento 'k' indica a quantidade de clusters
Vestibular$cluster_H <- factor(cutree(tree = cluster_hier, k = 3))

# Visualização da base de dados com a alocação das observações nos clusters
Vestibular %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE,
                font_size = 20)


# Estatisticas descritivas dos clusters por variável  ---------------------
## ATENÇÃO: Clusters 2 e 3 têm somente uma observação, não calcula 'sd'

# Estatísticas descritivas da variável 'matematica'
group_by(Vestibular, cluster_H) %>%
  summarise(
    mean = mean(matematica, na.rm = TRUE),
    sd = sd(matematica, na.rm = TRUE),
    min = min(matematica, na.rm = TRUE),
    max = max(matematica, na.rm = TRUE))

# Estatísticas descritivas da variável 'fisica'
group_by(Vestibular, cluster_H) %>%
  summarise(
    mean = mean(fisica, na.rm = TRUE),
    sd = sd(fisica, na.rm = TRUE),
    min = min(fisica, na.rm = TRUE),
    max = max(fisica, na.rm = TRUE))

# Estatísticas descritivas da variável 'quimica'
group_by(Vestibular, cluster_H) %>%
  summarise(
    mean = mean(quimica, na.rm = TRUE),
    sd = sd(quimica, na.rm = TRUE),
    min = min(quimica, na.rm = TRUE),
    max = max(quimica, na.rm = TRUE))


# Anova -------------------------------------------------------------------
# Análise de variância de um fator (ANOVA). Anova é utilizada para ver
# qual das variáveis mais contribuem para a separação dos grupos do cluster


# Interpretação do output:
## Mean Sq do cluster_H: indica a variabilidade entre grupos
## Mean Sq dos Residuals: indica a variabilidade dentro dos grupos
## F value: estatística de teste (Sum Sq do cluster_H / Sum Sq dos Residuals)
## Pr(>F): p-valor da estatística 
## p-valor < 0.05: pelo menos um cluster apresenta média estatisticamente diferente dos demais
## A variável mais discriminante dos grupos contém maior estatística F (e significativa)

# ANOVA da variável 'matematica'
# Ho: a variável matemática auxiliou na separação dos cluster
# Se p < 0.05 - rejeita Ho
summary(anova_matematica <- aov(formula = matematica ~ cluster_H,
                                data = Vestibular))

# ANOVA da variável 'fisica'
# Ho: a variável fisica auxiliou na separação dos cluster
# Se p < 0.05 - rejeita Ho
summary(anova_fisica <- aov(formula = fisica ~ cluster_H,
                            data = Vestibular))

# ANOVA da variável 'quimica'
# Ho: a variável quimica auxiliou na separação dos cluster
# Se p < 0.05 - rejeita Ho
summary(anova_quimica <- aov(formula = quimica ~ cluster_H,
                             data = Vestibular))



# -------------------------------------------------------------------------
# XXX--- Esquema de aglomeração não hierárquico K-MEANS ---------------------

# Elaboração da clusterização não hieráquica k-means
## centers: parametrização da quantidade de clusters
cluster_kmeans <- kmeans(Vestibular[,2:4],
                         centers = 3)


# Definir número de cluster -----------------------------------------------
# Método de Elbow (método do cotovelo) para identificação do número ótimo de clusters
## Apresenta a variação total dentro dos clusters para várias nº de clusters
## Em geral, quando há a dobra é um indício do número ótimo de clusters
fviz_nbclust(Vestibular[,2:4], kmeans, method = "wss", k.max = 4)
fviz_nbclust(Vestibular[,2:4], hcut, method = "wss", k.max = 4)

fviz_nbclust(Vestibular[,2:4], kmeans, method = "gap_stat", k.max = 4)
fviz_nbclust(Vestibular[,2:4], hcut, method = "gap_stat", k.max = 4)


# Criando variável categórica para indicação do cluster no banco de dados
Vestibular$cluster_K <- factor(cluster_kmeans$cluster)

# Visualização da base de dados
Vestibular %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE,
                font_size = 20)


# Anova -------------------------------------------------------------------
# Análise de variância de um fator (ANOVA)
# Avalia a importancia das variáveis
# No caso de todas as variáveis serem importantes, podemos avaliar a magnitude
# através do valor da estatística F. Quanto maior o valor da estatística F
# maior a contribuição da variável para a separação dos grupos.

# ANOVA da variável 'matematica'

summary(anova_matematica <- aov(formula = matematica ~ cluster_K,
                                data = Vestibular))

# ANOVA da variável 'fisica'
summary(anova_fisica <- aov(formula = fisica ~ cluster_K,
                            data = Vestibular))

# ANOVA da variável 'quimica'
summary(anova_quimica <- aov(formula = quimica ~ cluster_K,
                             data = Vestibular))

# Comparando os resultados dos esquemas hierárquico e não hierárquico
Vestibular %>%
  select(estudante, cluster_H, cluster_K) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = FALSE,
                font_size = 20)



# Esquema de aglomeração não hierárquico K-MEANS -------------------------------
# Exemplo 2 


# Dados -------------------------------------------------------------------
library(readr)
paises <- read_csv("raw_datas_exemplos/dados_paises.csv")
View(dados_paises)


# Estatísticas descritivas -----------------------------------------------
summary(paises)


# Padronizando ------------------------------------------------------------
# Padronizando as variáveis
pais_padronizado <- as.data.frame(scale(paises[,2:10]))
rownames(pais_padronizado) <- paises$country

## Todas as variáveis passam a ter média = 0 e desvio padrão = 1. 
## Por exemplo:
round(mean(pais_padronizado$exports),3)
round(sd(pais_padronizado$exports))

round(mean(pais_padronizado$gdpp),3)
round(sd(pais_padronizado$gdpp))


# Esquema de Aglomeração Hierarquico --------------------------------------

# 1. Matriz de dissimilaridades
matriz_D <- pais_padronizado %>% 
  dist(method = "euclidean")

# 1º Teste: Elaboração da clusterização hierárquica como "single linkage"
# Não ficou bom, os grupos são muito muito próximas umas das outras
cluster_hier_single <- agnes(x = matriz_D, method = "single")
# Dendrograma "single linkage"
dev.off()
fviz_dend(x = cluster_hier_single, show_labels = F)

# 2º Teste: Elaboração da clusterização hierárquica como "complete linkage"
## O método de encadeamento complete linkage melhora significativamente
cluster_hier_complete <- agnes(x = matriz_D, method = "complete")
# Dendrograma "complete linkage"
fviz_dend(x = cluster_hier_complete, show_labels = F)

# 3º Teste: Elaboração da clusterização hierárquica como "average linkage"
cluster_hier_average <- agnes(x = matriz_D, method = "average")
# Dendrograma "average linkage"
fviz_dend(x = cluster_hier_average, show_labels = F)

## Vamos optar pelo complete linkage (average cria clusters com menos observações)

# Dendrograma com visualização dos clusters (selecionando por "altura")
# h= 5.5 : seleciona a altura de corte
fviz_dend(x = cluster_hier_complete,
          h = 5.5,
          color_labels_by_k = F,
          rect = T,
          rect_fill = T,
          rect_border = "black",
          lwd = 1,
          show_labels = F,
          ggtheme = theme_bw())

# k = 5: escolhe o número de cluster na hora de cortar (substitui a altura)
fviz_dend(x = cluster_hier_complete,
          k = 5,
          color_labels_by_k = F,
          rect = T,
          rect_fill = T,
          rect_border = "black",
          lwd = 1,
          show_labels = F,
          ggtheme = theme_bw())


# Vamos detalhar esse esquema hierárquico
coeficientes <- sort(cluster_hier_complete$height, decreasing = FALSE) 
esquema <- as.data.frame(cbind(cluster_hier_complete$merge, coeficientes))
names(esquema) <- c("Cluster1", "Cluster2", "Coeficientes")
esquema

## Portanto, vamos gerar uma variável indicando 12 clusters
paises$cluster_H <- factor(cutree(tree = cluster_hier_complete, k = 12))
pais_padronizado$cluster_H <- factor(cutree(tree = cluster_hier_complete, k = 12))

# A seguir, vamos verificar se todas as variáveis ajudam na formação dos grupos
summary(anova_child_mort <- aov(formula = child_mort ~ cluster_H,
                                data = pais_padronizado))

summary(anova_exports <- aov(formula = exports ~ cluster_H,
                             data = pais_padronizado))

summary(anova_health <- aov(formula = health ~ cluster_H,
                            data = pais_padronizado))

summary(anova_imports <- aov(formula = imports ~ cluster_H,
                             data = pais_padronizado))

summary(anova_income <- aov(formula = income ~ cluster_H,
                            data = pais_padronizado))

summary(anova_inflation <- aov(formula = inflation ~ cluster_H,
                               data = pais_padronizado))

summary(anova_lifeexpec <- aov(formula = life_expec ~ cluster_H,
                               data = pais_padronizado))

summary(anova_totalfer <- aov(formula = total_fer ~ cluster_H,
                              data = pais_padronizado))

summary(anova_gdpp <- aov(formula = gdpp ~ cluster_H,
                          data = pais_padronizado))

## Conlcusão da Anova: Todas auxiliam na formação de pelo menos um cluster

# O que os cluster indicam? Vamos interpretar algumas variáveis médias:

análise <- group_by(paises, cluster_H) %>%
  summarise(income = mean(income, na.rm = TRUE),
            gdpp = mean(gdpp, na.rm = TRUE),
            mort = mean(child_mort, na.rm = TRUE),
            health = mean(health, na.rm = TRUE),
            expec = mean(life_expec, na.rm = TRUE))

## Por exemplo: os países do cluster 1 e 4 apresentam: 
## Baixa renda média, baixo PIB per capita, 
## Elevada mortalidade infantil, baixa expectativa de vida
## Portanto, são os países em que deve haver ajuda para melhoria das condições





# Exemplo 3 ---------------------------------------------------------------
# Aplicação em variáveis binárias

# Cluster pode ser aplicado em variáveis binárias, mas antes precisamos
# estabelecer a medida de emparelhamento simples, que é uma medida de 
# semelhança. A única coisa que muda dos anteriores para esse é a medida
# de distância.

# dist.binary( ): computa as matrizes de distancia para dados binários
# method = 1: Jaccard Index
# method = 2: Método de emparelhamento simples
# (...)


# Dados -------------------------------------------------------------------
load(file = "Pesquisa Binária.RData")

## Contexto: são respostas binárias para 50 perguntas de 35 respondentes
## Os respondentes são gestores de empresas em 3 setores distintos

# Visualização da base de dados
View(PesquisaBinária)

# Contagem das categorias por variável
vetor_var <- names(PesquisaBinária)
map(PesquisaBinária[vetor_var], ~ summary(as.factor(.)))

#---------- Esquema de aglomeração hierárquico ---------------------------------

# Matriz de dissimilaridades
## Em 'dist.binary', method = 2 indica similaridade por emparelhamento simples
matriz_D <- PesquisaBinária %>% 
  select(-setor) %>% 
  dist.binary(method = 2)


# Visualizando a matriz de dissimilaridades
data.matrix(matriz_D) %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE, 
                font_size = 20)

# Elaboração da clusterização hierárquica
cluster_hier <- agnes(x = matriz_D, method = "average")

# Outras opções de encadeamento:

## "complete": encadeamento completo (furthest neighbor ou complete linkage)
## "single": encadeamento único (nearest neighbor ou single linkage)
## "average": encadeamento médio (between groups ou average linkage)

# Definição do esquema hierárquico de aglomeração

# As distâncias para as combinações em cada estágio
coeficientes <- sort(cluster_hier$height, decreasing = FALSE) 
coeficientes

# Tabela com o esquema de aglomeração. Interpretação do output:

## As linhas são os estágios de aglomeração
## Nas colunas Cluster1 e Cluster2, observa-se como ocorreu a junção
## Quando for número negativo, indica observação isolada
## Quando for número positivo, indica cluster formado anteriormente (estágio)
## Coeficientes: as distâncias para as combinações em cada estágio

esquema <- as.data.frame(cbind(cluster_hier$merge, coeficientes))
names(esquema) <- c("Cluster1", "Cluster2", "Coeficientes")
esquema

# Visualização do esquema hierárquico de aglomeração
esquema %>%
  kable(row.names = T) %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE, 
                font_size = 20)

# Construção do dendrograma
dev.off()
fviz_dend(x = cluster_hier)

# Dendrograma com visualização dos clusters
# Parametrizando 3 clusters para comparar com setores
fviz_dend(x = cluster_hier,
          k = 3,
          k_colors = c("deeppink4", "darkviolet", "deeppink"),
          color_labels_by_k = F,
          rect = T,
          rect_fill = T,
          lwd = 1,
          ggtheme = theme_bw())

# Criando variável categórica para indicação do cluster no banco de dados
## O argumento 'k' indica a quantidade de clusters
PesquisaBinária$cluster_H <- factor(cutree(tree = cluster_hier, k = 3))

# Visualização da base de dados com a alocação das observações nos clusters
PesquisaBinária %>%
  select(setor, cluster_H) %>%
  arrange(setor) %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE,
                font_size = 20)

