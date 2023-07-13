
# SNE - Stochastic Neighbor Embedding
# novembro 2022
# Natália Freitas


# # A PCA usa um processo de linearização para reduzir a dimensionalidade e 
# achar o Y e A que melhor estima o X. (X = Y * A + mu). Ou seja, a 
# a apromiximação busca icognitas para plugar em uma equação que faz soma
# e multiplicação. Isso é bom em varios contexto. Mas as vezes não funciona.
# Alternativa para isso são os métodos Stochastic Neighbor Embedding (SNE)


# Métodos SNE
# As técnicas da família SNE buscam encontrar o que se chama de Stochastic
# Neighbor Embedding, que essencialmente parte da seguinte ideia:

# É útil imaginar que os pontos são posicionados de acordo com um sorteio:
# sorteia x1 -> sorteia x2 a partir de x1-> sorteia x3 a partir de x2

# A probabilidade de encontrarmos um x2 a partir de um x1 e de x3 a partir 
# de x2 ocorre através da distancia entre os pontos.
# Também existe a chance de um ponto ser muito distante dos anteriores,
# e esse número é chamado de "perplexidade"

# OBJETIVO: A ideia por tras do tSNE é procurar por uma distribuição que leve
# xj a xi em dimensão baixa que produza uma matriz de distancia similar a 
# matriz de distancias de dimensão alta.

# Quando a perplexidade é baixa. Os pontos estão pertos.
# Quando perplexidad é alta. Temos alguns pontos pertos, mas temos alguns 
# longes.


# Pacotes -----------------------------------------------------------------

library(tsne)
library(ggplot2)

# Dados -------------------------------------------------------------------


multishapes <- factoextra::multishapes |>
  tibble()

multishapes |>
  ggplot(aes(x = x, y = y, color = factor(shape))) +
  geom_point()


# Cluster: kmeans
# Interpretação: kmeans não consegue identificar direito os grupos. 
# o anel ele considera tudo uma coisa só.
multishapes |>
  select(-3) |>
  kmeans(centers = 3) |>
  fviz_cluster(data = multishapes[,-3])

# Cluster: hcut
# Interpretação: também não encontra os grupos
multishapes |>
  select(-3) |>
  hcut(k = 5, hc_method = "single") |>
  fviz_cluster(data = multishapes[,-3])

multishapes |>
  select(-3) |>
  hcut(k = 5) |>
  fviz_cluster(data = multishapes[,-3])



# tSNE --------------------------------------------------------------------

# tSNE: funciona parecido com a PCA. Vai reduzir a dimensionalidade
# k = : temos que informar o número de grupos que queremos
# perplexity: temos que informar a perplexidade. Em geral, de 10 ate 100.
#             teremos que tunar, testar vários números para ver qual minimiza
#             o erro.

reducao_sne <- multishapes[,-3] |>
  tsne(k = 2, perplexity = 50)


# Depois de usar o tSNE, ele monta um grupo que reproduz 
# a matriz de distancia original que foi passada para ele. Então, ele tenta
# procurar qual é o melhor jeito de gerar esses pontos todos, que seja de acordo 
# com as regras de sorteio, com a perplexidade (que diz os saltos que vai dá).
# Então, encontra nos desenhos, no sorteio o que melhor representa a distancia
# par a par original.
# Cada vez que rodamos dá uma coisa diferente, pois ele faz sorteios.
reduzido = reducao_sne |>
  as_tibble() |>
  set_names(c("x", "y")) |>
  mutate(shape = multishapes$shape)

reduzido |>
  ggplot(aes(x = x, y = y, color = factor(shape))) +
  geom_point()


# Depois que fazemos o tSNE podemos usar o cluster para definir os grupos.
# Neste caso, até o kmeans consegue separar os dados agora. 

reduzido[,-3] |>
  #kmeans(centers = 5) |>
  hcut(k = 5, hc_method = "single") |>
  fviz_cluster(data = reduzido[,-3])


#> Método muito flexível. É muito utilizado para imagens. Ele resolve métodos
#> mais complicados.