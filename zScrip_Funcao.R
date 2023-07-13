
# Natália Freitas
# Objetivo: Criar funções



# Contextualização --------------------------------------------------------
# Toda função no R tem 3 partes:
#  1. Nome;
#  2. Código
#  3. Conjunto de argumentos



# Estrutura ---------------------------------------------------------------
# my_function <- function() {}




# Criar a função ----------------------------------------------------------


# Objetivo: Sortear dois números; Somar os dois números
roll <- function() {
  die <- 1:6
  dice <- sample(die, size = 2, replace = TRUE)
  sum(dice)
}


# Rodar a função ----------------------------------------------------------
roll()

# Abrir o código da função ------------------------------------------------
roll


# Argumentos na função ----------------------------------------------------
roll2 <- function(bones) {
            dice <- sample(bones, size = 2, replace = TRUE)
                    sum(dice)
                            }


# Rodar a função com argumento --------------------------------------------
# Agora para rodar a função é necessário fornecer o argumento necessario.

roll2(bones = 1:10)
# ou

vetor <- 1:10
roll2(bones = vetor)



# Argumento com Default ---------------------------------------------------
# Se a função tem argumento, caso não coloque ela não irá funcionar. Para 
# prevenir erros, podemos deixar um default programado para o argumento. Para isso
# basta no momento de criar a função deixar o argumento com algum valor.

# Reescrevendo a função anterior com bones como default de 1:6
roll2 <- function(bones = 1:6) {
  dice <- sample(bones, size = 2, replace = TRUE)
  sum(dice)
}

# agora podemos rodar fornecendo o armento
roll2(bones = 10:20)

# ou assumindo o padrão
roll2()



# Função: Gráfico com Desvio padrão ---------------------------------------

# Para usar esta função é necessário carregar o pacote
library(sciplot)

# Objetivo: criar gráfico de média com desvio padrão. Usa como base uma 
# função do pacote "sciplot" não esquecer de carregar o pacote. 
# x é a variável categórica
# y é a variável quantitativa
# nome.do.x é o nome que você deseja para o eixo x
# nome.do.y é o nome que você deseja para o eixo y
graf.media.desvio= function(x, y, nome.do.x, nome.do.y){
  lineplot.CI(x, y, las=1, xlab=nome.do.x, ylab=nome.do.y, type="p",
              ci.fun= function(x) c(mean(x)-sd(x), mean(x)+sd(x)))
}

# Como usar
# ex:
graf.media.desvio (Ambiente, Riqueza, nome.do.x = "Ambiente", nome.do.y = "Riqueza")


# Como usar uma função criada? ---------------------------------------------------------------
# Para usar a função é necessário que o scrip dela esteja dentro
# do diretório de trabalho

# Arquivos do Diretório
dir()

# Executar a função
source("graf_media_desv.R")

# conferir o código função
graf.media.desvio

#Após carregar, Já podemos executar
graf.media.desvio (Ambiente, Riqueza, "Ambiente", "Riqueza")

