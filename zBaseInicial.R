
# NATÁLIA FREITAS
# Setembro 2022

# Criar Projeto -----------------------------------------------------------
# Canto superior direito - Interessante para separar por artigo


# Criar Hiperlink com Sumário ---------------------------------------------
# CTRL+SHIFT+R


# Instalar Pacotes --------------------------------------------------------
# Instalar pacotes - marcar a caixa "install dependences"
# Todas as dependencias são instaladas - consegue usar as funções na totalidade
install.packages("nome", dependencies = T)    
   

# Ajuda -------------------------------------------------------------------
help("nome")
?nome 

# Ajuda: Atalho  -------------------------------------------------------
# Apertar F1 com cursor em cima da função
  
# Listar Pacotes ----------------------------------------------------------
# Ajuda para listar todos os pacotes que fazem a função
??lm 

# Listar: Objetos Gravados na Memória --------------------------------------
# Listar o que está no Environment
ls()

# Listar: Argumentos da função --------------------------------------------
args(nomedafuncao)


# Atualizar o R -----------------------------------------------------------
library(installr)
updateR()
 

# Limpar Ambiente ---------------------------------------------------------
# Remover todos os objetos que estão no ambiente (environment)
rm(list = ls(all=T))
  


#__________________________________________________________________________
# Criar: Vetores ----------------------------------------------------------
# Pode ser criado pelas funções:
c(21,23,34,"maria", 1:6)
seq(from = 1, to =10, by = 2)
rep()

# ou: vetor() e digita asa posições
vetor <- vector(length = 8)
vetor[1] <- 59.5
vetor[2] <- 60
vetor[3] <- 53.5
vetor[4] <- 55
vetor[5] <- 52.5
vetor[6] <- 57.5
vetor[7] <- 53
vetor[8] <- 55
vetor


# Vetor: Comprimento ------------------------------------------------------
length(vetor)


#__________________________________________________________________________
# Tipo de Dados: Doubles ----------------------------------------------------------
# A double é um vetor regular de números. Os números podem ser positivos ou negativos
# grande ou pequeno, ter digitos decimais ou não.
die <- c(1, 2, 3, 4, 5, 6)
typeof(die)




# Tipo de Dados: Interger ---------------------------------------------------------
# Vetor Integer armazena numeros que não possuam decimal.
int <- c(-1L, 2L, 4L)
typeof(int)

# Tipo de Dados: Characters ---------------------------------------------------
text <- c("Hello",  "World")
text


# Tipo de Dados: Logical ---------------------------------------------------
3 > 4
logic <- c(TRUE, FALSE, TRUE)


# Tipo de Dados: Complex Vector ---------------------------------------------------
# Vetores Complex armazenam números complexos. Para criar um vetor complexo, basta
# criar um termo imaginário com i:
comp <- c(1 + 1i, 1 + 2i, 1 + 3i)
comp
typeof(comp)


# Tipo de dados: Raw ------------------------------------------------------
# Raw vectors store raw bytes of data. Making raw vectors gets complicated,
# but you can make an empty raw vector of length n with raw(n). See the help
# page of raw for more options when working with this type of data:
raw(3)
## 00 00 00


# Atributos: attributes () ------------------------------------------------------------
# Um atributo é uma informação que você pode anexar a um vetor atômico 
# (ou qualquer objeto R). O atributo não afetará nenhum dos valores do objeto 
# e não aparecerá quando você exibir seu objeto. Você pode pensar em um atributo
# como “metadados”; é apenas um local conveniente para colocar informações 
# associadas a um objeto. O R normalmente ignorará esses metadados, mas algumas
# funções do R verificarão atributos específicos. Essas funções podem usar os 
# atributos para fazer coisas especiais com os dados.

# Você pode ver quais atributos um objeto tem com attributes (). os atributos 
# retornarão NULL se um objeto não tiver atributos. Um vetor atômico, 
# como o dado, não terá nenhum atributo a menos que você forneça alguns:

die <- 1:6
attributes(die)


# Nomes: names () ---------------------------------------------------------------
# Os atributos mais comuns para dar um vetor atômico são nomes, dimensões (dim)
# e classes. Cada um desses atributos tem sua própria função auxiliar que 
# você pode usar para atribuir atributos a um objeto. Você também pode usar 
# as funções auxiliares para pesquisar o valor desses atributos para objetos
# que já os possuem. Por exemplo, você pode procurar o valor do atributo names 
# do dado com nomes:
names(die)
names(die) <- c("one", "two", "three", "four", "five", "six")

# agora:
names(die)
attributes(die)



# Classes: class( ) -------------------------------------------------------
class("Hello")

# Dimensões ---------------------------------------------------------------
# Você pode transformar um vetor atômico em uma matriz n-dimensional 
# dando a ele um atributo de dimensões com dim()
dim(die) <- c(2, 3)
die

dim(die) <- c(3, 2)
die

#__________________________________________________________________________
# Criar: Matriz -----------------------------------------------------------
# Um conjunto de vetores pode ser organizado em matriz
matrix(vetor, nrow=2, ncol=3, byrow=T)
  
# ou 
matrix_new <- matrix(nrow = 8, ncol = 4)
    # Completar Primeira Coluna da matriz
    matrix_new[,1] <- c(22.3, 19.7, 20.8, 20.3, 20.8, 21.5,
                        20.6, 21.5)
    # Segunda Coluna
    matrix_new[,2] <- c(9.5, 13.8, 14.8, 15.2, 15.5, 15.6,
                        15.6, 15.7)  
    matrix_new
    
# Matriz: Dimensão --------------------------------------------------------
dim(matriz)


# Matriz: Definir nome da Coluna -----------------------------------------------------
# colocar a quantidade de nome igual ao numero de colunas
colnames(matriz)= c("idade", "nome")
  

# Matriz: Definir Nome da linha -----------------------------------------------------
# colocar a quantidade de nome igual ao numero de linhas
rownames(matriz)= c( ) 
  
  

# Criar: Arrays -----------------------------------------------------------
# A função array () cria uma matriz n-dimensional. Por exemplo, você pode usar 
# array para classificar valores em um cubo de três dimensões ou um hipercubo 
# em 4, 5 ou n dimensões. array não é tão personalizável quanto matrix e 
# basicamente faz a mesma coisa que definir o atributo dim. Para usar array,
# forneça um vetor atômico como primeiro argumento e um vetor de dimensões 
# como segundo argumento, agora chamado dim:
    ar <- array(c(11:14, 21:24, 31:34), dim = c(2, 2, 3))
    ar
    

# Criar: Data/Hora --------------------------------------------------------
# A Data/hora parece um character string quando visualizamos, mas na verdade é
# um "double", e sua classe é "POSIXct" "POSIXt" (possui duas classes):
    now <- Sys.time()
    now
    
    typeof(now)
    class(now)    
      
# POSIXct is a widely used framework for representing dates and times. 
# In the POSIXct framework, each time is represented by the number of 
# seconds that have passed between the time and 12:00 AM January 1st 1970 
# (in the Universal Time Coordinated (UTC) zone).
# R creates the time object by building a double vector with one element, 
# 1395057600. You can see this vector by removing the class attribute of now,
# or by using the unclass function, which does the same thing:    
    unclass(now)    
    

# Criar: Fatores ----------------------------------------------------------
# Factors are R’s way of storing categorical information.
# This arrangement makes factors very useful for recording the treatment
# levels of a study and other categorical variables.
    gender <- factor(c("male", "female", "female", "male"))    
    typeof(gender)    
    attributes(gender)
    
    
#__________________________________________________________________________
# Criar: Lista ------------------------------------------------------------
# Misturar matriz de diferentess classes, vetores de diferentes tipos e de diferente 
# dimensoes. Cada conjunto dentro da lista eh visto como um conjunto independente de 
# informacoes e podemos acessa-las para guardar informacoes dos resultados ou dados 
# brutos que serao usados para analises (sendo acessado por [[]])

    
# As listas são como vetores atômicos porque agrupam dados em um conjunto 
    # unidimensional. No entanto, as listas não agrupam valores individuais;
    # listas agrupam objetos R, como vetores atômicos e outras listas.
    # Por exemplo, você pode criar uma lista que contenha um vetor numérico 
    # de comprimento 31 em seu primeiro elemento, um vetor de caracteres de 
    # comprimento 1 em seu segundo elemento e uma nova lista de comprimento 2 
    # em seu terceiro elemento. Para fazer isso, use a função de list()    
    list1 <- list(100:130, "R", list(TRUE, FALSE))
    list1
    
# 
list(c())

    x1 <- c(1, 2, 3)
    x2 <- c("a", "b", "c", "d")
    x3 <- 3
    x4 <- matrix(nrow = 2, ncol = 2)
    x4[, 1] <- c(1, 2)
    x4[, 2] <- c( 3, 4)
    Y <- list(x1 = x1, x2 = x2, x3 = x3, x4 = x4)
    Y
# Acessar a lista por nome
    Y$x1
    Y$x4
# Acessar lista por posição
  Y[[4]]
  
    
#__________________________________________________________________________
# Criar: DataFrame --------------------------------------------------------
altura <- c(1.65,1.90,1.56,1.76)    
idade <- c(23,26,58,34) 
nomes <- c("Maria", "João", "José", "Igor")  

dados= data.frame(altura, idade, nomes)
dados  

# Juntar vetores em colunas
d1 <- data.frame(cbind(altura, idade))

# ou 
d2 <- cbind.data.frame(altura, idade)
  
# Juntar vetores em linhas
d3 <- data.frame(rbind(altura, idade))
  




# **************************************************************************
# > IMPORTAÇÃO DE DADOS -----------------------------------------------------

  # ARGUMENTOS INTERESSANTES NA LEITURA
  
  # fill=TRUE:      toda a c?lula vazia ser? preenchida com NA
  # header=TRUE:    a primeira linha ? cabe?alho
  # row.names= 1:   primeira coluna ? nome das linhas 
  # nrows=5:        ler somente as 5 primeiras linhas do arquivo
  # file="../../":  quanto mais ../ coloca vc entra em uma pasta acima do seu diretorio de trabalho, apertar TAB para ver as pastas
  # skip=2:         pular tantas linhas

# ___________________________________________________________________
# Escolha do Diretorio --------------------------------------------------


  setwd(choose.dir())
  getwd()
  
  
# ___________________________________________________________________
# Pacotes para carregar dados ---------------------------------------------

  
  library(readr) # mais indicado para abrir os dados "read_delim"
  # read_tsv: arquivo separado por tabulacao
  # read_csv: arquivo separado por csv
  # read_delim: arquivo separado por um separador espec?fico(; , \t)
  
  library(readxl)
  # read_exel: especificar importar da quer carregar
  # exel_sheets: 
            
# ___________________________________________________________________
# Usar o Pacote sem carregar ----------------------------------------
# usar a funcao dentro do pacote readr sem  carrega-lo 
    readr::read_csv() 
  
  
# ___________________________________________________________________
# Listar arquivos da pasta ------------------------------------------
  dir()   
  


# __________________________________________________________________
# Leitura do arquivo formato txt -----------------------------------
  dados=read.table("bichinhos.txt", header=T)
  dados=read.table("pratica1.txt", header=T)


# __________________________________________________________________
# Leitura de arquivos em formato csv -------------------------------
  dados=read.csv("bichinhos.csv", header=T, fill=T)
  dados=read.csv2("pratica1.csv")


# __________________________________________________________________
# Leitura de arquivos na tela --------------------------------------
# Arquivos gravados no Ctrl+C  
  dados=read.table("clipboard", h=T)


# *************************************************************************
# > EXPORTAÇÃO DE DADOS -----------------------------------------------------

# Exportar Tabela ---------------------------------------------------------
  write.table(dados.novos, "dados_novos.txt")
  write.csv(deck, file = "cards.csv", row.names = FALSE)

  # Exportar: extensão do excel
  library(WriteXLS)
  WriteXLS()    

  # Exportar: pacote readr
  library(readr)
  
  write_tsv()   #mais simples
  write_delim()
  write_delim(x=arquivo, path="dados/teste.xls", delim="\t", col_names=T)



# > SELEÇÃO --------------------------------------------------------------
  
# dados 
  deck <- data.frame(
    face = c("king", "queen", "jack", "ten", "nine", "eight", "seven", "six",
             "five", "four", "three", "two", "ace", "king", "queen", "jack", "ten", 
             "nine", "eight", "seven", "six", "five", "four", "three", "two", "ace", 
             "king", "queen", "jack", "ten", "nine", "eight", "seven", "six", "five", 
             "four", "three", "two", "ace", "king", "queen", "jack", "ten", "nine", 
             "eight", "seven", "six", "five", "four", "three", "two", "ace"),  
    suit = c("spades", "spades", "spades", "spades", "spades", "spades", 
             "spades", "spades", "spades", "spades", "spades", "spades", "spades", 
             "clubs", "clubs", "clubs", "clubs", "clubs", "clubs", "clubs", "clubs", 
             "clubs", "clubs", "clubs", "clubs", "clubs", "diamonds", "diamonds", 
             "diamonds", "diamonds", "diamonds", "diamonds", "diamonds", "diamonds", 
             "diamonds", "diamonds", "diamonds", "diamonds", "diamonds", "hearts", 
             "hearts", "hearts", "hearts", "hearts", "hearts", "hearts", "hearts", 
             "hearts", "hearts", "hearts", "hearts", "hearts"), 
    value = c(13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 13, 12, 11, 10, 9, 8, 
              7, 6, 5, 4, 3, 2, 1, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 13, 12, 11, 
              10, 9, 8, 7, 6, 5, 4, 3, 2, 1)
  )
  
  
  # Agora que você tem um baralho de cartas, precisa de uma maneira de fazer 
  # coisas parecidas com cartas com ele. Primeiro, você deve embaralhar o baralho
  #> de tempos em tempos. E a seguir, você vai querer distribuir as cartas do 
  #> baralho (uma carta de cada vez, qualquer carta que esteja no topo - não 
  #> somos trapaceiros). Para fazer isso, você precisará trabalhar com os 
  #> valores individuais dentro do seu quadro de dados, uma tarefa essencial 
  #> para a ciência de dados. Por exemplo, para distribuir uma carta do topo 
  #> do seu baralho, você precisará escrever uma função que selecione a primeira
  #> linha de valores em seu quadro de dados, como esta
  
  # Selecionar tudo
  deck[ , ]
  
  # [linha, coluna]
  deck[1, 1]
  
  # Extrair mais de uma coluna
  deck[1, c(1, 2, 3)]
  
  # Extrair mais de uma linha e mais de uma coluna
  deck[c(1, 1), c(1, 2, 3)]
  
  # todas as linhas da coluna "value"
  deck[ , "value"]

  # Selecionar a parte decimal de um numero
  a <- 3.14
  dec <- a - trunc(a)
  dec
  ## 0.14

  


  
# > MUDANDO VALORES -------------------------------------------------------
  
  # Vetor
  vec <- c(0, 0, 0, 0, 0, 0)
  vec[1]
  
  vec[1] <- 1000
  vec


# Adicionando coluna ------------------------------------------------------
  deck <- deck[1:10,]
  deck$new <- 1:10
  deck
  

# Apagando coluna ---------------------------------------------------------
  deck$new <- NULL
  deck


# Substituir algumas linhas -----------------------------------------------
  deck$value[c(1, 4)] <- c(0, 0)
  deck
  
  
# *************************************************************************
# > VISUALIZAÇÃO DE DADOS -------------------------------------------------
  

# __________________________________________________________________
# Visualizacao dos dados
  dados

# __________________________________________________________________
# Visualizacao do cabecalho da tabela 
  head(dados)

# __________________________________________________________________
# visualizacao do final da tabela
  tail(dados)

# __________________________________________________________________
# nome das colunas
  names (dados)

# __________________________________________________________________
# Verificando presenca de dados NA
  is.na(dados)
  
# __________________________________________________________________
  # Estrutura dos dados
  str(dados)
  # Caso a classe dos objetos n?o esteja certo:podemos transformar
  # exemplo:transforma??o da vari?vel (coluna1)
  # dados$coluna1= as.factor(dados$coluna1)
  # dados$coluna1= as.numeric(dados$coluna1)  
  
# __________________________________________________________________
  # Gravar uma tabela para trabalhar com ela
  attach(dados)
  
# __________________________________________________________________
  # Desgravar para utilizar outra
  detach(dados)
  
# __________________________________________________________________
# Numero de colunas e linhas
  ncol(dados)
  nrow(dados)
  
# __________________________________________________________________
  # Dimensoes do data frame
  dim(dados)

# __________________________________________________________________
# Nome de colunas 
  colnames(dados) 
  names(dados)
  
  
  
# *************************************************************************
# > FUNÇÕES ---------------------------------------------------------------

  
# tapply ( ) --------------------------------------------------------------
# Calcular funções de forma eficiente. A mesmo função para as colunas
# separadas por categorias 
  data(iris)  
  mean <- tapply( X = iris$Sepal.Length, INDEX = iris$Species, FUN= mean)
  sd <- tapply(iris$Sepal.Length, iris$Species, sd)
  length <-tapply(iris$Sepal.Length, iris$Species, length)
  
  # Juntar a resposta
    cbind(mean, sd, length)
  

# sapply ( ) ---------------------------------------------------------------
# Cacular a função para todas as colunas
  data(iris)
  sapply(iris, FUN = mean) 
  
# lapply ( ) ---------------------------------------------------------------
# O mesmo que o sapply mas o output diferente - resultado sai em lista
  data(iris)
  lapply(iris, FUN = mean) 
  

# summary ( ) -------------------------------------------------------------
# calcula resumo com informações básicas para todas as colunas 
  summary(iris)


# table ( ) ---------------------------------------------------------------
# Contagem do número de observações (valores únicos)
  table(iris$Species)
  table(iris$Sepal.Width)

  
  # > CONDIÇÕES -------------------------------------------------------------
  symbols <- c("7","7", "7")
  
  # Condição: todos os elementos do vetor symbol são iguais?
  symbols[1] == symbols[2] & symbols[2]==symbols[3]
  
  #ou
  all(symbols == symbols[1])
  
  #ou
  length(unique(symbols) == 1)
  
  # Condição: todos os elementos do vetor symbols são um tipo de B?
  symbols <- c("BB", "B", "BBB")
  all(startsWith(symbols, "B"))
  
  # ou
  all(symbols %in% c("B", "BB", "BBB"))
  all(c("B", "BB", "BBB") %in% symbols)
  
  
  
  
  
  
  
# *************************************************************************
# > LOOPS ---------------------------------------------------------------  
#   Usado para automatizar comandos
  
#  for (i in 1 : 27) {
#    do something
#    do something
#    do something
#  }
  
  
  
  
  # ****************************************************************************************************************************************
  # Passo 3 - Preparacao dos dados ------------------------------------------
  # ****************************************************************************************************************************************
  
# __________________________________________________________________
# Ordenando a tabela 
# Exemplo: ordenar do menor para maior pela vari?vel riqueza (coluna)
  dados1=dados[order(dados$Esp?cie),] 
  head(dados1)
  
  
# __________________________________________________________________
# Ordenando os fatores para gr?fico 
  plot(...,levels= c("Referencia","Intermedi?rio","Impactado"))
  
# __________________________________________________________________
# Ordenando um fator (Reordenando)


  #quando numero
  index= c(30,10,50,90,86,56)
  factor(index, ordered=T)
  
  #quando caracter
  nivel= c("Impactado", "Intermedi?rio", "Referencia")
  nivel
  # "Impactado" "Intermedi?rio" "Referencia" 
  
  nivel= ordered(nivel, c("Referencia","Intermedi?rio","Impactado"))
  # [1] Impactado Intermedi?rio Referencia   
  # Levels: Referencia < Intermedi?rio < Impactado
  
  







#___________________________________________
# Removendo dados 
# Neste caso, removendo a linha 51 e 52 de todas as colunas
dados.novos=dados[-c(51,52),]





# __________________________________________________________________
# Coersao para:
as.factor(objeto)    # fator com diferentes n?veis
as.numeric(objeto)   # numerico (virgula)
as.character(objeto) # caracter
as.logical(objeto)   # logico 
as.integer(objeto)   # munerico (inteiro)


#___________________________________________
# visualizacao da classe do objeto
class(dados$area)

#___________________________________________
# Mudar a classe do objeto
dados$area= as.factor(dados$area)
dados$area= as.numeric(dados$area)


#___________________________________________
# Extrair atributos do resultado das an?lises
# resultado$atributos , como exemplo a seguir 
pca$sdev
pca$rotation 

#___________________________________________
# Acrescentando uma nova coluna nos dados

#  Ex, calculei o log e agora quero salva-lo como coluna no meu banco de dados

logRiqueza=log10(Riqueza +1)

#salvar uma nova coluna na planilha
cbind(dados, logRiqueza)
#___________________________________________
# SELECAO
#___________________________________________

###### Filtrar tabelas
# selecionar coluna igual Brazil
# filtragem de dados que possuem NA, usar funcao "subset" que nao retem os NAs
revistas <- subset(x = tabela, subset = Country=="Brazil")

#ou por indexacao, mantem o NA na selecao
tabela[tabela$nomecoluna=="Brazil",]

#####
# selecionar coluna igual Brazil ou United Kingdom
revistas <- subset(x = tabela, subset = Country=="Brazil"|Country=="United Kingdom")

# ou 
revistas <- subset(x = tabela, subset = Country %in% c("Brazil","United Kingdom"))

#####
# selecionar coluna mais de uma condicao
revistas <- subset(x = tabela, subset = Country == "Brazil"| SJR>'')


# > OPERADORES ------------------------------------------------------------
# ou    :  |
# e     : &
# igual : ==
# acima : >
# abaixo:<
# %in%  : mesmo que "ou (|)", mas nesse caso eh mais simples quando voce tem mutias categorias
# diferente : !=
# não é x: !x 
# todos elementos: all()
 


# DUPLOS
#> && and || behave like & and | but can sometimes be more efficient. 
#> The double operators will not evaluate the second test in a pair of tests 
#> if the first test makes the result clear. For example, if symbols[1] does 
#> not equal symbols[2] in the next expression, && will not evaluate 
#> symbols[2] == symbols[3]; it can immediately return a FALSE for the whole 
#> expression (because FALSE & TRUE and FALSE & FALSE both evaluate to FALSE).
#> This efficiency can speed up your programs; however, double operators are 
#> not appropriate everywhere. && and || are not vectorized, which means they
#> can only handle a single logical test on each side of the operator.




#___________________________________________
# LISTAS
#___________________________________________
lista <- list(tab1, tab2)

# exportar

write.rds()

# ou
library(readr)
write_rds ()



# ****************************************************************************************************************************************
# Passo 3: An?lise descritiva ---------------------------------------------
# ****************************************************************************************************************************************

# Resumo dos dados
summary(dados)

# Atributo das colunas
str(dados)


# ______ Medidas de Tend?ncia ______
#       1. M?dia
mean (Riqueza)# [1] 29.22

#       2. Mediana
#  ? menos sens?vel do que a m?dia para dados extremos. Indicado para quando os dados possuem distribui??o 
#  assim?trica

# Quartis
quantile(Riqueza)
quantile(Riqueza, 0.1) #Escolher o percentil, neste caso ? o percentil 10
quantile(Riqueza, c(0.1,0.5,0.9)) #calcular v?rios percentis


# ______Medidas de Dispers?o _______

#    1. Desvio Padr?o 
sd(Riqueza)# [1] 8.031291  
# desvio padr?o = mostra a m?dia da varia??o em torno da m?dia ? de 8
# Os valores mais comuns seriam os valores entre 29 (+-8) 
# OBS: importante a m?dia sempre vir acompanhada do desvio padr?o
# que vai indicar o desvio dos dados em torno da m?dia. O desvio ? a raiz quadrada da vari?ncia
# Var= somat?rio(xi- m?dia)/n
# Desv.Pad= raiz(VAR)

# Desvio Padr?o maior do que a m?dia: normalmente ocorre com dados assim?tricos. Nesse caso n?o representa bem a tendencia central dos dados.
# para isso usamos a mediana para representar os dados no lugar da m?dia.



#   2. Vari?ncia: pouco usado porque o desvio padr?o oferece um valor mais f?cil de interpretar
#   e a unidade do desvio pad est?o na mesma unidade que a m?dia
var(Riqueza)

# M?dia dos valores de riqueza por ambiente
mean(Riqueza[Ambiente=="prim?rio"])
mean(Riqueza[Ambiente=="secund?rio"])
#ou
tapply (Riqueza, Ambiente, mean)
tapply (Riqueza, Ambiente, sd)

#   3. Amplitude
max(Riqueza)-min(Riqueza) #valor m?ximo menos o valor m?nimo: n?o ? t?o usado pq os valores m?ximo
#  e m?nimo podem ser outliers


#  4. Coeficiente de Varia??o
# ? o desvio padr?o em porcentagem. ? interessante para comparar qual 
# qual de duas vari?veis que est?o em unidades diferentes ou em magnitudes diferente varia mais
# CV= desv.pad/m?dia

(sd(Riqueza)/mean(Riqueza))*100
# [1] 27.48559       interpreta??o: essa varia??o m?dia representa 27% do valor m?dio
(sd(?rea)/mean(?rea))*100
# [1] 36.00638       A ?rea tem uma varia??o maior do que a Riqueza

# ******************************************************************************************************************************************************
# Identificando Outlier
# ****************************************************************************************************************

# Ap?s fazer o gr?fico podemos identificar cada ponto do gr?fico, e assim, identicar quais pontos est?o
# se destacando dos demais. Fazemos isso usando o seguinte comando:
# text(posi??o no X, posi??o no y, lable="o que escrever", pos= posi??o que vai aparecer).
text(Ambiente, Riqueza, lable=row.names(dados), pos=1)


