





# Spark -------------------------------------------------------------------
# É uma ferramenta Open source usada para processamente de big data distribuído
# em memória. O spark possui uma biblioteca defaut para machine learning que
# pode ser acessada.

# O R-studio vai funcionar como um Driver Program para contruir um SparkContext.
# Vamos criar um cluster manager local. Onde nossa propria maquina funciona como
# um cluster Spark que vai funcionar em paralelo com o r-studio. Criamos nós,
# cada nó é um core da nossa máquina, e trabalha junto para resolver o problema.


# Procedimentos no PC -----------------------------------------------------
# Antes temos que seguir o tutorial para liberar o acesso do Spark no windows



# Pacotes -----------------------------------------------------------------
instalar_carregar_pacotes <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# lista de pacotes necess?rios
pacotes <- c("sparklyr", 
             "dplyr", 
             "arrow",
             "nlme",
             "ggplot2", 
             "carrier", 
             "mlflow", 
             "reticulate", 
             "stats", 
             "glue")

library(sparklyr)

# instalar e carregar os pacotes
instalar_carregar_pacotes(pacotes)


# Desconectar conexão ativa com o spark -----------------------------------
# Caso tenha alguma conexão aberta, será desconectada
sparklyr::spark_disconnect_all()


# Conectar ao cluster spark local -----------------------------------------
# sparkhome: colocar o local de instalação do spark
# Após isso, vai aparecer uma aba em Connextions, no painel Environment
sc <- spark_connect(master = "local", 
                    spark_home="E:/Documentos_E/aula-big-data/spark/build/spark-3.4.0-bin-hadoop3")



# Spark: Leitura Dados ----------------------------------------------------------
# Utilizar o Spark para ler um arquivo com grande volume de dados. O spark está
# fora do computador. Ele está em contato com o R através do SparkContext (que
# foi criado através do "spark_connect")


# Dados -------------------------------------------------------------------
# CONHECENDO NOSSOS DADOS
# fonte: https://www.kaggle.com/datasets/yuanyuwendymu/airline-delay-and-cancellation-data-2009-2018?resource=download&select=2017.csv
# Airline Delay and Cancellation Data, 2009 - 2018
# pasta datasets -> dois arquivos -> 1 de aproximadamente 700MB e outro de aproximadamente 476MB


# Leitura -----------------------------------------------------------------
# carregando o primeiro dataset da base de dados de voos de avioes
# como input, sempre temos que colocar o "sc" a conexão criada anteriormente
# Importante notar que conseguimos ver apenas as 1000 primeiras linhas da 
# planilha, então ainda temos que ser capaz de observar e trabalhar com a 
# planilha total
dados_voos_2017 <- spark_read_csv(sc, 
                                  name="dados_voos_2017", 
                                  path="raw_datas_exemplos/Spark_datasets/voos-avioes/2017.csv", 
                                  header=TRUE, 
                                  infer_schema=TRUE)


# Explorando --------------------------------------------------------------
# verificando nossos dados. Vemos que ele sabe a quantidade de colunas, mas 
# nao sabe a quantidade de linhas.
glimpse(dados_voos_2017)


# contagem de linhas do dataset
# Vemos que temos muitos dados, que a partir de então teremos dificuldades para
# cosneguir rodar modelos com a biblioteca que nós estamos acostumados.
sdf_nrow(dados_voos_2017)
#[1] 5674621



# verificar o tempo de processamento para coletar os dados do dataset dos dados de 2017
system.time(dados_voos_2017 <- spark_read_csv(sc, 
                                              name="dados_voos_2017", 
                                              path="raw_datas_exemplos/Spark_datasets/voos-avioes/2017.csv", 
                                              header=TRUE, 
                                              infer_schema=TRUE))


# Operações de Transformação de Dados -------------------------------------
# quando um dataset é lido pelo spark, ele é carregado de forma distribuída na memória
# elas ainda não foram entregues para o driver (R Studio), essas são as operações de
# de transformações de dados. Essas operações são realizadas mais rápido
apenas_voos_cancelados <- dados_voos_2017 %>% filter(CANCELLED == 1)

# Transformação de dados:
# leitura
# seleção
# agrupamento
# filtros
# ordenação
# joins


# Operações de Ação/Manipulações de Dados --------------------------------------
# as operaçoes de manipulações são feitas distruídas e com execução realizada 
# quando uma ação é chamada ação: mostrar os dados
glimpse(apenas_voos_cancelados)

# Ações:
# trazer os dados para o RStudio
# contagem
# salvar
# mostrar os dados


#> Quando fazemos as operações de manipulação é como colocar passos a serem 
#> feitos com o spark. Então quando eu executo uma tarefa de ação. Os passos 
#> anteriores são realizados (carregamente e filtragem dos dados).
#> Neste caso, as informações sobre carregamento e filtragem, como já foram feitos,
#> ficam pre-carregados na memória, acelerando o processamento.



# Filtros -----------------------------------------------------------------
# criando uma variavel que sinaliza que o voo esta atrasado quando 
# o tempo de atraso na chegada for maior que 15 minutos
dados_voos_2017 <- dados_voos_2017 %>%
  mutate(ATRASADO = ifelse(ARR_DELAY >= 15, 1, 0))

# verificando a variavel criada
glimpse(dados_voos_2017)


# Exemplo de Transformação ------------------------------------------------
# exemplo de uma operação de transformação e guardar os dados em uma variável
# contagem da quantidade de voos atrasados e não atrasados
dados_agrupados_voos_atrasados <- dados_voos_2017 %>% 
  group_by(ATRASADO) %>%
  summarize(QUANTIDADE = n()) %>%
  mutate(PORCENTAGEM = QUANTIDADE/sum(QUANTIDADE)*100)

# outra ação de visualização
# Somente depois que pedimos essa visualização é que o Spark entrega de fato 
# o resultado para a gnt. 
glimpse(dados_agrupados_voos_atrasados)


# Transformação: Dataframe Spark -> Dataframe R ---------------------------
# o dataframe spark pode ser transferido da memória do R e transformado em um R dataframe com 
# a função collect nessa operação, os dados distribuídos vão todos para o driver na aplicação 
# e faz a transformação. Cuidado! Pode Travar o computador dependendo da quantidade
# de dados.Então a dica é coletar uma pequena amostra para algum teste, ou então
# sempre transforamar e coletar o minimo de informações que puder
 dados_agrupados_voos_atrasados_R <- collect(dados_agrupados_voos_atrasados)

# R dataframe, fora do cluster spark
dados_agrupados_voos_atrasados_R

# verificando diferença entre quantidade de voos atrasados e voos não atrasados de 2017
ggplot(data=dados_agrupados_voos_atrasados_R, aes(x=as.character(ATRASADO), y=QUANTIDADE)) +
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=QUANTIDADE), vjust=1.6, color="white", size=3.5)+
  xlab("Voos atrasados") +
  ylab("Quantidade") +
  ggtitle("Quantidade de voos atrasados em 2017") +
  theme_minimal()

# verificando os missing values
dados_voos_2017 %>% filter(is.na(ATRASADO))


# Transformação: DataFrame R -> Dataframe Spark ---------------------------
# levando o dataframe R para o cluster spark  copy_to()
dados_agrupados_voos_atrasados_spark <- copy_to(sc, 
                                                dados_agrupados_voos_atrasados_R, 
                                                "dados_agrupados_voos_atrasados_spark")

# verificando o dataframe spark
glimpse(dados_agrupados_voos_atrasados_spark)


# Lista de Tabelas Spark --------------------------------------------------
# mostrando a lista de tabelas do cluster spark
src_tbls(sc)



# Leitura de Novos Dados --------------------------------------------------
# lendo os dados de 2018 do mesmo esquema de dataset como um dataframe Spark
dados_voos_2018 <- spark_read_csv(sc, 
                                  name="dados_voos_2018", 
                                  path="raw_datas_exemplos/Spark_datasets/voos-avioes/2018.csv", 
                                  header=TRUE, 
                                  infer_schema=TRUE)

# visualizando os dados de 2018
glimpse(dados_voos_2018)


# Explorando --------------------------------------------------------------
# criando uma variável que sinaliza que o voo está atrasado quando o tempo de atraso 
# na partida for maior que 15 minutos também para os dados de de 2018
dados_voos_2018 <- dados_voos_2018 %>%
  mutate(ATRASADO = ifelse(ARR_DELAY >= 15, 1, 0))

# exemplo de uma operação de transformação e guardar os dados em uma variável
# contagem da quantidade de voos atrasados e não atrasados
dados_agrupados_voos_atrasados_2018 <- dados_voos_2018 %>% 
  group_by(ATRASADO) %>%
  summarize(QUANTIDADE = n()) %>%
  mutate(PORCENTAGEM = QUANTIDADE/sum(QUANTIDADE)*100)

# verificando as porcentagens de voos atrasados em 2017
dados_agrupados_voos_atrasados

# verificando as porcentagens de voos atrasados em 2018
dados_agrupados_voos_atrasados_2018

# verificando diferença entre quantidade de voos cancelados e voos não cancelados de 2018
ggplot(data=collect(dados_agrupados_voos_atrasados_2018), aes(x=as.character(ATRASADO), y=QUANTIDADE)) +
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=QUANTIDADE), vjust=1.6, color="white", size=3.5)+
  xlab("Voos atrasados") +
  ylab("Quantidade") +
  ggtitle("Quantidade de voos atrasados em 2018") +
  theme_minimal()


# Unindo dois datasets ----------------------------------------------------
# unindo os dois datasets: dados de 2017 e 2018
dados_voos_completos <- union_all(dados_voos_2017, dados_voos_2018)

# continua as mesmas informacões
glimpse(dados_voos_completos)

# verificando a quantidade de linhas do dataset
sdf_nrow(dados_voos_completos)


# Estrategia para calcular quantidade de linhas ---------------------------
# em dataset gigantescos

# 1. criar um objeto de fração com valor bem pequeno (Ex. 1% dos dados)
fracao_contagem <- 0.01

# coleta uma pequena amostra do dataset completo
pequena_amostra <- sdf_sample(dados_voos_completos, 
                              fraction = fracao_contagem, 
                              replacement = TRUE, 
                              seed = 42) 

# contagem de linhas do dataset
numero_de_linhas <- sdf_nrow(pequena_amostra)

# aproximacao contagem
aproximacao_contagem_linhas <- numero_de_linhas/fracao_contagem

# valores aproximado da quantidade de linhas do dataset
print(aproximacao_contagem_linhas)

# contagem de linhas do dataset
diferenca <- sdf_nrow(dados_voos_completos) - aproximacao_contagem_linhas

# diferenca computada
print(diferenca)



# Modelo Linear -----------------------------------------------------------

# Modelos de regressao linear

# Objetivo 1: 
# Desenvolver um modelo para calcular tempo decorrido real em relacao a distancia percorrida
# o mais simples possivel

# selecionando apenas as colunas que sao importantes para o modelo
dados_selecionados <- select(dados_voos_completos, ACTUAL_ELAPSED_TIME, DISTANCE)

# retirar missing values do dataset caso existam
dados_selecionados <- dados_selecionados %>% 
  na.omit


# Correlação --------------------------------------------------------------
# verificando a correlacao 
ml_corr(dados_selecionados, 
        columns = c("ACTUAL_ELAPSED_TIME","DISTANCE"), 
        method = "pearson")



# Coletando Amostra dos dados ---------------------------------------------
# coletando uma pequena amostra
pequena_amostra <- sdf_sample(dados_selecionados, fraction = 0.01, seed = 42)


# Estimando quantidade de linhas ------------------------------------------
# contando a quantidade de linhas da amostra (1%)
sdf_nrow(pequena_amostra)


# Gráfico de correlação ---------------------------------------------------
# verificando a correlacao das duas variaveis no grafico de dispersao
ggplot(pequena_amostra, aes(x=DISTANCE, y=ACTUAL_ELAPSED_TIME)) +
  geom_point(alpha = 0.1) +
  geom_smooth(method=lm, color="red", se=FALSE) +
  ylab("Distancia percorrida") +
  xlab("Tempo total decorrido") +
  theme_minimal()


# Tutorial ----------------------------------------------------------------
# Para saber como usar a biblioteca usada para fazer análises dentro do 
# spark olhar esse site: https://spark.rstudio.com/
library(sparklyr)

# Fit Modelo --------------------------------------------------------------
# realizando o fit do modelo
modelo_linear <- dados_selecionados %>%
  ml_generalized_linear_regression(ACTUAL_ELAPSED_TIME ~ DISTANCE)

# relatorio do modelo
summary(modelo_linear)

# realizando o fit do modelo utilizando a função ml_linear_regression
# Aqui nos fornece o R² do modelo
dados_selecionados %>%
  ml_linear_regression(ACTUAL_ELAPSED_TIME ~ DISTANCE) %>%
  summary()

# coletando os fitted values
dados_selecionados <- ml_predict(modelo_linear, dados_selecionados)

# verificando os fitted values no dataset
head(dados_selecionados)

# verificando a correlação ao quadrado entre valores observados e fitted values
# Uma outra forma de ver o R² do modelo
ml_corr(dados_selecionados, 
        columns = c("ACTUAL_ELAPSED_TIME","prediction"), 
        method = "pearson")^2

# coletando os resíduos
residuos <- sdf_residuals(modelo_linear)

# verificando os resíduos coletados
head(residuos)

# coletando amostra para verificar a distribuição dos resíduos
# Para Visualizar quase sempre coletamos uma amostra para facilitar 
# o processamento. Neste caso, vamos coletar uma amostra para 
# ver o resíduos
amostra_residuos <- sdf_sample(residuos, fraction = 0.01) %>%
  collect

# visualizando a distribuição dos resíduos
# Temos aqui uma ótima visualização de que o modelo está errando
# pouco
ggplot(amostra_residuos, aes(x = residuals)) + 
  geom_histogram(aes(y = ..density..), binwidth = 5) + 
  ylab("Frequencia") +
  xlab("Residuos") +
  theme_minimal() +
  geom_density()


# Predict -----------------------------------------------------------------
# realizando um predict no modelo distribuído no spark

# criando um R dataframe
dados_novos <- data.frame(DISTANCE=c(2000))


# Transformando o datafram R em um dataframe Spark ------------------------
dados_novos_spark <- copy_to(sc, dados_novos, "dados_novos_spark")

#
# qual o tempo total de viagem quando a distância é igual a 2000 km?
resultado <- ml_predict(modelo_linear, dados_novos_spark)

# previsão do modelo
print(resultado)


# Resultado do Modelo Spark: collect ( ) ----------------------------------
# lembrando que o resultado e um dataframe spark, para coletá-lo precisa usar a função collect
# precisamos fazer isso para pegar o resultado do spark e trazer para o R para 
# imprimir na tela
resultado_R <- resultado %>%
  collect

# previsão como um dataframe R
print(resultado_R$prediction)


# Objetivo 2: Regressão Logistica --------------------------------------------------------------
# Desenvolver um modelo para calcular se o voo vai atrasar para chegar ou não baseado 
# nas variáveis do banco de dados
# variável dependente: ATRASADO
# Proposta de modelo: Regressão Logística

# revisitando nosso dataset completo
glimpse(dados_voos_completos)

# adicionando variáveis de tempo para verificar as interferências dessas variáveis nos atrasos dos voos
dados_voos_completos_tratado <- dados_voos_completos %>%
  mutate(ATRASO_PARTIDA = as.numeric(DEP_DELAY),
         ATRASO_CHEGADA = as.numeric(ARR_DELAY),
         MES = as.character(month(FL_DATE)),
         DIA_DA_SEMANA = as.character(weekday(FL_DATE)))


# Coletar em R para visualizar os dados -----------------------------------
# coletando uma pequena amostra para verificar essas variáveis graficamente
amostra_voos_completos_tratado_R <- dados_voos_completos_tratado %>%
  group_by(DIA_DA_SEMANA) %>%
  summarise(media_atraso_chegada = mean(ATRASO_CHEGADA)) %>%
  collect


# Explorando --------------------------------------------------------------
# verificando a variacao da media de atraso na chegada dos voos em relacao ao dia da semana
ggplot(data=amostra_voos_completos_tratado_R, aes(x=as.character(DIA_DA_SEMANA), y=sprintf("%0.2f", round(media_atraso_chegada, digits = 2)))) +
  geom_bar(stat="identity", fill="steelblue") +
  xlab("Dia da semana") +
  ylab("Media de atraso dos voos") +
  ggtitle("Media de atraso dos voos por dia da semana") +
  theme_minimal()


# Coletando Amostras ------------------------------------------------------
# coletando uma pequena amostra para verificar essas variacoes graficamente agora nos meses
amostra_voos_completos_tratado_R <- dados_voos_completos_tratado %>%
  group_by(MES) %>%
  summarise(media_atraso_chegada = mean(ATRASO_CHEGADA)) %>%
  collect

# verificando a variação da média de atraso na chegada dos voos em relaçã ao mês do ano
ggplot(data=amostra_voos_completos_tratado_R, aes(x=as.factor(MES), y=sprintf("%0.2f", round(media_atraso_chegada, digits = 2)))) +
  geom_bar(stat="identity", fill="steelblue") +
  xlab("Mes") +
  ylab("Media de atraso dos voos") +
  ggtitle("Media de atraso dos voos por mes") +
  theme_minimal()


# Selecionando Variaveis do Modelo ----------------------------------------
# selecionando as variáveis da modelagem
dados_modelo_atraso <- select(dados_voos_completos_tratado, ATRASADO, CRS_DEP_TIME, MES, DIA_DA_SEMANA, DISTANCE)

# verificando minhas variáveis de entrada do modelo
glimpse(dados_modelo_atraso)

# adicionando a variável de horas nos dados de entrada do modelo baseado na coluna CRS_DEP_TIME
dados_modelo_atraso <- dados_modelo_atraso %>%
  ft_bucketizer(
    input_col = "CRS_DEP_TIME",
    output_col = "HORAS",
    splits = seq(0, 2400, 400)
  )

# visualizando como ficaram as nossas variáveis de entrada
glimpse(dados_modelo_atraso)

# retirando os missing values do modelo
dados_modelo_atraso_tratados <- dados_modelo_atraso %>%
  na.omit


# Fit Modelo Logistico ----------------------------------------------------
# realizando o fit do modelo logística para a variável y ATRASADO
modelo_atrasos_voos <- ml_generalized_linear_regression(dados_modelo_atraso_tratados, 
                                                        ATRASADO ~ DIA_DA_SEMANA + HORAS + MES + DISTANCE, 
                                                        famlily="binomial"
)


# Resumo ------------------------------------------------------------------
# resumo dos modelos e seus coeficientes
summary(modelo_atrasos_voos)

# coletando os fitted values
valores_predicts <- ml_predict(modelo_atrasos_voos, dados_modelo_atraso_tratados)

# visualizando as primeiras linhas do dataset
head(valores_predicts)

# adotando o threshold de 0.5 para coletar os labels dos eventos e não eventos
valores_predicts <- valores_predicts %>%
  mutate(y_hat = ifelse(prediction >= 0.5, 1, 0))

# verificando os labels
head(valores_predicts)

# coleta da performance do modelo via AUC (Area abaixo da curva ROC)
ml_binary_classification_evaluator(valores_predicts,  
                                   label_col = "ATRASADO", 
                                   metric_name = "areaUnderROC",
                                   raw_prediction_col ="prediction") 

# verificando a matriz de confusao em grandes volumes de dados
# Conseguimos aqui ver q nosso modelo não acertou nada
table(pull(valores_predicts, ATRASADO), pull(valores_predicts, y_hat))

# realizando um previsção se um voo novo vai atrasar ou não pelo modelo
novo_dado_atraso_R <- data.frame(DIA_DA_SEMANA=c("3"),
                                 HORAS=c(2),
                                 MES=c("2"),
                                 DISTANCE=c(2000))

# novo dado transformado para um spark dataframe
# Para fazer as previsões eu jogo para o spark
novo_dado_atraso_spark <- copy_to(sc, novo_dado_atraso_R, "novo_dado_atraso")

# coletando os fitted values
novo_dado_atraso_predict <- ml_predict(modelo_atrasos_voos, novo_dado_atraso_spark)

# resultado da previsão
print(novo_dado_atraso_predict)

# estrategias de trabalhar com grandes volumes de dados




# Armazenamento em Parquet ------------------------------------------------
# São arquivos menores que csv
# armazenando o dataset em formato parquet no diretório local
spark_write_parquet(dados_voos_completos, 
                    path = "./datasets/arquivos-comprimidos/voos-avioes.parquet",
                    mode = "overwrite")


# modos de armazenamento salvar arquivos parquet
# overwrite (substituir)
# append (adicionar)
# error (dá erro quando tem dado)
# ignore (ignora a operação quando tem dado)

# lendo o arquivo parquet
dados_voos_completo_parquet <- spark_read_parquet(sc, 
                                                  name = "dados_voos_completo_parquet", 
                                                  path = "./datasets/arquivos-comprimidos/voos-avioes.parquet")

# visualizando o dataset carregado
glimpse(dados_voos_completo_parquet)

# contando a quantidade de linhas
sdf_nrow(dados_voos_completo_parquet)

# comparando tempo de consulta

# verificar o tempo de processamento para coletar os dados armazenados em parquet
system.time(spark_read_parquet(sc, 
                               name = "dados_voos_completo_parquet", 
                               path = "./datasets/arquivos-comprimidos/voos-avioes.parquet"))

# menos tempo que carregar o dataset de um dos anos 2017 ou 2018 em formato csv

# adicionando partições no dataset comprimido

# armazenando o dataset em formato parquet no diretório local e com partições de ano, para isso precisaremos de uma
# variável de ano

dados_voos_completo_parquet <- dados_voos_completo_parquet %>%
  mutate(ANO = as.character(year(FL_DATE)))

# verificando a variável adicionada
glimpse(dados_voos_completo_parquet)

# carregando o dataset em um diretório local e particionando os dados por ano
spark_write_parquet(dados_voos_completo_parquet, 
                    path = "./datasets/arquivos-comprimidos/voos-avioes-anos.parquet",
                    partition_by = c("ANO"),
                    mode = "overwrite")


# Particionar os dados ----------------------------------------------------
# agora e possível acessar as partições e ler de uma forma muito mais rápida os dados de cada partição

# verificar o tempo de processamento para coletar os dados armazenados em parquet do ano de 2017
system.time(dados_voos_ano_2017_parquet <- spark_read_parquet(sc, 
                                                              name = "dados_voos_ano_2017_parquet", 
                                                              path = "./datasets/arquivos-comprimidos/voos-avioes-anos.parquet/ANO=2017"))

# mesmo procedimento para acessar os dados do ano de 2018
system.time(dados_voos_ano_2018_parquet <- spark_read_parquet(sc, 
                                                              name = "dados_voos_ano_2018_parquet", 
                                                              path = "./datasets/arquivos-comprimidos/voos-avioes-anos.parquet/ANO=2018"))

# armazenando o dataset em formato parquet no diretório local e com partições de ano e mes, 
# para isso precisaremos de uma variável de mes

dados_voos_completo_parquet <- dados_voos_completo_parquet %>%
  mutate(MES = as.character(month(FL_DATE)))

# e possivel adicionar mais particoes quando o arquivo parquet e salvo em algum diretório
spark_write_parquet(dados_voos_completo_parquet, 
                    path = "./datasets/arquivos-comprimidos/voos-avioes-anos-mes.parquet",
                    partition_by = c("ANO","MES"),
                    mode = "overwrite")

# verificar o tempo de processamento para coletar os dados de mes 2 de 2017
system.time(dados_voos_ano_2017_2_parquet <- spark_read_parquet(sc, 
                                                                name = "dados_voos_ano_2017_2_parquet", 
                                                                path = "./datasets/arquivos-comprimidos/voos-avioes-anos-mes.parquet/ANO=2017/MES=2"))

# verificar o tempo de processamento para coletar os dados de mes 4 de 2018
system.time(dados_voos_ano_2018_4_parquet <- spark_read_parquet(sc, 
                                                                name = "dados_voos_ano_2018_4_parquet", 
                                                                path = "./datasets/arquivos-comprimidos/voos-avioes-anos-mes.parquet/ANO=2018/MES=4"))


# comparando quantidade de linhas retornadas

# quantidade de linhas do dataset completo
sdf_nrow(dados_voos_completo_parquet)

# quantidade de linhas do dataset completo
sdf_nrow(dados_voos_ano_2018_parquet)

# quantidade de linhas do dataset completo
sdf_nrow(dados_voos_ano_2018_4_parquet)

