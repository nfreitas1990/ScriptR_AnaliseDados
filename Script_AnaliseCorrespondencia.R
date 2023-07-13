

# Analise de Correspondencia
# Natalia Freitas
# Janeiro 2023

# Analise de correspondencia


# Introdução 


# Contextualização --------------------------------------------------------
# É uma técnica não-supervisionada. Portanto trata de uma técnica 
# exploratória para avaliar a relação conjunta entre as variáveis.
# Não sendo adequadas para inferencias. Se temos a adição de novas
# variáveis temos que refazer as análises.



# Objetivo:  --------------------------------------------------------------
# - Verifica se existe associação estatisticamente significativa
#   entre as variáveis e suas categorias, criando um mapa perceptual
#   para visualizar as associações;
# - Analise de variáveis categóricas (exclusiva);
# - Duas variáveis categoricas (analise correspondencia simples)
# - Mais de duas variáveis categoricas (analise correspondencia múltipla)


# Obs: Se as variáveis forem quantitativas, elas devem ser 
#      categorizadas antes que possamos usar análise de correspondencia



# Etapas para Analise -----------------------------------------------------
# 1. Teste Qui-Quadrado: Análise de significancia estatistica da associação 
#    entre as variáveis e suas categorias por meio do teste qui-quadrado (X²)
# 2. Mapa Percentual: Elaboração e interpretação do mapa perceptual




# Pacotes -----------------------------------------------------------------
pacotes <- c("plotly",
             "tidyverse",
             "ggrepel",
             "knitr", "kableExtra",
             "sjPlot",
             "FactoMineR",
             "amap", 
             "ade4")

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
# Carregamento dos dados para exemplo de análise
dados_adapta <- read.csv("raw_datas_exemplos/estudantes_adapta.csv")
load(file = "raw_datas_exemplos/perfil_investidor_aplicacao.RData")



# Exemplo da disposição dos dados -----------------------------------------
# Visualização da base de dados
# - Duas variáveis categoricas (perfil e aplicação)
# - A coluna "estudante" é apenas o ID, não entra na análise
# - vamos verificar a associação entre o perfil e a aplicação
perfil_investidor_aplicacao %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE,
                font_size = 20)

# Tabelas de frequência das variáveis
summary(perfil_investidor_aplicacao)


# xx Análise de Correspondencia Simples (ANACOR)------------------------------


# ETAPA 1 -----------------------------------------------------------------

# Se fizermos apenas o teste chi-quadrado, saberemos se as variáveis 
# são ou não associadas entre si. Porém, não saberíamos de onde vem a
# associação, qual as categorias associadas. Então, para ter essa infor
# mação, calculamos os resíduos padronizados ajustados para identificar,
# detalhar a associação (então a análise de correspondencia acaba sendo
# um pós-teste do teste chi-quadrado, como fazemos na anova com o pos teste)

# Teste Chi-quadrado ------------------------------------------------------
#   Análise da associação de variáveis categóricas por meio de tabelas de 
#   Contingencia. Neste momento testamos se existe e se a associação entre
#   as variáveis categóricas são significativas. 
#   A análise é sempre feita para pares de categorias das variáveis, então 
#   cada par terá um valor de chi-quadrado. Depois calcularemos 
#   um chi-quadrado global.



# 1. Tabela de contingência:
#    - Contém as frequencias absolutas observadas para cada par de 
#      categorias das variáveis;
#    - É uma tabela de classificação cruzada (cross tabulation) usada
#      para a contagem da categoria de cada par de variáveis.
#    - Nao é necessaria que as variáveis possuam a mesma quantidade de
#      categorias

# Essa Tabela necessária pq usaremos teste chi-quadrado
#    para a análise posterior da associação entre cada categoria das duas
#    variáveis.

#    Tab. Contingência com frequências absolutas OBSERVADAS
    tabela_contingencia <- table(perfil_investidor_aplicacao$perfil,
                             perfil_investidor_aplicacao$aplicacao)
    tabela_contingencia

#   Definição da quantidade de observações na tabela de contingência
    n <- sum(tabela_contingencia)
    n
    
# 2. Teste Chi-quadrado: tem como objetivo encontrar um valor de 
#    dispersão para duas variáveis categóricas nominais e avaliar
#    a associação entre variáveis qualitativas. O princípio básico
#    é comparar proporções, ou seja, possíveis divergências entre
#    as frequencias esperadas e observadas para um certo evento.

#  Ho: frequencias observadas = frequencias esperadas. Não há 
#      associação entre os grupos.Variáveis se associam de forma
#      aleatória.
#  p-value: risco de rejeitar uma hipotese verdadeira
#  X-square: ao nível de significancia alpha é denominado qui-
#            quadrado crítico ou tabelado.    

# Dado o nível de significancia e os graus de liberdade, se o valor
# da estatistica x-square for maior do que seu valor crítico, há 
# associação significativa entre as duas variáveis.
    qui2 <- chisq.test(x = tabela_contingencia)
    qui2
    
    
# 3. Tabela de Frequencias absolutas esperadas:  
#    Esssa tabela é calculada com base no somatório das margens (linha/ coluna)
#    da tabela de contingncia de cada par de categorias 
    qui2$expected  
  
#> Tabela de Frequencias observadas (=tabela de contingencia)   
    qui2$observed

#> Tabela de contingência com frequências absolutas observadas e esperadas
    sjt.xtab(var.row = perfil_investidor_aplicacao$perfil,
             var.col = perfil_investidor_aplicacao$aplicacao,
             show.exp = TRUE)
  
# 4. Tabela com os resíduos: diferença entre a frequencia observada
#   e a frequencia esperada. O mesmo calculo é realizado para cada
#   par de categorias. Esta tabela serve de base para o calculo em 
#   si do chi-quadrado.    
    
# Resíduo Padronizado Ajustado ajuda a entender de onde vem a associação
# identificada pelo teste chi-quadrado. Pois o Chiquadrado só diz que a 
# associação existe.
# Se o valor do resíduo padronizado ajustado for > 1.96, interpreta-se
# que existe associação significativa (ao nível de 5%) entre as duas
# categorias que interagem na célula, se for <1.96, não há associação
# Esse valor de 1.96 é o valor crítico da normal padrão para o nível 
# de significancia de 5%.
    
    # residuo
    qui2$observed - qui2$expected
    
    # resíduos padronizados
    qui2$residuals
    
    # resíduos padronizados ajustados
    qui2$stdres

#> resultado: A categoria "conservador" está associado com a categoria 
#>            poupança; A categoria "moderado" está associada com a
#>            categoria "ações" e "CDB"; A categoria "agressivo" está
#>            associado a categoria "Poupança" e "Ações"
#>             Ou seja, pessoas com o perfil conservador tende a investir
#>             em poupança. Pessoas com o perfil moderado tende a
#>             investir em açõe e CD. Pessoas com perfil agressivo tende
#>             a investir em em ações e poupança 
    
        
# Valores de qui-quadrado por célula
# Embora tenhamos o qui-quadrado global, temos também esse qui-quadrado
# local
  ((qui2$observed - qui2$expected)^2)/qui2$expected      

    
# 5. Mapa de calor dos resíduos padronizados ajustados
#     Interpretação:A categoria "conservador" está associado com a categoria 
#>            poupança; A categoria "moderado" está associada com a
#>            categoria "ações" e "CDB"; A categoria "agressivo" está
#>            associado a categoria "Poupança" e "Ações"
#>             Ou seja, pessoas com o perfil conservador tende a investir
#>             em poupança. Pessoas com o perfil moderado tende a
#>             investir em açõe e CD. Pessoas com perfil agressivo tende
#>             a investir em em ações e poupança 
    
      data.frame(qui2$stdres) %>%
      rename(perfil = 1,
             aplicacao = 2) %>% 
      ggplot(aes(x = fct_rev(perfil), y = aplicacao,
                 fill = Freq, label = round(Freq, 3))) +
      geom_tile() +
      geom_text(size = 5) +
      scale_fill_gradient2(low = "white", 
                           mid = "white", 
                           high = "purple",
                           midpoint = 1.96) +
      labs(x = 'Perfil', y = 'Aplicação', fill = "Res. Pad. Ajustados") +
      coord_flip() +
      theme_bw()    

    
# ETAPA 2 -----------------------------------------------------------------

# Análise da associação por meio do mapa perceptual -----------------------
# Transcreve as informações das planilhas em um mapa de dispersão. As categorias
# são colocadas no grafico. Onde pontos mais próximos são mais parecidos do que
# pontos mais distantes.
    
    # 1. Determinar os autovalores: quantidade de autovalores depende da quanti
    #    dade de categorias nas variáveis (coordenadas no gráfico)
    #    Esses valores representam inércias principais parciais e são a base 
    #    para determinar a inércia principal total e o percentual de inercia 
    #    principal total em cada dimensão do mapa percentual.
    #    - quantidade de autovalores é a quantidade de categorias das linhas menos 1
    #    e quantidade de categorias de colunas menos 1.
    #    - são as coordenadas dos pontos no gráfico;  
    
    
# 1.A. Definição da matriz A
#      Resíduos padronizados (qui2$residuals) divididos pela raiz quadrada do 
#      tamanho da amostra (n)
    matrizA <- qui2$residuals/sqrt(n)
    matrizA
# 1.B. Definição da matriz W (Matriz W = Matriz A transposta X Matriz A)
    matrizW <- t(matrizA) %*% matrizA
    matrizW 
#1.C. Definição da quantidade de dimensões
    qtde_dimensoes <- min(nrow(matrizW) - 1, ncol(matrizW) - 1)
    qtde_dimensoes
    
#1.D. Definição dos valores singulares
    VS_AV <- svd(matrizA, nu = qtde_dimensoes, nv = qtde_dimensoes)
    
#1.E. Valores singulares de cada dimensão
    valores_singulares <- VS_AV$d[1:qtde_dimensoes]
    valores_singulares
    
#1.F. Autovalores (eigenvalues) de cada dimensão
#     autovalores de cada eixo    
    eigenvalues <- (valores_singulares)^2
    eigenvalues
    
#1.G. Cálculo da inércia principal total (a partir do qui-quadrado)
#     autovalroes totais
    inercia_total <- as.numeric(qui2$statistic/sum(tabela_contingencia))
    inercia_total
    
#1.H. Cálculo da variância explicada em cada dimensão
#     A inércia é usada para calcular a explicação de cada eixo    
    variancia_explicada <- eigenvalues / inercia_total
    variancia_explicada


    
      # 2. Calculo das Massas: As massas representam a influencia que cada 
      #>   categoria exerce sobre as demais categorias de sua variavel, seja
      #>   na coluna (column profile) ou linha (row profile)
      #>   Com base nos "totais" da tabela de contingencia, para a categoria 1
      #>   das variáveis, obtem-se as massas médias 
        
#2.a. Cálculo das massas das colunas (column profiles)
    soma_colunas <- apply(tabela_contingencia, MARGIN = 1, FUN = sum)
    soma_colunas
#2.b. Massas das colunas (column profiles)
#     pegando o total da coluna (n), da margem da coluna
    massa_colunas <- soma_colunas / n
    massa_colunas
# Interpretação: 17% tem perfil conservador; 25% moderado;58% agressivo   
    
        
#2.c. Cálculo das massas das linhas (row profiles)
    soma_linhas <- apply(tabela_contingencia, MARGIN = 2, FUN = sum)
    soma_linhas
    
#2.d. Massas das linhas (row profiles)
#     pegando o total da linha (n), da margem das linhas
    massa_linhas <- soma_linhas / n
    massa_linhas
# Interpretação: 15% investem em poupança; 40% CDB; 45% Acoes   
    


      # 3. Determinando os Autovetores: As massas calculadas acima serão
      #    utilizadas para o calculo dos autovetores. A partir da matriz
      #    W podemos encontrar os autovetores a partir dos autovalores 
      #    já calculados.    
      # Cada autovalor, vai gerar seu autovetor.    
    
        
#1.O. Autovetores v das dimensões
    autovetor_v <-VS_AV$v
    autovetor_v
    
#1.P. Autovetores u das dimensões
    autovetor_u <-VS_AV$u
    autovetor_u

####        
#### Resumindo as informações até aqui
### Valor singular:raiz quadrada do autovalor
### Inércia Principal: autovalor 
### Qui2: Valores de qui-quadrado para cada dimensão
### Percentual de inercia total: Quantidade de variância explicada por dimensão    
### Percentual de inercia acumulada: Quantidade de variância acumulada nas dimensões
    
      data.frame(Dimensão = paste("Dimensão", 1:qtde_dimensoes),
               `Valor Singular` = valores_singulares,
               `Inércia Principal Parcial eigenvalues` = eigenvalues) %>%
      mutate(`Percentual da Inércia Principal Total` = (`Inércia.Principal.Parcial.eigenvalues`/inercia_total) * 100,
             `Percentual da Inércia Principal Total Acumulada` = cumsum(`Percentual da Inércia Principal Total`),
             Qui2 = qui2$statistic[[1]] * `Percentual da Inércia Principal Total` / n,
             `Valor Singular` = `Valor.Singular`,
             `Inércia Principal Parcial eigenvalues` = Inércia.Principal.Parcial.eigenvalues) %>%
      select(Dimensão, `Valor Singular`, `Inércia Principal Parcial eigenvalues`,
             Qui2, `Percentual da Inércia Principal Total`,
             `Percentual da Inércia Principal Total Acumulada`) %>%
      kable() %>%
      kable_styling(bootstrap_options = "striped", 
                    full_width = FALSE, 
                    font_size = 17)
    

# Mapa Perceptual---------------------------------------------------------------

# 1. Coordenadas para plotar as categorias no mapa perceptual
      
  # Variável em linha na tabela de contingência ('perfil')
      # Coordenadas das abcissas
      coord_abcissas_perfil <- sqrt(valores_singulares[1]) * (massa_colunas^-0.5) * autovetor_u[,1]
      coord_abcissas_perfil
      
      # Coordenadas das ordenadas
      coord_ordenadas_perfil <- sqrt(valores_singulares[2]) * (massa_colunas^-0.5) * autovetor_u[,2]
      coord_ordenadas_perfil
      
  # Variável em coluna na tabela de contingência ('aplicacao')
      # Coordenadas das abcissas
      coord_abcissas_aplicacao <- sqrt(valores_singulares[1]) * (massa_linhas^-0.5) * autovetor_v[,1]
      coord_abcissas_aplicacao
      
      # Coordenadas das ordenadas
      coord_ordenadas_aplicacao <- sqrt(valores_singulares[2]) * (massa_linhas^-0.5) * autovetor_v[,2]
      coord_ordenadas_aplicacao
      
# 2. Mapa perceptual
      cbind.data.frame(coord_abcissas_perfil, coord_ordenadas_perfil,
                       coord_abcissas_aplicacao, coord_ordenadas_aplicacao) %>%
        rename(dim_1_perfil = 1,
               dim_2_perfil = 2,
               dim_1_aplicacao = 3,
               dim_2_aplicacao = 4) %>%
        rownames_to_column() %>%
        setNames(make.names(names(.), unique = TRUE)) %>%
        mutate(aplicacao = rownames(data.frame(coord_abcissas_aplicacao,
                                               coord_ordenadas_aplicacao))) %>%
        rename(perfil = 1,
               dim_1_perfil = 2,
               dim_2_perfil = 3,
               dim_1_aplicacao = 4,
               dim_2_aplicacao = 5) %>%
        ggplot() +
        geom_point(aes(x = dim_1_perfil, y = dim_2_perfil),
                   color = "deeppink1",
                   fill = "deeppink1",
                   shape = 24,
                   size = 4) +
        geom_text_repel(aes(x = dim_1_perfil, y = dim_2_perfil, label = perfil)) +
        geom_point(aes(x = dim_1_aplicacao, y = dim_2_aplicacao),
                   color = "turquoise3",
                   fill = "turquoise3",
                   shape = 21,
                   size = 4) +
        geom_text_repel(aes(x = dim_1_aplicacao, y = dim_2_aplicacao, label = aplicacao)) +
        geom_vline(aes(xintercept = 0), linetype = "longdash", color = "grey48") +
        geom_hline(aes(yintercept = 0), linetype = "longdash", color = "grey48") +
        labs(x = paste("Dimensão 1:", paste0(round(variancia_explicada[1] * 100, 2),"%")),
             y = paste("Dimensão 2:", paste0(round(variancia_explicada[2] * 100, 2),"%"))) +
        theme_bw()
      

# Função CA: pacote FactoMineR --------------------------------------------
# O mesmo resultado pode ser obtido com a função 'CA' do pacote 'FactoMineR'
library(FactoMineR)
anacor <- CA(tabela_contingencia, graph = TRUE)
      
# OBS: Note que a função 'CA' gera um mapa perceptual construído com coordenadas
#      definidas de maneira diferente em relação às calculadas antes.Entretanto, 
#      as proporções das proximidades entre as categorias das variáveis
#      permanecem as mesmas, assim como os percentuais da inércia principal 
#      total por dimensão!
      
#####    
# Interpretação do Mapa Perceptual
#> a proximidade dos pontos no gráfico, nos diz as associações
#> Reflete o que encontramos nas tabelas anteriores, mas de forma visual    



# xx Análise de Correspondencia Multipla (ACM)------------------------------

# Objetivo: Associação entre mais de duas variáveis.

# - Só participam da ACM as variáveis que apresentam associação estatisticamente
#   significativa com pelo menos um outra variável contida na análise.
# - Antes de elaborar a ACM é importante realizar um teste X² para cada par
#   de variável;
# - Se alguma delas não apresentar associação com outras, não é incluída na 
#   análise de correspondencia.

#     Etapa 1. Tabela de Frequencia
#     Etapa 2. Tabela de Contingencia
#     Etapa 3. Matriz Binária e Matriz de Burt

# Dados -------------------------------------------------------------------
load(file = "raw_datas_exemplos/perfil_investidor_aplicacao_estadocivil.RData")


# Visualização da base de dados -------------------------------------------
perfil_investidor_aplicacao_estadocivil %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE,
                font_size = 20)


# ETAPA 1. Tabelas de Frequencia ---------------------------------------------------
# Tabelas de frequência das variáveis qualitativas
summary(perfil_investidor_aplicacao_estadocivil)


# ETAPA 2. Tabelas de contingência -------------------------------------------------
# Tabela de contingencia com cada par de variável. Para ver a associação 
# entre cada uma delas. Aqui fazemos o teste qui quadrado para cada par de 
# variável. Somente as variáveis que forem associadas a alguma outra, irão
# seguir na analise.

# Perfil x Aplicação
sjt.xtab(var.row = perfil_investidor_aplicacao_estadocivil$perfil,
         var.col = perfil_investidor_aplicacao_estadocivil$aplicacao,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)

# Perfil x Estado Civil
sjt.xtab(var.row = perfil_investidor_aplicacao_estadocivil$perfil,
         var.col = perfil_investidor_aplicacao_estadocivil$estado_civil,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)

# Aplicação x Estado Civil
sjt.xtab(var.row = perfil_investidor_aplicacao_estadocivil$aplicacao,
         var.col = perfil_investidor_aplicacao_estadocivil$estado_civil,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)





# ETAPA 3. Matriz ---------------------------------------------------------
# Na análise simples, fazemos basicamente com base na tabela de contingencia.
# Só que nesse caso, teremos mais de uma tabela de contingencia e precisamos
# agrupar todas elas em uma única tabela.

#1º Método: utilizando Matriz Binária
#> A Matriz binária é obtida pela transformação das variáveis qualitativas em
#> variáveis binárias, ou seja, 0 e 1. Com base na matriz binária (Z) pode
#> ser obtida a inércia principal total na ACM. Supondo que a matriz binária 
#> seja semelhante a uma tabela de contingência da Anacor, é possível obter a 
#> inércia principal das dimensões, autovalores, autovetores e coordenadas
#> dessa matriz.

#> Quantidade de dimensões: é a quantidade total de categorias menos a quantidade
#> de variáveis

# Para a Matriz binária
matriz_binaria <- matlogic(perfil_investidor_aplicacao_estadocivil[,2:4])
matriz_binaria

# 2º Método: utilizando a Matriz de Burt
#> É possível combinar em uma única matriz as tabelas de contingência com o 
#> cruzamento de todos os pares variáveis. AO considerar a matriz de Burt uma
#> tabela de contingencia, é possível realizar uma anacor e obter as coordenadas
#> das categorias

# Para a matriz de Burt
matriz_burt <- burt(perfil_investidor_aplicacao_estadocivil[,2:4])
matriz_burt
verifica_burt <- t(matriz_binaria) %*% matriz_binaria


# Analise -----------------------------------------------------------------
# Elaboração da análise de correspondência múltipla (ACM)
# Função para a análise múltipla dudi.acm(). Onde inserimos a tabela do 
# banco de dados para rodar acm
ACM <- dudi.acm(perfil_investidor_aplicacao_estadocivil[,2:4], scannf = FALSE)

# Visualização das coordenadas principais das categorias das variáveis
# Método da matriz de Burt B (componente 'co' do objeto 'ACM')
# Comp1 e Comp2: são as coordenadas no eixo X e Y, respectivamente
round(ACM$co, 3) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = FALSE,
                font_size = 20)

# Visualização das coordenadas-padrão das categorias das variáveis
# Método da matriz binária (componente 'c1' do objeto 'ACM')
# CS1 e CS2: são as coordenadas no eixo X e Y, respectivamente
round(ACM$c1, 3) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = FALSE,
                font_size = 20)

# Massas das linhas e colunas (componente 'cw' do objeto 'ACM')
# Proporção para cada categoria, conforme na analise simples.
ACM$cw

# Inércias principais (componente 'eig' do objeto 'ACM')
# eigenvalues de cada dimensão. Há cinco dimensões, pq é número de 
# categorias - variáveis.
## Portanto, há 5 dimensões. J-Q = 8-3 = 5 dimensões
ACM$eig


# Percentual de variância explicada por dimensão
# 36% da variancia explicada no eixo X
# 26 da variância explicada no eixo y
perc_variancia <- (ACM$eig / sum(ACM$eig)) * 100
perc_variancia

# Visualização do percentual de variância explicada por dimensão
# Duas primeiras dimensões explicam mais 
data.frame(Dimensão = paste("Dimensão", 1:length(perc_variancia)),
           Variância = perc_variancia) %>%
  ggplot(aes(x = Dimensão,
             y = Variância,
             label = paste0(round(Variância, 2),"%"))) +
  geom_bar(stat = "identity", fill = "cyan") +
  geom_text(vjust = 2.5, size = 5) +
  theme_bw()

# Mapa perceptual na ACM

# Definição da quantidade de categorias de cada variável qualitativa
# Contar a quantidade de variável para plotar o mapa
#input: banco de dados com as variáveis qualitativa
quant_categorias <- apply(perfil_investidor_aplicacao_estadocivil[,2:4],
                          MARGIN =  2,
                          FUN = function(x) nlevels(as.factor(x)))

# Consolidando as coordenadas-padrão obtidas por meio da matriz binária ('c1')
df_ACM <- data.frame(ACM$c1, Variável = rep(names(quant_categorias),
                                            quant_categorias))

# Visualizando as coordenadas
df_ACM %>%
  rownames_to_column() %>%
  rename(Categoria = 1) %>%
  mutate(Categoria = gsub("perfil.","", Categoria),
         Categoria = gsub("aplicacao.","", Categoria),
         Categoria = gsub("estado_civil.","", Categoria)) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = FALSE,
                font_size = 20)

# Plotando o mapa perceptual
# Mapa é sempre interpretado utilizando a proximidade dos pontos
# quanto mais próximo, mais associado são os pontos.
# interpretação: perfil agressivo tende a investir em ações, e em geral são solteiros
# o perfil moderado, que em geral são casados, investem em CDB. Os conservadores
# investem em poupança
df_ACM %>%
  rownames_to_column() %>%
  rename(Categoria = 1) %>%
  mutate(Categoria = gsub("perfil.","", Categoria),
         Categoria = gsub("aplicacao.","", Categoria),
         Categoria = gsub("estado_civil.","", Categoria)) %>%
  ggplot(aes(x = CS1, y = CS2, label = Categoria, color = Variável)) +
  geom_point() +
  geom_label_repel() +
  geom_vline(aes(xintercept = 0), linetype = "longdash", color = "grey48") +
  geom_hline(aes(yintercept = 0), linetype = "longdash", color = "grey48") +
  labs(x = paste("Dimensão 1:", paste0(round(perc_variancia[1], 2), "%")),
       y = paste("Dimensão 2:", paste0(round(perc_variancia[2], 2), "%"))) +
  scale_color_manual("Variável",
                     values = c("turquoise3", "springgreen4", "deeppink1")) +
  theme_bw()

# Coletando as coordenadas das observações
# Todas as observações q constam no banco de dados tem uma posição no eixo 1 e
# eixo 2. Então a partir das variáveis qualitativas, geramos variávei quantitativas.
ACM_observacoes_df <- data.frame(ACM$li)

# Vamos acrescentar as informações das observacões ao mapa perceptual da ACM
# Conseguimos observar quais são as observações que possuem as categorias que foram
# analisadas na ACM.
ACM_observacoes_df %>% 
  ggplot(aes(x = Axis1, y = Axis2, label = perfil_investidor_aplicacao$estudante)) +
  geom_point(shape = 17, color = "red", size = 2) +
  geom_hline(yintercept = 0, linetype = "longdash", color = "grey48") +
  geom_vline(xintercept = 0, linetype = "longdash", color = "grey48") +
  geom_text_repel(max.overlaps = 100, size = 3) +
  geom_density2d(color = "gray") +
  geom_label_repel(data = df_ACM, 
                   aes(x = CS1, y = CS2, 
                       label = rownames(df_ACM), 
                       fill = Variável), 
                   color = "white") +
  labs(x = paste("Dimensão 1:", paste0(round(perc_variancia[1], 2), "%")),
       y = paste("Dimensão 2:", paste0(round(perc_variancia[2], 2), "%"))) +
  scale_fill_viridis_d() +
  theme(panel.background = element_rect("white"),
        panel.border = element_rect("NA"),
        panel.grid = element_line("gray95"),
        legend.position = "none")




# xxxxxxxxxxxxxxxxxxxxxxxxx ---------------------------------------------
# EXEMPLO DE APLICAÇÃO ----------------------------------------------------
# Fonte Dados
# Fonte: https://www.kaggle.com/code/jiagengchang/heart-disease-multiple-correspondence-analysis

# Pacotes
# Instalação e carregamento dos pacotes utilizados
pacotes <- c("plotly", 
             "tidyverse", 
             "ggrepel",
             "knitr", "kableExtra", 
             "sjPlot", 
             "FactoMineR", 
             "amap", 
             "ade4",
             "readxl")

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
# Importando a base de dados
# Dados sobre pessoas onde se registram informações relacionadas a problemas
# cardíacos. A variável Doenca_card indica se a pessoa teve ou não problema 
# cardíaco. Temos algumas variáveis qualitativas e outras quantitativas.
# se quisermos avaliar todas as variáveis, teremos que tratar as variáveis 
# quantitativas.
dados_cor <- read_excel("raw_datas_exemplos/dados_cor_acm.xlsx")



# Categorizar Variáveis Quantitativas -------------------------------------
# Vamos categorizar por critério estatístico, utilizando os quartis para isso.
# Poderia usar critérios técnicos, saber que um valor acima de X para 
# uma variável seria mais arriscado.
# quartil 25%: quantile (0.25)
# quartil 75%: quantile (0.75)

# idade
dados_cor <-  dados_cor |>  
              mutate(Categ_Idade = case_when(
                Idade <= quantile(Idade, 0.25, na.rm = T) ~ "menores_idades",
                Idade > quantile(Idade, 0.25, na.rm = T) &
                Idade <= quantile(Idade, 0.75, na.rm = T) ~ "idades_médias",
                Idade > quantile(Idade, 0.75, na.rm = T) ~ "maiores_idades"))

# Ps_Descanso
dados_cor <- 
  dados_cor |>  
  mutate(Categ_PS_Desc = case_when(
    PS_Descanso <= quantile(PS_Descanso, 0.25, na.rm = T) ~ "PS_descanso_baixo",
    PS_Descanso > quantile(PS_Descanso, 0.25, na.rm = T) &
    PS_Descanso <= quantile(PS_Descanso, 0.75, na.rm = T) ~ "PS_descanso_médio",
    PS_Descanso > quantile(PS_Descanso, 0.75, na.rm = T) ~ "PS_descanso_alto"))

# Colesterol
dados_cor <-
  dados_cor  |>  
  mutate(Categ_Colest = case_when(
    Colesterol <= quantile(Colesterol, 0.25, na.rm = T) ~ "menor_colesterol",
    Colesterol > quantile(Colesterol, 0.25, na.rm = T) & 
    Colesterol <= quantile(Colesterol, 0.75, na.rm = T) ~ "colesterol_médio",
    Colesterol > quantile(Colesterol, 0.75, na.rm = T) ~ "maior_colesterol"))

# BC_Max
dados_cor <-
  dados_cor |>  
  mutate(Categ_BC_Max = case_when(
    BC_Max <= quantile(BC_Max, 0.25, na.rm = T) ~ "menor_BC_Max",
    BC_Max > quantile(BC_Max, 0.25, na.rm = T) & 
    BC_Max <= quantile(BC_Max, 0.75, na.rm = T) ~ "BC_Max_médio",
    BC_Max > quantile(BC_Max, 0.75, na.rm = T) ~ "maior_BC_Max"))


# Vamos remover as variáveis que não utilizaremos (quantitativas)
dados_cor <- dados_cor|>  
  select(-Idade, -PS_Descanso, -Colesterol, -BC_Max)


# Análise ACM -------------------------------------------------------------

# 1. Transformação: 
# A função para a criação da ACM pede que sejam utilizados "fatores"
dados_cor <- as.data.frame(unclass(dados_cor), stringsAsFactors=TRUE)
str(dados_cor)        

# 2. Tabela Contingencia
# Tabelas de contingência (todas apresentam associação com alguma variável?)
# Avaliando se a variável tem alguma associação com a doenca cardiaca
# Na barra da tabela de contingencia temos o valor do teste qui-quadrado
# para saber se há associação estatistica (p<0.05)
sjt.xtab(var.row = dados_cor$Doença_Card,
         var.col = dados_cor$Sexo,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE, 
         encoding = "UTF-8")

sjt.xtab(var.row = dados_cor$Doença_Card,
         var.col = dados_cor$Tipo_Dor_Peito,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE, 
         encoding = "UTF-8")

sjt.xtab(var.row = dados_cor$Doença_Card,
         var.col = dados_cor$Açucar_Sangue,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE,
         encoding = "UTF-8")

sjt.xtab(var.row = dados_cor$Doença_Card,
         var.col = dados_cor$ECG_Descanso,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE,
         encoding = "UTF-8")

sjt.xtab(var.row = dados_cor$Doença_Card,
         var.col = dados_cor$Angina_Exerc,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE,
         encoding = "UTF-8")

sjt.xtab(var.row = dados_cor$Doença_Card,
         var.col = dados_cor$Categ_Idade,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE,
         encoding = "UTF-8")

sjt.xtab(var.row = dados_cor$Doença_Card,
         var.col = dados_cor$Categ_PS_Desc,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE,
         encoding = "UTF-8")

sjt.xtab(var.row = dados_cor$Doença_Card,
         var.col = dados_cor$Categ_Colest,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE, 
         encoding = "UTF-8")

sjt.xtab(var.row = dados_cor$Doença_Card,
         var.col = dados_cor$Categ_BC_Max,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE, 
         encoding = "UTF-8")


# 3. Análise de Correspondencia
ACM <- dudi.acm(dados_cor, scannf = FALSE)
ACM$cw

# Analisando as variâncias de cada dimensão
perc_variancia <- (ACM$eig / sum(ACM$eig)) * 100
paste0(round(perc_variancia,2),"%")

# Quantidade de categorias por variável
quant_categorias <- apply(dados_cor,
                          MARGIN =  2,
                          FUN = function(x) nlevels(as.factor(x)))

# Consolidando as coordenadas-padrão obtidas por meio da matriz binária
# Extrair as coordenadas para serem plotadas ($C1 do resultado da ACM)
df_ACM <- data.frame(ACM$c1, Variável = rep(names(quant_categorias),
                                            quant_categorias))

# 4. Mapa perceptual
df_ACM  |> 
  rownames_to_column()  |> 
  rename(Categoria = 1) |> 
  ggplot(aes(x = CS1, y = CS2, label = Categoria, color = Variável)) +
  geom_point() +
  geom_label_repel() +
  geom_vline(aes(xintercept = 0), linetype = "longdash", color = "grey48") +
  geom_hline(aes(yintercept = 0), linetype = "longdash", color = "grey48") +
  labs(x = paste("Dimensão 1:", paste0(round(perc_variancia[1], 2), "%")),
       y = paste("Dimensão 2:", paste0(round(perc_variancia[2], 2), "%"))) +
  theme_bw()


# 4. Mapa perceptual (opção2)

# Mapa perceptual com as coordenadas obtidas por meio da matriz de Burt
# Consolidando as coordenadas-padrão obtidas por meio da matriz de Burt
df_ACM_B <- data.frame(ACM$co, Variável = rep(names(quant_categorias),
                                              quant_categorias))

# Plotando o mapa perceptual
df_ACM_B %>%
  rownames_to_column() %>%
  rename(Categoria = 1) %>%
  ggplot(aes(x = Comp1, y = Comp2, label = Categoria, color = Variável)) +
  geom_point() +
  geom_label_repel() +
  geom_vline(aes(xintercept = 0), linetype = "longdash", color = "grey48") +
  geom_hline(aes(yintercept = 0), linetype = "longdash", color = "grey48") +
  labs(x = paste("Dimensão 1:", paste0(round(perc_variancia[1], 2), "%")),
       y = paste("Dimensão 2:", paste0(round(perc_variancia[2], 2), "%"))) +
  theme_bw()

# É possível obter as coordenadas das observações
df_coord_obs <- ACM$li

# Plotando o mapa perceptual
df_coord_obs %>%
  ggplot(aes(x = Axis1, y = Axis2, color = dados_cor$Doença_Card)) +
  geom_point() +
  geom_vline(aes(xintercept = 0), linetype = "longdash", color = "grey48") +
  geom_hline(aes(yintercept = 0), linetype = "longdash", color = "grey48") +
  labs(x = paste("Dimensão 1:", paste0(round(perc_variancia[1], 2), "%")),
       y = paste("Dimensão 2:", paste0(round(perc_variancia[2], 2), "%")),
       color = "Doença Cardíaca") +
  theme_bw()










# xxxxxxxxxxxxxxxxxxxxxxxxx ---------------------------------------------
# EXEMPLO DE APLICAÇÃO 2 --------------------------------------------------
# Utilizando o resultado da análise de cluster como variável dentro de
# uma análise de correspondencia

# Fonte dos dados: https://www.kaggle.com/datasets/kaushiksuresh147/customer-segmentation


# Pacotes
pacotes <- c("plotly", 
             "tidyverse", 
             "ggrepel",
             "knitr", "kableExtra", 
             "sjPlot", 
             "FactoMineR", 
             "amap", 
             "ade4",
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


# Dados
load("raw_datas_exemplos/segmenta.Rdata")
str(segmenta)
glimpse(segmenta)
summary(segmenta)


# Descritivo


# Exclusão de NAs
segmenta <- drop_na(segmenta)


# Tipo de variáveis
# Nesta aplicação vamos separar o banco de dados em qualitativas e quantitativas
str(segmenta)
segmenta_quali <- segmenta[,c(1,2,4,5)]
segmenta_quanti <- segmenta[,c(3,6)]

# Transformar qualitativas para fatores. Poris a função para a criação da 
# ACM exige que sejam utilizados "fatores"
segmenta_quali <- as.data.frame(unclass(segmenta_quali), stringsAsFactors=TRUE)
class(segmenta_quali$Gender)
class(segmenta_quali$Ever_Married)

# Estatísticas descritivas dos dados
# Note que análise descritiva de dados qualitativos envolve tabelas de frequencia
summary(segmenta_quanti)
summary(segmenta_quali)


# Analise Cluster ---------------------------------------------------------
# O cluster será usado nas variáveis quantitativas e qualitativa


# 1. Padronizar (z-score)
segm_pad <- as.data.frame(scale(segmenta_quanti))

# Para checar se o banco de dados está padronizado:
# Note que o banco terá média 0
round(mean(segm_pad$Age), 3)
round(mean(segm_pad$Family_Size), 3)
# Note que o banco terá desvio padrão 1
round(sd(segm_pad$Age), 3)
round(sd(segm_pad$Family_Size), 3)

# 2. Identificação do número adequado de clusters
#    Método de Elbow
#    Escolher onde temos uma quebra maior (pode ser 5, neste caso)
fviz_nbclust(segm_pad, kmeans, method = "wss", k.max = 15)

# 3. Análise de Cluster
# Elaboração da clusterização não hieráquica k-means
cluster_kmeans <- kmeans(segm_pad,
                         centers = 5)  # quantidade de clusters

# Criando variável categórica para indicação do cluster no banco de dados
segm_pad$cluster_K <- factor(cluster_kmeans$cluster)
segmenta$cluster_K <- factor(cluster_kmeans$cluster)

# 4. ANOVA
# Análise de variância de um fator (ANOVA). Interpretação do output:

## Mean Sq do cluster_K: indica a variabilidade entre grupos
## Mean Sq dos Residuals: indica a variabilidade dentro dos grupos
## F value: estatística de teste (Sum Sq do cluster_K / Sum Sq dos Residuals)
## Pr(>F): p-valor da estatística 
## p-valor < 0.05: pelo menos um cluster apresenta média estatisticamente diferente dos demais

## A variável mais discriminante dos grupos contém maior estatística F (e significativa)

# ANOVA da variável 'Age'
summary(anova_Age <- aov(formula = Age ~ cluster_K,
                         data = segm_pad))

# ANOVA da variável 'Family_Size'
summary(anova_Family_Size <- aov(formula = Family_Size ~ cluster_K,
                                 data = segm_pad))

# 5. Estatisticas Descritivas por Cluster
# Estatísticas descritivas para as variáveis originais 
## 'Age'
group_by(segmenta, cluster_K) %>%
  summarise(
    mean = mean(Age, na.rm = TRUE),
    sd = sd(Age, na.rm = TRUE),
    min = min(Age, na.rm = TRUE),
    max = max(Age, na.rm = TRUE),
    obs = n())
# Conclusão: Cluster 3, tem pessoas novas, com média 25 de idade ...


## 'Family_Size'
group_by(segmenta, cluster_K) %>%
  summarise(
    mean = mean(Family_Size, na.rm = TRUE),
    sd = sd(Family_Size, na.rm = TRUE),
    min = min(Family_Size, na.rm = TRUE),
    max = max(Family_Size, na.rm = TRUE),
    obs = n())

## 'Gender'
group_by(segmenta, cluster_K) %>%
  count(Gender) %>%
  mutate(prop = prop.table(n))

## 'Ever_Maried'
group_by(segmenta, cluster_K) %>%
  count(Ever_Married) %>%
  mutate(prop = prop.table(n))

## 'Graduated'
group_by(segmenta, cluster_K) %>%
  count(Graduated) %>%
  mutate(prop = prop.table(n))

## 'Spending_Score'
group_by(segmenta, cluster_K) %>%
  count(Spending_Score) %>%
  mutate(prop = prop.table(n))



# Análise ACM ---------------------------------------------------------------------
# Análise de Correspondência Múltipla nas variáveis qualitativas
# Devemos manter na análise somente as variáveis qualitativas que possuem
# associação com alguma outra variável

# Adicionando variável qualitativa que indica o cluster
segmenta_quali$cluster_K <- factor(cluster_kmeans$cluster)

# Estatísticas descritivas
summary(segmenta_quali)

# Tabelas de contingência
# qui-quadrado<0.05: existe associação (=pode entrar na ACM)
sjt.xtab(var.row = segmenta_quali$Spending_Score,
         var.col = segmenta_quali$Gender,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE, 
         encoding = "UTF-8")

sjt.xtab(var.row = segmenta_quali$Spending_Score,
         var.col = segmenta_quali$Ever_Married,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE, 
         encoding = "UTF-8")

sjt.xtab(var.row = segmenta_quali$Spending_Score,
         var.col = segmenta_quali$Graduated,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE,
         encoding = "UTF-8")

sjt.xtab(var.row = segmenta_quali$Spending_Score,
         var.col = segmenta_quali$cluster_K,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE,
         encoding = "UTF-8")

# Vamos gerar a ACM (para 3 eixos, ou seja para os 3 primeiros fatores)
ACM <- dudi.acm(segmenta_quali, scannf = FALSE, nf = 3)

# Analisando as variâncias de cada dimensão
# Gerou 9 autovalores, mas vamos selecionar só os 3 primeiros fatores (dimensão/componente)
perc_variancia <- (ACM$eig / sum(ACM$eig)) * 100
perc_variancia

# Quantidade de categorias por variável
quant_categorias <- apply(segmenta_quali,
                          MARGIN =  2,
                          FUN = function(x) nlevels(as.factor(x)))

# Consolidando as coordenadas-padrão obtidas por meio da matriz binária
df_ACM <- data.frame(ACM$c1, Variável = rep(names(quant_categorias),
                                            quant_categorias))

# Plotando o mapa perceptual
df_ACM %>%
  rownames_to_column() %>%
  rename(Categoria = 1) %>%
  ggplot(aes(x = CS1, y = CS2, label = Categoria, color = Variável)) +
  geom_point() +
  geom_label_repel() +
  geom_vline(aes(xintercept = 0), linetype = "longdash", color = "grey48") +
  geom_hline(aes(yintercept = 0), linetype = "longdash", color = "grey48") +
  labs(x = paste("Dimensão 1:", paste0(round(perc_variancia[1], 2), "%")),
       y = paste("Dimensão 2:", paste0(round(perc_variancia[2], 2), "%"))) +
  theme_bw()

# Mapa perceptual em 3D (3 primeiras dimensões)
ACM_3D <- plot_ly()

# Adicionando as coordenadas
ACM_3D <- add_trace(p = ACM_3D,
                    x = df_ACM$CS1,
                    y = df_ACM$CS2,
                    z = df_ACM$CS3,
                    mode = "text",
                    text = rownames(df_ACM),
                    textfont = list(color = "blue"),
                    marker = list(color = "red"),
                    showlegend = FALSE)

ACM_3D



# xxxxxxxxxxxxxxxxxxxxxxxxx ---------------------------------------------
# EXEMPLO DE APLICAÇÃO 3 ---------------------------------------------------------------
# Análise de Correspondência Múltipla + Análise Fatorial PCA
# Fonte: https://www.kaggle.com/datasets/elakiricoder/jiffs-house-price-prediction-dataset


# Pacotes
pacotes <- c("plotly",
             "tidyverse",
             "ggrepel",
             "knitr", 
             "kableExtra",
             "reshape2",
             "PerformanceAnalytics", 
             "psych",
             "ltm", 
             "Hmisc",
             "readxl",
             "sjPlot",
             "ade4")

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
casas <- read_xlsx("raw_datas_exemplos/Preco Casas.xlsx")


# Separação das variáveis em qualitativas e quantitativas
var_quali <- casas[,c(5,6,7,8,10,12,15)]
var_quanti <- casas[,c(1,2,3,4,9,11,13,14)]

## Nota: vamos deixar a variável "valor da casa" fora da análise por enquanto
## O objetivo é criar um ranking que capture os valores das casas



# Tipo de dados: transformar em fatores
# A função para a criação da ACM pede que sejam utilizados "fatores"
var_quali <- as.data.frame(unclass(var_quali), stringsAsFactors=TRUE)



# Ajustando variáveis quantitativas que estão como textos
var_quanti$distance_to_school <- as.double(var_quanti$distance_to_school)
var_quanti$distance_to_supermarket_km <- as.double(var_quanti$distance_to_supermarket_km)
var_quanti$crime_rate_index <- as.double(var_quanti$crime_rate_index)

# Estatísticas descritivas
summary(var_quali)
summary(var_quanti)



# Análise ACM -----------------------------------------------------------
# Iniciando a Análise de Correspondência Múltipla nas variáveis qualitativas

# Tabelas de contingência
sjt.xtab(var.row = var_quali$large_living_room,
         var.col = var_quali$parking_space,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE, 
         encoding = "UTF-8")

sjt.xtab(var.row = var_quali$large_living_room,
         var.col = var_quali$front_garden,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE, 
         encoding = "UTF-8")

sjt.xtab(var.row = var_quali$large_living_room,
         var.col = var_quali$swimming_pool,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE, 
         encoding = "UTF-8")

sjt.xtab(var.row = var_quali$large_living_room,
         var.col = var_quali$wall_fence,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE, 
         encoding = "UTF-8")

sjt.xtab(var.row = var_quali$large_living_room,
         var.col = var_quali$water_front,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE, 
         encoding = "UTF-8")

sjt.xtab(var.row = var_quali$large_living_room,
         var.col = var_quali$room_size_class,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE, 
         encoding = "UTF-8")

# Análise de Correspondência Múltipla
ACM <- dudi.acm(var_quali, scannf = FALSE)

# Analisando as variâncias de cada dimensão
perc_variancia <- (ACM$eig / sum(ACM$eig)) * 100
perc_variancia

# Quantidade de categorias por variável
quant_categorias <- apply(var_quali,
                          MARGIN =  2,
                          FUN = function(x) nlevels(as.factor(x)))

# Consolidando as coordenadas-padrão obtidas por meio da matriz binária
df_ACM <- data.frame(ACM$c1, Variável = rep(names(quant_categorias),
                                            quant_categorias))

# Plotando o mapa perceptual
df_ACM %>%
  rownames_to_column() %>%
  rename(Categoria = 1) %>%
  ggplot(aes(x = CS1, y = CS2, label = Categoria, color = Variável)) +
  geom_point() +
  geom_label_repel() +
  geom_vline(aes(xintercept = 0), linetype = "longdash", color = "grey48") +
  geom_hline(aes(yintercept = 0), linetype = "longdash", color = "grey48") +
  labs(x = paste("Dimensão 1:", paste0(round(perc_variancia[1], 2), "%")),
       y = paste("Dimensão 2:", paste0(round(perc_variancia[2], 2), "%"))) +
  theme_bw()

# Obtendo as coordenadas das observações
coord_obs <- ACM$li

# Adicionando as coordenadas ao banco de dados de variáveis quantitativas
var_quanti <- bind_cols(var_quanti, coord_obs)

# Coeficientes de correlação de Pearson para cada par de variáveis
rho <- rcorr(as.matrix(var_quanti), type="pearson")

corr_coef <- rho$r # Matriz de correlações
corr_sig <- round(rho$P, 5) # Matriz com p-valor dos coeficientes

# Elaboração de um mapa de calor das correlações de Pearson entre as variáveis
ggplotly(
  var_quanti %>%
    cor() %>%
    melt() %>%
    rename(Correlação = value) %>%
    ggplot() +
    geom_tile(aes(x = Var1, y = Var2, fill = Correlação)) +
    geom_text(aes(x = Var1, y = Var2, label = format(round(Correlação,3))),
              size = 3) +
    scale_fill_viridis_b() +
    labs(x = NULL, y = NULL) +
    theme_bw(base_size = 6))

### Elaboração a Análise Fatorial Por Componentes Principais ###

# Teste de esfericidade de Bartlett
cortest.bartlett(var_quanti)

# Elaboração da análise fatorial por componentes principais
fatorial <- principal(var_quanti,
                      nfactors = length(var_quanti),
                      rotate = "none",
                      scores = TRUE)
fatorial

# Eigenvalues (autovalores)
eigenvalues <- round(fatorial$values, 5)
eigenvalues
round(sum(eigenvalues), 2) # soma dos autovalores

# Identificação da variância compartilhada em cada fator
variancia_compartilhada <- as.data.frame(fatorial$Vaccounted) %>% 
  slice(1:3)

rownames(variancia_compartilhada) <- c("Autovalores",
                                       "Prop. da Variância",
                                       "Prop. da Variância Acumulada")

# Variância compartilhada pelas variáveis originais para a formação de cada fator
round(variancia_compartilhada, 3) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE, 
                font_size = 20)

### Elaboração da Análise Fatorial por Componentes Principais ###
### Fatores extraídos a partir de autovalores maiores que 1 ###

# Definição da quantidade de fatores com eigenvalues maiores que 1
k <- sum(eigenvalues > 1)
print(k)

# Elaboração da análise fatorial por componentes principais
fatorial_final <- principal(var_quanti,
                            nfactors = k,
                            rotate = "none",
                            scores = TRUE)

# Cálculo dos scores fatoriais
scores_fatoriais <- as.data.frame(fatorial_final$weights)

# Visualização dos scores fatoriais
round(scores_fatoriais, 3) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE, 
                font_size = 20)

# Cálculo dos fatores propriamente ditos
fatores <- as.data.frame(fatorial_final$scores)
View(fatores)

# Cálculo das cargas fatoriais
cargas_fatoriais <- as.data.frame(unclass(fatorial_final$loadings))

# Visualização das cargas fatoriais
round(cargas_fatoriais, 3) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE, 
                font_size = 20)

# Cálculo das comunalidades
comunalidades <- as.data.frame(unclass(fatorial_final$communality)) %>%
  rename(comunalidades = 1)

# Visualização das comunalidades para os 2 fatores extraídos
round(comunalidades, 3) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = FALSE,
                font_size = 20)

# Loading plot com as cargas dos 2 primeiros fatores
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

# Criação de um ranking Critério da soma ponderada e ordenamento)
casas$ranking <- fatores$PC1 * variancia_compartilhada$PC1[2] +
  fatores$PC2 * variancia_compartilhada$PC2[2] +
  fatores$PC3 * variancia_compartilhada$PC3[2] + 
  fatores$PC4 * variancia_compartilhada$PC4[2]

# Ranking e valor
corr_valor <- rcorr(as.matrix(casas[,16:17]))

valor_corr_coef <- corr_valor$r # Matriz de correlações
valor_corr_sig <- round(corr_valor$P, 5) # Matriz com p-valor dos coeficientes


