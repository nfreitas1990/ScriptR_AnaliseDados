
# Machine Learning
# Natália Freitas
# fevereiro 2023


#> Contextualização
#> O machine learning consiste em fazer o computador aprender
#> padrões sem programa-lo diretamente para isso. Muitos preferem
#> tratar esse tema como Modelagem Preditiva ao invés de Machine
#> Learning
#> A modelagem preditiva é um conjunto de técnica que visa gerar 
#> estimativas e predições mais precisas. O foco é na predição
#> e não na explicação do fenomeno
 
#> Machine Learning: 
#>  - Modelos Supervisionados (Classificação e Regressao): quando
#>    queremos prever um evento
#>  - Modelos Não-supervisionados (cluster e redução de dimensionalidade)
#>  - Reinforcement learning (algoritmo interage com ambiente; tem q
#>    aprender sequencias de decisões)
    
#> Interpretação
#> Conforme o modelo se torna cada vez mais flexível (complexo), perdemos
#> o poder de interpretação, mas ganhamos em desempenho. Existe um
#> tradeoff entre interpretação e desempenho. Quanto mais flexivo o 
#> o modelo, mais dados seria necessário. Então o modelo a ser usado
#> é uma escolha entre interpretação, desempenho e conjunto de dados 
#> que nós temos.
#> Importante notar que quanto mais flexivel o modelo, mais ele se ajusta 
#> ao seu conjunto de dados, em contrapartida ele perde desempenho ao 
#> lidar com um cojunto externo de dados. Então temos que ter um equílibro
#> entre a quanto o modelo se ajusta, para não perder o poder de 
#> generalizar.

#> Inferencia vs Predição
#> Na inferencia estamos interessados em entender a relação entre as 
#> variáveis explicativa e resposta. No entanto, na predição não.
#> Na predição estamos focados em acertar a predição com a maior certeza
#> possível 

#> Predição:
#> 1. Ordenar: as vezes temos interesse em ordenar (quais pessoas tem 
#>    maior chance de cancelar uma assinatura)
#> 2. Média: alguns problemas, não só a ordem interessa, mas também o 
#>    valor predito
#> 3. Variância: As vezes estamos mais interessados em saber o quanto
#>    eu confio na minha estimativa
#> 4. Explicação

#> Métrica de Erro - RMSE
#> Queremos sempre encontrar um modelo que erre o menos possível.
#> Em um modelo de predição em que o evento é numérico, usamos 
#> o Raiz do Erro Quadrático médio - Root Mean Squared error (RMSE)  
#> Dependendo do problema, podemos escolher métricas diferentes.
#> Entao sempre queremos escolher o modelo com menor RMSE, ou seja, que 
#> erre menos.
#> Temos outra métricas: Erro médio Absoluto (MAE), R-squared (R²)
#> 
#> Em geral:
#> Problemas regressao: RMSE, R², MAE, MAPE
#> Problemas classificação:Deviance, Acuracia, AUC ROC, Precision/Recall
#> Kappa, F1
#> yardstick
#> 


# Pacotes -----------------------------------------------------------------
library(tidymodels)


# Regressão Linear Simples ------------------------------------------------
linear_reg |> 
  fit(dist ~speed, data = cars)

lm(dist ~speed, data = cars)

# Regressão Linear Múltipla -----------------------------------------------
linear_reg |> 
  fit(mpg ~ wt + disp, data = mtcars)

