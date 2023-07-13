# Correlação


# Pacotes -----------------------------------------------------------------
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


# Visualizar as relações entre variáveis ----------------------------------
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



# Mapa de Calor  ----------------------------------------------------------
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



# correlation () ----------------------------------------------------------
# A função 'correlation' do pacote 'correlation' faz com que seja estruturado um
# diagrama interessante que mostra a inter-relação entre as variáveis e a
# magnitude das correlações entre elas
# Requer instalação e carregamento dos pacotes 'see' e 'ggraph' para a plotagem
#dados
load(file = "raw_datas_exemplos/empresas.RData")
#Estatísticas univariadas
summary(empresas)

empresas %>%
  correlation(method = "pearson") %>%
  plot()

# chart.Correlation ( ) ---------------------------------------------------
#A função 'chart.Correlation' do pacote 'PerformanceAnalytics' apresenta as
#distribuições das variáveis, scatters, valores das correlações e suas
#respectivas significâncias
chart.Correlation((empresas[2:6]), histogram = TRUE)



# pairs.panels ( ) --------------------------------------------------------
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


# corr_plot ( ) -----------------------------------------------------------
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

