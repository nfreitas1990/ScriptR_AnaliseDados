

# Natália
# Modelos Lineares Generalizados Multinível (GLMM)
# Mixed Models

# 3h:14

# Contextualizacao --------------------------------------------------------
# São modelos que reconhecem a existencia de estrutura mutinivel ou hierarquica
# nos dados. 

# Os modelos tradicionais de regressão ignoram as interações entre as variáveis
# no componente de efeitos fixos e as interações entre termos de erros e variáveis
# no componente de efeitos aleatórios


# Possui vários Nomes: --------------------------------------------------
# GLMM - Modelos Lineares Generalizados Multiníveis
# GLLAM- Gen Lin. Latent Multilevel (or Mixed) Models
# Modelos Mistos - Mixed Models
# HLM - Hierarchical Linear Models
# RCM - Random Coefficients Models
# Nested Models - Modelos Aninhados

# FMM- Frite Mixture Models - quando são usados modelos diferentes para cada
# grupo (nível) dentro da análise multiníveis. Combinando vários tipos de GLMs 

# livro: multilevel statistical models 4.ed.chichester.John wiley e sons, 2011



# Variância dos Termos Aleatórios -----------------------------------------
# Se as variâncias dos termos aleatórios voj e v1j foram estatisticamente
# diferentes de zero, procedimentos tradicionais de estimação dos parâmetros
# do modelo, como mínimos quadrados ordinários, NÃO serão adequados. Ou seja,
# as dummies só entram nos componentes de efeitos fixos. Não permitindo avalia
# ção de efeitos aleatórios de inclinação. No máximo permite diferenças nos 
# interceptos.


# Por que Utilizar? -------------------------------------------------------
#> Os modelos multiníveis permitem, portanto, o desenvolvimento de novos e mais
#> bem elaborados constructos para a predição e tomada de decisão.
#> Dentro de uma estrutura de modelo de equação unica (OLS/GLM), parece não
#> haver uma conexão entre indivíduos e a sociedade em que vivem. Neste sentido,
#> o uso de equações em níveis (GLMM), permite que o pesquisador "pule" de uma
#> ciencia para outra: alunos e escolas, famílias e bairros, firmas e países. 
#> Ignorar essas relações significa elaborar análises incorretas sobre o 
#> comportamento dos indivíduos e, igualmente, sobre os comportamentos dos 
#> grupos. Somente o reconhecimento destas recíprocas influências permite a 
#> análise correta dos fenomenos.

# Dummies -----------------------------------------------------------------
# Apenas a inserção de dummies de grupo não capturaria os efeitos contextuais
# visto que não permitiria que se separassem os efeitos observáveis dos não
# observáveis sobre a variável dependente.


# Passos:
# 1. Faço o Modelo Nulo - testo se o efeito do intercepto é significativo
# 2. Faço o Modelo 1 - comparo os modelos para ver o maior loglike. Se for o 
#    do modelo nulo com maior loglik (mais perto do zero, já que valor negativo),
#    significa que nosso modelo 1 não está bem. que só a variação do intercepto 
#    já explica toda a variação na variável resposta. 
# 3. Verificar a significancia - verificar a significancia do intercepto
#    se for significativo, vamos manter ele aletório. Verificar a significancia 
#    da inclinação, se for significativa, manter ela aleatoria tb.


# Pacotes -----------------------------------------------------------------
pacotes <- c("plotly","tidyverse","reshape2","knitr","kableExtra","rgl",
             "gghalves","ggdist","tidyquant","car","nlme","lmtest",
             "fastDummies","msm","lmeInfo","jtools","gganimate","ggridges",
             "viridis","hrbrthemes")

options(rgl.debug = TRUE)    # Importante para R-studio rodar grafico 3D

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}



# Algoritmo: Função pronta HLM2 e HLM3 ----------------------------------------
# Algoritmo para determinação dos erros-padrão das variâncias no componente de
# efeitos aleatórios. Será usada para avaliar a significância dos termos de erro

# ATENÇÃO: A função abaixo é plenamente funcional para modelos do tipo HLM2
# e HLM3, desde que estimados pelo pacote nlme

stderr_nlme <- function(model){
  if(base::class(model) != "lme"){
    base::message("Use a lme object model from nlme package")
    stop()}
  resume <- base::summary(model)
  if(base::length(base::names(model$groups))==1){
    m.type <- "HLM2"
  } else if(base::length(base::names(model$groups))==2){
    m.type <- "HLM3"
  }
  if(m.type == "HLM2"){
    vcov_matrix <- model$apVar
    logs_sd_re <- base::attr(vcov_matrix,"Pars")
    if(base::length(logs_sd_re)==2){
      stderr_tau00 <- msm::deltamethod(~exp(x1)^2,logs_sd_re,vcov_matrix)
      stderr_sigma <- msm::deltamethod(~exp(x2)^2,logs_sd_re,vcov_matrix)
      results <- base::data.frame(`RE Components`=base::c("Var(v0j)","Var(e)"),
                                  `Variance Estimatives`= base::c(base::exp(logs_sd_re)[[1]]^2,
                                                                  base::exp(logs_sd_re[[2]])^2),
                                  `Std Err.`=base::c(stderr_tau00,
                                                     stderr_sigma),
                                  z=base::c(base::exp(logs_sd_re)[[1]]^2/stderr_tau00,
                                            base::exp(logs_sd_re[[2]])^2/stderr_sigma),
                                  `p-value`=base::round(stats::pnorm(q=base::c(base::exp(logs_sd_re)[[1]]^2/stderr_tau00,
                                                                               base::exp(logs_sd_re[[2]])^2/stderr_sigma),
                                                                     lower.tail=F)*2,3))
      return(results)
    }
    else{
      stderr_tau00 <- msm::deltamethod(~exp(x1)^2,logs_sd_re,vcov_matrix)
      stderr_tau01 <- msm::deltamethod(~exp(x2)^2,logs_sd_re,vcov_matrix)
      stderr_sigma <- msm::deltamethod(~exp(x4)^2,logs_sd_re,vcov_matrix)
      results <- base::data.frame(Components=base::c("Var(v0j)","Var(v1j)","Var(e)"),
                                  Estimatives= base::c(base::exp(logs_sd_re)[[1]]^2,
                                                       base::exp(logs_sd_re[[2]])^2,
                                                       base::exp(logs_sd_re[[4]])^2),
                                  Std_Err=base::c(stderr_tau00,
                                                  stderr_tau01,
                                                  stderr_sigma),
                                  z=base::c(base::exp(logs_sd_re)[[1]]^2/stderr_tau00,
                                            base::exp(logs_sd_re[[2]])^2/stderr_tau01,
                                            base::exp(logs_sd_re[[4]])^2/stderr_sigma),
                                  `p-value`=base::round(stats::pnorm(q=base::c(base::exp(logs_sd_re)[[1]]^2/stderr_tau00,
                                                                               base::exp(logs_sd_re[[2]])^2/stderr_tau01,
                                                                               base::exp(logs_sd_re[[4]])^2/stderr_sigma),
                                                                     lower.tail=F)*2,3))
      return(results)
    }
  }
  if(m.type == "HLM3"){
    vcov_matrix <- model$apVar
    logs_sd_re <-  base::attr(vcov_matrix,"Pars")
    if(base::length(logs_sd_re) == 3){
      stderr_tau_r000 <- msm::deltamethod(~exp(x1)^2,logs_sd_re,vcov_matrix)
      stderr_tau_u000 <- msm::deltamethod(~exp(x2)^2,logs_sd_re,vcov_matrix)
      stderr_sigma <- msm::deltamethod(~exp(x3)^2,logs_sd_re,vcov_matrix)
      results <- base::data.frame(Components=base::c("Var(t00k)","Var(v0jk)","Var(e)"),
                                  Estimatives=base::c(base::exp(logs_sd_re)[[2]]^2,
                                                      base::exp(logs_sd_re)[[1]]^2,
                                                      base::exp(logs_sd_re)[[3]]^2),
                                  Std_Err=base::c(stderr_tau_u000,
                                                  stderr_tau_r000,
                                                  stderr_sigma),
                                  z=base::c(base::exp(logs_sd_re)[[2]]^2/stderr_tau_u000,
                                            base::exp(logs_sd_re)[[1]]^2/stderr_tau_r000,
                                            base::exp(logs_sd_re)[[3]]^2/stderr_sigma),
                                  `p-value`=base::round(stats::pnorm(q=base::c(base::exp(logs_sd_re)[[2]]^2/stderr_tau_u000,
                                                                               base::exp(logs_sd_re)[[1]]^2/stderr_tau_r000,
                                                                               base::exp(logs_sd_re)[[3]]^2/stderr_sigma),
                                                                     lower.tail=F)*2,3))
      return(results)
    } 
    else{
      stderr_tau_r000 <- msm::deltamethod(~exp(x1)^2,logs_sd_re,vcov_matrix)
      stderr_tau_r100 <- msm::deltamethod(~exp(x2)^2,logs_sd_re,vcov_matrix)
      stderr_tau_u000 <- msm::deltamethod(~exp(x4)^2,logs_sd_re,vcov_matrix)
      stderr_tau_u100 <- msm::deltamethod(~exp(x5)^2,logs_sd_re,vcov_matrix)
      stderr_sigma <- msm::deltamethod(~exp(x7)^2,logs_sd_re,vcov_matrix)
      results <- base::data.frame(`RE_Components`=base::c("Var(t00k)","Var(t10k)",
                                                          "Var(v0jk)","Var(v1jk)",
                                                          "Var(e)"),
                                  `Variance Estimatives`=base::c(base::exp(logs_sd_re)[[4]]^2,
                                                                 base::exp(logs_sd_re)[[5]]^2,
                                                                 base::exp(logs_sd_re)[[1]]^2,
                                                                 base::exp(logs_sd_re)[[2]]^2,
                                                                 base::exp(logs_sd_re)[[7]]^2),
                                  `Std Err.`=base::c(stderr_tau_u000,
                                                     stderr_tau_u100,
                                                     stderr_tau_r000,
                                                     stderr_tau_r100,
                                                     stderr_sigma),
                                  z=base::c(base::exp(logs_sd_re)[[4]]^2/stderr_tau_u000,
                                            base::exp(logs_sd_re)[[5]]^2/stderr_tau_u100,
                                            base::exp(logs_sd_re)[[1]]^2/stderr_tau_r000,
                                            base::exp(logs_sd_re)[[2]]^2/stderr_tau_r100,
                                            base::exp(logs_sd_re)[[7]]^2/stderr_sigma),
                                  `p-value`=base::round(stats::pnorm(q=base::c(base::exp(logs_sd_re)[[4]]^2/stderr_tau_u000,
                                                                               base::exp(logs_sd_re)[[5]]^2/stderr_tau_u100,
                                                                               base::exp(logs_sd_re)[[1]]^2/stderr_tau_r000,
                                                                               base::exp(logs_sd_re)[[2]]^2/stderr_tau_r100,
                                                                               base::exp(logs_sd_re)[[7]]^2/stderr_sigma),
                                                                     lower.tail=F)*2,3))
      return(results)
    }
  }
}




# x -----------------------------------------------------------------------
# Modelagem HLM2 com Dados Agrupados --------------------------------------
# Hierarchical Linear Model com dois níveis de agrupamento



# Dados -------------------------------------------------------------------
load(file = "raw_datas_exemplos/estudante_escola.RData")


# Visualização ------------------------------------------------------------
# Observar que a variável tempo de experiencia dos professores, se repete 
# para as observações da mesma escola. Esta variável atinge todas as pessoas
# da mesma escola de forma invariável.
estudante_escola %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 25)


# Estatísticas descritivas ------------------------------------------------
summary(estudante_escola)


# Estudo sobre o desbalanceamento dos dados 
# Quantidade de observações não é igual para cada escola. Não tem problema
estudante_escola %>% 
  group_by(escola) %>% 
  summarise(quantidade = n()) %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 25)

# Desempenho médio dos estudantes por escola 
estudante_escola %>%
  group_by(escola) %>%
  summarise(`desempenho médio` = mean(desempenho, na.rm = T)) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = F,
                font_size = 25)


# Exploração visual do desempenho médio 
estudante_escola %>%
  group_by(escola) %>%
  mutate(desempenho_medio = mean(desempenho, na.rm = TRUE)) %>% 
  ggplot() +
  geom_point(aes(x = escola, y = desempenho),color = "orange", alpha = 0.5,
             size = 4) +
  geom_line(aes(x = escola, y = desempenho_medio, 
                group = 1, color = "Desempenho Escolar Médio"), size = 1.5) +
  scale_colour_viridis_d() +
  labs(x = "Escola",
       y = "Desempenho Escolar") +
  theme(legend.title = element_blank(),
        panel.border = element_rect(NA),
        panel.grid = element_line("grey"),
        panel.background = element_rect("white"),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 90))


# Boxplot da variável dependente (desempenho) 
ggplotly(
  ggplot(estudante_escola, aes(x = "", y = desempenho)) +
    geom_boxplot(fill = "deepskyblue",    # cor da caixa
                 alpha = 0.7,             # transparência
                 color = "black",         # cor da borda
                 outlier.colour = "red",  # cor dos outliers
                 outlier.shape = 15,      # formato dos marcadores dos outliers
                 outlier.size = 2.5) +    # tamanho dos marcadores dos outliers
    geom_jitter(width = 0.1, alpha = 0.5, size = 1.3, color = "darkorchid") +
    labs(y = "Desempenho") +
    theme(panel.background = element_rect("white"),
          panel.grid = element_line("grey95"),
          panel.border = element_rect(NA),
          legend.position="none",
          plot.title = element_text(size=15)) +
    ggtitle("Boxplot da variável 'desempenho'") +
    xlab("")
)

# Kernel density estimation (KDE) 
# Função densidade de probabilidade da variável dependente (desempenho),
# com histograma

ggplotly(
  ggplot(estudante_escola, aes(x = desempenho)) +
    geom_density(aes(x = desempenho), 
                 position = "identity", color = "black", size = 1) +
    geom_histogram(aes(y = ..density..), color = "white", fill = "deepskyblue",
                   bins = 30) +
    theme_classic()
)


# Boxplot da variável dependente (desempenho) por escola
ggplotly(
  ggplot(estudante_escola, aes(x = escola,y = desempenho)) +
    geom_boxplot(aes(fill = escola, alpha = 0.7)) +
    geom_jitter(width = 0.1, alpha = 0.5, size = 1.3, color = "darkorchid") +
    scale_fill_viridis_d() +
    labs(y = "Desempenho") +
    theme_classic() +
    ggtitle("Boxplots da variável 'desempenho' para as escolas")
)


# Boxplot alternativo da variável dependente (desempenho) por escola
# pacote 'gghalves'
estudante_escola %>%
  ggplot(aes(escola, desempenho, fill = escola)) +
  gghalves::geom_half_boxplot(
    outlier.colour = "red") +
  gghalves::geom_half_dotplot(
    aes(fill = escola),
    dotsize = 0.75,
    alpha = 0.5,
    stackratio = 0.45,
    color = "black"
  ) +
  scale_fill_viridis_d() +
  theme_tq() +
  labs(title = "Boxplots da variável 'desempenho' para as escolas")


# Distribuições da variável 'desempenho' para as escolas, com boxplots
# pacote 'ggdist'
estudante_escola %>%
  ggplot(aes(x = escola, y = desempenho, fill = escola)) +
  ggdist::stat_halfeye(
    adjust = 0.5,
    justification = -.2,
    .width = 0,
    point_colour = NA
  ) +
  geom_boxplot(
    width = .12,
    outlier.color = NA,
    alpha = 0.5
  ) +
  ggdist::stat_dots(
    side = "left",
    justification = 1.1,
    binwidth = .25
  ) +
  scale_fill_viridis_d() +
  theme_tq() +
  labs(
    title = "Distribuições da variável 'desempenho' para as escolas",
    subtitle = "com boxplots",
    x = "Escola",
    y = "Desempenho") +
  coord_flip()

# Gráfico alternativo com distribuições da variável 'desempenho' para as escolas
# função 'geom_density_ridges_gradient' do pacote 'ggridges'
# option: inferno; turbo; magma; (cor)
# direction: inverter o gradiente de cor
ggplot(estudante_escola, aes(x = desempenho, y = escola, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis(name = "Desempenho", option = "turbo", direction = -1) +
  labs(
    title = "Distribuições da variável 'desempenho' para as escolas",
    x = "Desempenho",
    y = "Escola") +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 5)
  )


# Kernel density estimation (KDE) - função densidade de probabilidade da
# variável dependente (desempenho) por escola
ggplotly(
  ggplot(estudante_escola, aes(x = desempenho)) +
    geom_density(aes(color = escola, fill = escola), 
                 position = "identity", alpha = 0.3) +
    scale_color_viridis_d() +
    scale_fill_viridis_d() +
    theme_classic()
)


# Kernel density estimation (KDE) - função densidade de probabilidade da
# variável dependente (desempenho), com histograma e por escola separadamente
# (função facet_wrap)
estudante_escola %>% 
  group_by(escola) %>% 
  mutate(linhas = 1:n()) %>% 
  mutate(x = unlist(density(desempenho, n = max(linhas))["x"]),
         y = unlist(density(desempenho, n = max(linhas))["y"])) %>%
  ggplot() +
  geom_area(aes(x = x, y = y, group = escola, fill = escola), color = "black", alpha = 0.3) +
  geom_histogram(aes(x = desempenho, y = ..density.., fill = escola), 
                 color = "black", position = 'identity', alpha = 0.1) +
  facet_wrap(~ escola) +
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  theme_bw()


# Gráfico Pontos ----------------------------------------------------------
# Gráfico de desempenho x horas (OLS)
ggplotly(
  estudante_escola %>%
    ggplot(aes(x = horas, y = desempenho)) +
    geom_smooth(method = "lm", formula = y ~ x, se = F) +
    geom_point() +
    scale_colour_viridis_d() +
    labs(x = "Quantidade Semanal de Horas de Estudo do Aluno",
         y = "Desempenho Escolar") +
    theme_bw()
)


# Grafico Dinâmico --------------------------------------------------------
# Gráfico de desempenho x horas (OLS) por escola separadamente
# (funções transition_states e animate do pacote gganimate)
ggplot(estudante_escola, aes(x=horas, y=desempenho, color=escola)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = F) +
  transition_states(escola, transition_length = 1, state_length = 2) + 
  enter_fade() + 
  exit_shrink() +
  ease_aes('linear') + 
  labs(x = "Quantidade Semanal de Horas de Estudo do Aluno",
       y = "Desempenho Escolar") +
  scale_color_viridis_d() +
  ggtitle("Desempenho escolar por escola", subtitle = "Escola: {closest_state}") +
  theme_minimal() -> p

animate(p, nframes = 400, fps = 100)


#> Esse gráfico mostra que com o gráfico total de pontos não conseguiamos ver.
#> Que existe diferença de intercepto e inclinação dependendo da escola.



# Gráfico de desempenho x horas por escola (visualização do contexto)
# NOTE QUE A PERSPECTIVA MULTINÍVEL NATURALMENTE CONSIDERA O COMPORTAMENTO
# HETEROCEDÁSTICO NOS DADOS!
ggplotly(
  estudante_escola %>%
    ggplot(aes(x = horas, y = desempenho, color = escola)) +
    geom_smooth(method = "lm", formula = y ~ x, se = F) +
    geom_point() +
    guides(color = "none") +
    scale_colour_viridis_d() +
    labs(x = "Quantidade Semanal de Horas de Estudo do Aluno",
         y = "Desempenho Escolar") +
    theme_bw()
)

# O gráfico a seguir apresenta uma plotagem sob a perspectiva de um modelo
# com equação única (ex.: OLS)
base_exemplo <- estudante_escola %>%
  filter(escola %in% c("1","2","3","4","5","6")) %>%
  mutate(escola = as.numeric(escola))

scatter3d(desempenho ~ horas + escola, #função scatter3d do pacote car
          data = base_exemplo,
          fit = "linear")

# Agora plotamos o mesmo gráfico, porém de forma tridimensional,
#considerando modelos distintos para as diferentes escolas. Plotamos
# apenas as 06 primeiras escolas em razão de uma limitação do algoritmo
scatter3d(desempenho ~ horas + escola,
          groups = factor(base_exemplo$escola),
          data = base_exemplo,
          fit = "linear",
          surface = T)



# Passos: -----------------------------------------------------------------
# 1. Modelo Nulo - Verificar se há efeito aleatório no intercepto



# 1. Estimando Modelo Nulo HLM2----------------------------------------------------
# Estimação do modelo nulo é necessário para verificar se existe significancia
# de efeito aleatório no intercepto.
modelo_nulo_hlm2 <- nlme::lme(fixed = desempenho ~ 1, 
                              random = ~ 1 | escola,
                    data = estudante_escola,
                    method = "REML") 

# method: REML = restricted estimation of maximum likelihood (Gelman)
# fixed = desempenho ~ 1: sem nenhum efeito de var.explicativa
# random = ~1 | escola : ñ tem efeito aleatorio de nenhum explicativa somente do
#                        intercepto para o nível escola


# Output ------------------------------------------------------------------
# Parâmetros do modelo
summary(modelo_nulo_hlm2)

# Linear mixed-effects model fit by REML
# Data: estudante_escola 
# AIC      BIC    logLik
# 2838.015 2849.648 -1416.007
# 
# Random effects:
#   Formula: ~1 | escola
# (Intercept) Residual
# StdDev:    20.34946 11.95508
# 
# Fixed effects:  desempenho ~ 1 
#             Value     Std.Error  DF  t-value p-value
# (Intercept) 42.38711  6.468659 348 6.552689       0
# 
# Standardized Within-Group Residuals:
#   Min           Q1          Med           Q3          Max 
# -3.517143601 -0.426183104  0.004840772  0.450713210  3.567711200 


#> desempenho = 42.38711+ v0j + Erro


# Significancia do modelo nulo --------------------------------------------
# Verificando a funcionalidade da função 'stderr_nlme' desenvolvida
stderr_nlme(modelo_nulo_hlm2)

#       RE.Components Variance.Estimatives  Std.Err.   z       p.value
# 1      Var(v0j)       414.1005          197.09749  2.100993    0.036
# 2        Var(e)       142.9239          10.83481   13.191175   0.000


#> Var(v0j) p<0.05 : variância do erro aleatório. Existe diferença significativa
#> no desempenho entre alunos provenientes de escolas distintas. Quais razões?
#> Não sabemos ainda. Esse valor de 414.1005, é o mesmo que pegar o Residual
#>  20.349 e elevar ao quadrado (ou seja, variância).

#> Var(e) p<0.05: representa a variancia do erro idiossincrático (resíduo que já
#> existia nos modelos anteriores). Houve efeito significativo entre os alunos.
#> (não importa muito, o outro erro é o mais importante neste caso)


#>>> Se o parâmetro  Var(v0j) não for significante, pode ser usado um modelo
#>   GLM. Se ele for significante, temos que usar um modelo GLMM


# Importancia relativa do efeito ------------------------------------------
# ICC: (residual)^2/(var (v0j) + Var(e))

icc <- 414.1005/ (414.1005+142.9239)
icc
# [1] 0.7434154

#> Interpretação: 74% da variação no desempenho dos alunos é devido ao efeito
#> escola. Ou seja, não é culpa do aluno (das observações), é culpa da escola
#> 


# COMPARAÇÃO DO HLM2 NULO COM UM OLS NULO ---------------------------------
# Para estimarmos o modelo OLS nulo, podemos comandar o seguinte
modelo_ols_nulo <- lm(formula = desempenho ~ 1, 
                      data = estudante_escola)

# Parâmetros do modelo OLS nulo
summary(modelo_ols_nulo)

# Para comparar os LLs dos modelos, vamos utilizar a função lrtest do pacote lmtest
lrtest(modelo_ols_nulo, modelo_nulo_hlm2)

# Likelihood ratio test
# 
# Model 1: desempenho ~ 1
# Model 2: desempenho ~ 1
# #Df  LogLik Df  Chisq Pr(>Chisq)    
# 1   2 -1633.5                         
# 2   3 -1416.0  1 434.96  < 2.2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#> Interpretação: permitir que haja o efeito escola, aumenta o LogLik em mais 
#> de 200. (podemos olhar isso pelo qui-quadrado. O ganho é metade do valor do
#> qui-quadrado)
 

# Comparação entre os LLs dos modelos
data.frame(OLS_Nulo = logLik(modelo_ols_nulo),
           HLM2_Nulo = logLik(modelo_nulo_hlm2)) %>%
  rename(`OLS Nulo` = 1,
         `HLM2 Nulo` = 2) %>%
  melt() %>%
  ggplot(aes(x = variable, y = (abs(-value)), fill = factor(variable))) +
  geom_bar(stat = "identity") +
  geom_label(aes(label = (round(value,3))), hjust = 1.1, color = "white", size = 7) +
  labs(title = "Comparação do LL", 
       y = "LogLik", 
       x = "Modelo Proposto") +
  coord_flip() +
  scale_fill_manual("Legenda:",
                    values = c("grey25","grey45")) +
  theme(legend.title = element_blank(), 
        panel.background = element_rect("white"),
        legend.position = "none",
        axis.line = element_line())



#> Interpretação: Podemos ver que o Modeolo HLM2 nulo tem o maior loglik
#> (note que a escala está negativa, então a barra menor, é maior loglik).
#> Isso significa que só de permitir maior flexibilidade do intercepto já
#> melhora consideravelmente o modelo nulo.

#> Olhando para esse resultado, conseguimos concluir, mesmo sem var. preditoras
#> que parte da variação no desempenho dos alunos, é simplesmente devido a 
#> escola (usado como fator aleatório) que o aluno estudo. Parte do desempenho
#> não tem relação com o aluno, mas com o simples fato de ele estudar em uma 
#> determinada escola.  




# Obter Intervalo de confiança --------------------------------------------
intervals(modelo_final_hlm2)



# x -----------------------------------------------------------------------


# Estimação do Modelo com Intercepto Aleatório HLM2 -----------------------
# Apenas pelo modelo nulo, já podemos ver que existe efeito do intercepto.
# Agora vamos colocar 1 variável preditora

# Permitindo a variação do intercepto -------------------------------------
# Estimação do modelo com Interceptos Aleatórios
modelo_intercept_hlm2 <- lme(fixed = desempenho ~ horas, # preditor hora
                             random = ~ 1 | escola,   # efeito aleatório de intercepto
                             data = estudante_escola,
                             method = "REML")


# Summary -----------------------------------------------------------------
# Parâmetros do modelo
summary(modelo_intercept_hlm2)


# Verificar Significancia do efeito aleatorio -----------------------------
# Erros-padrão por meio da função 'stderr_nlme' desenvolvida
stderr_nlme(modelo_intercept_hlm2)

#> Valor da variância de v0j (efeito aleatório) <0.05. Logo existe efeito 
#> aleatorio das escolas. Da mesma forma que observamos no modelo nulo. 


# Comparação entre LogLike ------------------------------------------------
# Comparação entre os LLs dos modelos nulo, 
data.frame(OLS_Nulo = logLik(modelo_ols_nulo),
           HLM2_Nulo = logLik(modelo_nulo_hlm2),
           HLM2_Intercept_Aleat = logLik(modelo_intercept_hlm2)) %>%
  rename(`OLS Nulo` = 1,
         `HLM2 Nulo` = 2,
         `HLM2 com Interceptos Aleatórios` = 3) %>%
  melt() %>%
  ggplot(aes(x = variable, y = (abs(-value)), fill = factor(variable))) +
  geom_bar(stat = "identity") +
  geom_label(aes(label = (round(value,3))), hjust = 1.1, color = "white", size = 7) +
  labs(title = "Comparação do LL", 
       y = "LogLik", 
       x = "Modelo Proposto") +
  coord_flip() +
  scale_fill_manual("Legenda:",
                    values = c("grey25","grey45","bisque4")) +
  theme(legend.title = element_blank(), 
        panel.background = element_rect("white"),
        legend.position = "none",
        axis.line = element_line())

lrtest(modelo_intercept_hlm2, modelo_nulo_hlm2)



# x -----------------------------------------------------------------------
# x -----------------------------------------------------------------------
# x -----------------------------------------------------------------------

# Estimar Modelo: ---------------------------------------------------------
# Interceptos e Inclinações Aleatórias ------------------------------------

# Estimação do modelo com Interceptos e Inclinações Aleatórios
modelo_intercept_inclin_hlm2 <- lme(fixed = desempenho ~ horas,
                                    random = ~ horas | escola, 
                                    data = estudante_escola,
                                    method = "REML")


# random = ~ horas | escola: inclui inclinações aleatorias para horas e
#                            interceptos aleatórios em função de escola



# summary -----------------------------------------------------------------
# Parâmetros do modelo
summary(modelo_intercept_inclin_hlm2)

#> parametros fixos (var. horas) explica a variação no desempenho dos alunos

# Efeito do parâmetro aleatorio -------------------------------------------
# Erros-padrão por meio da função 'stderr_nlme' desenvolvida
stderr_nlme(modelo_intercept_inclin_hlm2)

# Components Estimatives    Std_Err         z p.value
# 1   Var(v0j)  55.0696316 26.9481751  2.043538   0.041
# 2   Var(v1j)   0.9379128  0.4610987  2.034083   0.042
# 3     Var(e)   7.0497481  0.5418259 13.011094   0.000


#> Variância do efeito aleatorio do intercepto Var(v0j)
#>  foi significativo
#>  
#>  Variância do efeito aleatorio da inclinação Var(v1j)
#>  foi significativo. 
#>  Se esse efeito não for significativo (enquanto o V0j é),
#>  indica que temos de fato, efeitos no intercepto (V0j), mas todos tem a 
#>  mesma inclinação (já que o v1j não deu significativo no caso hipotetico)

#> Aqui custuma-se flexibilizar para 0.1 de nível de significancia
#> ao inves de usar 0.05


# Comparação dos modelos: loglik --------------------------------------------------

# Comparação entre os LLs do modelos
data.frame(OLS_Nulo = logLik(modelo_ols_nulo),
           HLM2_Nulo = logLik(modelo_nulo_hlm2),
           HLM2_Intercept_Aleat = logLik(modelo_intercept_hlm2),
           HLM2_Intercept_Inclin_Aleat = logLik(modelo_intercept_inclin_hlm2)) %>%
  rename(`OLS Nulo` = 1,
         `HLM2 Nulo` = 2,
         `HLM2 com Interceptos Aleatórios` = 3,
         `HLM2 com Interceptos e Inclinações Aleatórios` = 4) %>%
  melt() %>%
  ggplot(aes(x = variable, y = (abs(-value)), fill = factor(variable))) +
  geom_bar(stat = "identity") +
  geom_label(aes(label = (round(value,3))), hjust = 1.1, color = "white", size = 6) +
  labs(title = "Comparação do LL", 
       y = "LogLik", 
       x = "Modelo Proposto") +
  coord_flip() +
  scale_fill_manual("Legenda:",
                    values = c("grey25","grey45","bisque4","bisque3")) +
  theme(legend.title = element_blank(), 
        panel.background = element_rect("white"),
        legend.position = "none",
        axis.line = element_line())

lrtest(modelo_intercept_hlm2, modelo_nulo_hlm2,
       modelo_intercept_inclin_hlm2, modelo_ols_nulo)




# Estimar Modelo Final HLM ----------------------------------------------------
# O tempo de experiencia (nova variável resposta) é uma variável de nível 2. 
# ela está relacionada com as escolas (nível 1) e não com cada observação.
# Essa é uma variável de nível contextual. Então aqui, vamos ver se os alunos
# que estudam em diferentes escolas, e que contam com nível de experiencia maior
# desses professores, já tem naturalmente maiores valores iniciais de desempenho
# essa variável.

#. Naõ se atera entre indivíduos (observações), mas sim entre escolas

# Estimação do modelo final
modelo_final_hlm2 <- lme(fixed = desempenho ~ horas + texp + horas:texp,
                         random = ~ horas | escola,
                         data = estudante_escola,
                         method = "REML")

# Summary -----------------------------------------------------------------
# Parâmetros do modelo
summary(modelo_final_hlm2)


# Significancia -----------------------------------------------------------
# Erros-padrão por meio da função 'stderr_nlme' desenvolvida
stderr_nlme(modelo_final_hlm2)


# Comparação Loglike -------------------------------------------------------
# Comparação entre os LLs do modelos
data.frame(OLS_Nulo = logLik(modelo_ols_nulo),
           HLM2_Nulo = logLik(modelo_nulo_hlm2),
           HLM2_Intercept_Aleat = logLik(modelo_intercept_hlm2),
           HLM2_Intercept_Inclin_Aleat = logLik(modelo_intercept_inclin_hlm2),
           HLM2_Modelo_Final = logLik(modelo_final_hlm2)) %>%
  rename(`OLS Nulo` = 1,
         `HLM2 Nulo` = 2,
         `HLM2 com Interceptos Aleatórios` = 3,
         `HLM2 com Interceptos e Inclinações Aleatórios` = 4,
         `HLM2 Modelo Final` = 5) %>%
  melt() %>%
  ggplot(aes(x = variable, y = (abs(-value)), fill = factor(variable))) +
  geom_bar(stat = "identity") +
  geom_label(aes(label = (round(value,3))), hjust = 1.1, color = "white", size = 6) +
  labs(title = "Comparação do LL", 
       y = "LogLik", 
       x = "Modelo Proposto") +
  coord_flip() +
  scale_fill_manual("Legenda:",
                    values = c("grey25","grey45","bisque4","bisque3",
                               "deepskyblue1")) +
  theme(legend.title = element_blank(), 
        panel.background = element_rect("white"),
        legend.position = "none",
        axis.line = element_line())


# Melhorar Visualização ---------------------------------------------------
# Melhor visualização dos interceptos e das inclinações aleatórios por escola,
# para o modelo final HLM2

v_final <- data.frame(modelo_final_hlm2[["coefficients"]][["random"]][["escola"]]) %>%
  rename(v00 = 1,
         v10 = 2)
v_final$escola <- c(1:10)
v_final$escola <- as.factor(v_final$escola)

v_final %>% 
  select(escola, everything()) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 25)

# Para observarmos graficamente o comportamento dos valores de v0j, ou seja,
# dos interceptos aleatórios por escola, podemos comandar
random.effects(modelo_final_hlm2) %>% 
  rename(v0j = 1) %>% 
  rownames_to_column("Escola") %>% 
  mutate(color_v0j = ifelse(v0j < 0, "A", "B"),
         hjust_v0j = ifelse(v0j > 0, 1.15, -0.15)) %>% 
  arrange(Escola) %>% 
  ggplot(aes(label = format(v0j, digits = 2), 
             hjust = hjust_v0j)) +
  geom_bar(aes(x = fct_rev(Escola), y = v0j, fill = color_v0j),
           stat = "identity", color = "black") +
  geom_text(aes(x = Escola, y = 0), size = 4.1, color = "black") +
  coord_flip() +
  labs(x = "Escola",
       y = expression(nu[0][j])) +
  scale_fill_manual("oi", values = c("firebrick1","green1")) +
  theme(panel.background = element_rect("white"),
        panel.border = element_rect(NA),
        panel.grid = element_line("grey95"),
        legend.position = "none")


# Para observarmos graficamente o comportamento dos valores de v1j, ou seja
# das inclinações aleatórias por escola, podemos comandar
random.effects(modelo_final_hlm2) %>% 
  rename(v1j = 2) %>% 
  rownames_to_column("Escola") %>% 
  mutate(color_v1j = ifelse(v1j < 0, "A", "B"),
         hjust_v1j = ifelse(v1j > 0, 1.15, -0.15)) %>% 
  arrange(Escola) %>% 
  ggplot(aes(label = format(v1j, digits = 2), 
             hjust = hjust_v1j)) +
  geom_bar(aes(x = fct_rev(Escola), y = v1j, fill = color_v1j),
           stat = "identity", color = "black") +
  geom_text(aes(x = Escola, y = 0), size = 4.1, color = "black") +
  coord_flip() +
  labs(x = "Escola",
       y = expression(nu[1][j])) +
  scale_fill_manual("oi", values = c("firebrick1","green1")) +
  theme(panel.background = element_rect("white"),
        panel.border = element_rect(NA),
        panel.grid = element_line("grey95"),
        legend.position = "none")


# Extrair os valores de efeito aleatorio ----------------------------------
random.effects(modelo_final_hlm2)

# Predict -----------------------------------------------------------------
# Gerando os fitted values do modelo HLM2 Final
estudante_escola$hlm2_fitted <- predict(modelo_final_hlm2,
                                        estudante_escola)

# Visualizando os fitted values do modelo
# Visualizando os fitted values por estudante e por escola
predict(modelo_final_hlm2, level = 0:1) %>% 
  mutate(escola = gsub("^.*?\\/","",escola),
         escola = as.factor(as.numeric(escola)),
         desempenho = estudante_escola$desempenho,
         etjk = resid(modelo_final_hlm2)) %>% #função resid gera os termos etjk
  select(escola, desempenho, everything()) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 18)

#> etjk é o erro (observado - esperado)


# Efetuando predições
# Exemplo: Quais os valores previstos de desempenho escolar, para dado
# aluno que estuda na escola "1", sabendo-se que ele estuda 11h semanais,
# e que a escola oferece tempo médio de experiência de seus professores
# igual a 3.6 anos?
predict(modelo_final_hlm2, level = 0:1,
        newdata = data.frame(escola = "1",
                             horas = 11,
                             texp = 3.6))


# Valores previstos do desempenho escolar em função da variável horas para o 
# modelo final HLM2 com interceptos e inclinações aleatórios
estudante_escola %>%
  mutate(fitted_escola = predict(modelo_final_hlm2, level = 1)) %>% 
  ggplot() +
  geom_point(aes(x = horas, y = fitted_escola)) +
  geom_smooth(aes(x = horas, y = fitted_escola, color = factor(escola)), 
              method = "lm", se = F) +
  scale_colour_viridis_d() +
  labs(x = "Quantidade Semanal de Horas de Estudo do Aluno",
       y = "Desempenho Escolar (Fitted Values)") +
  theme_bw()


# Predict Manual ----------------------------------------------------------
# Para escola= 1 | 11 horas semanais de estudo e 3.6 tempo de experiencia
# Componente Fixo da predição:
-0.8495955 + 0.7134608*11 + 1.5852559*3.6 + 0.2318290*11*3.6

# Componente fixo + Aleatório (mesmo que usar o predict())
-0.8495955 + 0.7134608*11 + 1.5852559*3.6 + 0.2318290*11*3.6 - 0.21 + 0.4388*11

# valores usados abaixo
summary(modelo_final_hlm2)
# Fixed effects:  desempenho ~ horas + texp + horas:texp 
#                 Value   Std.Error  DF   t-value p-value
# (Intercept) -0.8495955 2.9948411 346 -0.283686  0.7768
# horas        0.7134608 0.3206702 346  2.224905  0.0267
# texp         1.5852559 0.4863036   8  3.259807  0.0115
# horas:texp   0.2318290 0.0526263 346  4.405190  0.0000

# Extrair os valores de efeito aleatorio ----------------------------------
random.effects(modelo_final_hlm2)

# escola(Intercept)     horas
# 1   -0.2113827  0.438811425
# 2    1.2915001 -0.278181129
# 3    6.0907720 -0.779129691
# 4   -5.2454692  0.450366532
# 5   -1.6953372 -0.139679396
# 6    3.8642473  0.005098088
# 7   -8.2708877  0.909170865
# 8   -1.3634799  0.217166353
# 9    3.7056861 -0.607100983
# 10   1.8343512 -0.216522062




# x -----------------------------------------------------------------------
# Comparação com Modelos OLS ----------------------------------------------
# Elaborando um modelo OLS para fins de comparação
modelo_ols <- lm(formula = desempenho ~ horas + texp,
                 data = estudante_escola)

modelo_ols2 <- lm(formula = desempenho ~ horas + texp +horas:texp,
                  data = estudante_escola)

# Parâmetros
summary(modelo_ols)

# Comparando os LL dos modelos elaborados
data.frame(OLS = logLik(modelo_ols),
           HLM2_Modelo_Final = logLik(modelo_final_hlm2),
           OLS_INTERACAO = logLik(modelo_ols2)) %>%
  rename(`OLS` = 1,
         `HLM2 Modelo Final` = 2,
         `OLS2` = 3) %>%
  melt() %>%
  ggplot(aes(x = variable, y = (abs(-value)), fill = factor(variable))) +
  geom_bar(stat = "identity") +
  geom_label(aes(label = (round(value,3))), hjust = 1.1, color = "white", size = 7) +
  labs(title = "Comparação do LL", 
       y = "LogLik", 
       x = "Modelo Proposto") +
  coord_flip() +
  scale_fill_manual("Legenda:",
                    values = c("darkorchid","deepskyblue1", "darkgray")) +
  theme(legend.title = element_blank(), 
        panel.background = element_rect("white"),
        legend.position = "none",
        axis.line = element_line())



# Comparação LR teste -----------------------------------------------------
# LR Test
lrtest(modelo_ols, modelo_final_hlm2, modelo_ols2)


# Fitted Values -----------------------------------------------------------
# Comparando a aderência dos fitted values dos modelos estimados
# Gerando os fitted values do modelo OLS
estudante_escola$ols_fitted <- modelo_ols$fitted.values

#Plotagem
estudante_escola %>%
  ggplot() +
  geom_smooth(aes(x = desempenho, y = ols_fitted, color = "OLS"),
              method = "lm", se = F, formula = y ~ splines::bs(x, df = 5),
              size = 1.5) +
  geom_smooth(aes(x = desempenho, y= hlm2_fitted, color = "HLM2 Final"),
              method = "lm", se = F, formula = y ~ splines::bs(x, df = 5),
              size = 1.5) +
  geom_smooth(aes(x = desempenho, y = desempenho), method = "lm", 
              color = "gray44", size = 1.05,
              linetype = "longdash") +
  geom_point(aes(x = desempenho, y = ols_fitted,
                 color = "OLS")) +
  geom_point(aes(x = desempenho, y = hlm2_fitted,
                 color = "HLM2 Final"))  +
  scale_color_manual("Modelos:", 
                     values = c("deepskyblue1","darkorchid")) +
  labs(x = "Desempenho", y = "Fitted Values") +
  theme_bw()


# x -----------------------------------------------------------------------
# Comparando com Modelos Com Dummies --------------------------------------
# Procedimento n-1 dummies para o contexto
estudante_escola_dummies <- dummy_cols(.data = estudante_escola,
                                       select_columns = "escola",
                                       remove_first_dummy = TRUE,
                                       remove_selected_columns = TRUE)

# Visualizando as dummies na nova base de dados 'estudante_escola_dummies'
estudante_escola_dummies %>%
  select(-hlm2_fitted,-ols_fitted, everything()) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = F,
                font_size = 19)

# Modelo OLS com dummies
modelo_ols_dummies <- lm(formula = desempenho ~ horas + texp + escola_2 +
                           escola_3 + escola_4 + escola_5 + escola_6 +
                           escola_7 + escola_8 + escola_9 + escola_10,
                         data = estudante_escola_dummies)

# Parâmetros
summary(modelo_ols_dummies)

# Procedimento stepwise
modelo_ols_dummies_step <- step(object = modelo_ols_dummies,
                                step = qchisq(p = 0.05, df = 1,
                                              lower.tail = FALSE))

# Parâmetros do modelo OLS estimado com dummies por escola a partir do
# procedimento Stepwise
summary(modelo_ols_dummies_step)

# Comparando os LL dos modelos HLM2 Final, OLs e OLS com Dummies e Stepwise
data.frame(OLS = logLik(modelo_ols),
           OLS_Dummies_Step = logLik(modelo_ols_dummies_step),
           HLM2_Modelo_Final = logLik(modelo_final_hlm2)) %>%
  rename(`OLS` = 1,
         `OLS com Dummies e Stepwise` = 2,
         `HLM2 Modelo Final` = 3) %>%
  melt() %>%
  ggplot(aes(x = variable, y = (abs(-value)), fill = factor(variable))) +
  geom_bar(stat = "identity") +
  geom_label(aes(label = (round(value,3))), hjust = 1.1, color = "white", size = 7) +
  labs(title = "Comparação do LL", 
       y = "LogLik", 
       x = "Modelo Proposto") +
  coord_flip() +
  scale_fill_manual("Legenda:",
                    values = c("darkorchid","maroon1","deepskyblue1")) +
  theme(legend.title = element_blank(), 
        panel.background = element_rect("white"),
        legend.position = "none",
        axis.line = element_line())

# LR Test
lrtest(modelo_ols_dummies_step, modelo_final_hlm2)

# Comparação entre os parãmetros dos modelos (atente-se para a quantidade de
# parâmetros estimados em cada um deles!)
export_summs(modelo_ols_dummies_step, modelo_final_hlm2,
             model.names = c("OLS com Dummies", "HLM2 Final"))


# Comparando a aderência dos fitted values dos modelos HLM2 Final, OLS e
# OLS com Dummies e Stepwise
# Gerando os fitted values do modelo OLS com Dummies e Stepwise
estudante_escola$ols_step_fitted <- modelo_ols_dummies_step$fitted.values

# Gráfico para a comparação entre os fitted values dos modelos HLM2 Final, OLs e
# OLS com Dummies e Procedimento Stepwise
estudante_escola %>%
  ggplot() +
  geom_smooth(aes(x = desempenho, y = ols_step_fitted, color = "OLS com Dummies"),
              method = "lm", se = F, formula = y ~ splines::bs(x, df = 5),
              size = 1.5) +
  geom_smooth(aes(x = desempenho, y= hlm2_fitted, color = "HLM2 Final"),
              method = "lm", se = F, formula = y ~ splines::bs(x, df = 5),
              size = 1.5) +
  geom_smooth(aes(x = desempenho, y= ols_fitted, color = "OLS"),
              method = "lm", se = F, formula = y ~ splines::bs(x, df = 5),
              size = 1.5) +
  geom_smooth(aes(x = desempenho, y = desempenho), method = "lm", 
              color = "gray44", size = 1.05,
              linetype = "longdash") +
  scale_color_manual("Modelos:", 
                     values = c("deepskyblue1", "maroon1", "darkorchid")) +
  labs(x = "Desempenho", y = "Fitted Values") +
  theme_bw()


# Comparação entre os LLs de todos os modelos estimados neste exemplo
data.frame(OLS_Nulo = logLik(modelo_ols_nulo),
           HLM2_Nulo = logLik(modelo_nulo_hlm2),
           OLS = logLik(modelo_ols),
           HLM2_Intercept_Aleat = logLik(modelo_intercept_hlm2),
           OLS_Dummies_step = logLik(modelo_ols_dummies_step),
           HLM2_Intercept_Inclin_Aleat = logLik(modelo_intercept_inclin_hlm2),
           HLM2_Modelo_Final = logLik(modelo_final_hlm2)) %>%
  rename(`OLS Nulo` = 1,
         `HLM2 Nulo` = 2,
         `OLS` = 3,
         `HLM2 com Interceptos Aleatórios` = 4,
         `OLS com Dummies e Stepwise` = 5,
         `HLM2 com Interceptos e Inclinações Aleatórios` = 6,
         `HLM2 Modelo Final` = 7) %>%
  melt() %>%
  ggplot(aes(x = variable, y = (abs(-value)), fill = factor(variable))) +
  geom_bar(stat = "identity") +
  geom_label(aes(label = (round(value,3))), hjust = 1.1, color = "white", size = 5) +
  labs(title = "Comparação do LL", 
       y = "LogLik", 
       x = "Modelo Proposto") +
  coord_flip() +
  scale_fill_manual("Legenda:",
                    values = c("grey25","grey45","darkorchid","bisque4",
                               "maroon1","bisque3","deepskyblue1")) +
  theme(legend.title = element_blank(), 
        panel.background = element_rect("white"),
        legend.position = "none",
        axis.line = element_line())




# ♥ ☺ ♥ ☺ ♥ ☺ ♥ ☺ ♥ ☺ ♥ ☺ ♥ ☺ ♥ ☺ ♥ ☺ ♥ ☺ ♥ ☺ ♥ ☺ ♥ ☺ ♥ ☺ ♥ ☺ ♥ ☺ ♥ ☺ ♥ ☺
# x -----------------------------------------------------------------------
# x -----------------------------------------------------------------------
# x -----------------------------------------------------------------------

# Modelos HLM3 - 3 níveis com medidas repetidas --------------------------------
# Aqui poderiamos trabalhar com três níveis de camadas de hierarquia com os dados
# Mas neste caso, vamos colocar a camada temporal. Monitorar os alunos ao longo
# do tempo

# Quando temos o tempo, monitorando as observações ao longo do tempo, temos o que
# se caracteriza como medidas repetidas. Neste caso, teremos vários períodos 
# aninhados em uma observação. E não precisa ser balancedo. Não precisa da mesma 
# quantidade. Quando fazemos esses monitoramentos, temos os períodos aninhados
# dentro das observações, o período será sempre o nível 1. As observações são 
# grupos a que pertence esses períodos, sendo portanto nível 2. E as observações
# podem estar aninhadas em outro grupo.

# No exemplo anterior. Tinhamos os estudantes e cada um pertencia a uma escola
# então as observações (estudantes) estavam aninhadas dentro de escolas.
# Agora, o período está aninhado dentro de cada observação (estudante), e cada
# estudante está aninhado dentro da escola (nível 3).




# Pacotes -----------------------------------------------------------------
pacotes <- c("plotly","tidyverse","reshape2","knitr","kableExtra",
             "nlme","lmtest","fastDummies","msm","lmeInfo","jtools","gganimate",
             "ggridges","viridis","hrbrthemes")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}


# Função para determinar Significância ------------------------------------
# Algoritmo para determinação dos erros-padrão das variâncias no componente de
# efeitos aleatórios

# ATENÇÃO: A função abaixo é plenamente funcional para modelos do tipo HLM2
# e HLM3, desde que estimados pelo pacote nlme

stderr_nlme <- function(model){
  if(base::class(model) != "lme"){
    base::message("Use a lme object model from nlme package")
    stop()}
  resume <- base::summary(model)
  if(base::length(base::names(model$groups))==1){
    m.type <- "HLM2"
  } else if(base::length(base::names(model$groups))==2){
    m.type <- "HLM3"
  }
  if(m.type == "HLM2"){
    vcov_matrix <- model$apVar
    logs_sd_re <- base::attr(vcov_matrix,"Pars")
    if(base::length(logs_sd_re)==2){
      stderr_tau00 <- msm::deltamethod(~exp(x1)^2,logs_sd_re,vcov_matrix)
      stderr_sigma <- msm::deltamethod(~exp(x2)^2,logs_sd_re,vcov_matrix)
      results <- base::data.frame(`RE Components`=base::c("Var(v0j)","Var(e)"),
                                  `Variance Estimatives`= base::c(base::exp(logs_sd_re)[[1]]^2,
                                                                  base::exp(logs_sd_re[[2]])^2),
                                  `Std Err.`=base::c(stderr_tau00,
                                                     stderr_sigma),
                                  z=base::c(base::exp(logs_sd_re)[[1]]^2/stderr_tau00,
                                            base::exp(logs_sd_re[[2]])^2/stderr_sigma),
                                  `p-value`=base::round(stats::pnorm(q=base::c(base::exp(logs_sd_re)[[1]]^2/stderr_tau00,
                                                                               base::exp(logs_sd_re[[2]])^2/stderr_sigma),
                                                                     lower.tail=F)*2,3))
      return(results)
    }
    else{
      stderr_tau00 <- msm::deltamethod(~exp(x1)^2,logs_sd_re,vcov_matrix)
      stderr_tau01 <- msm::deltamethod(~exp(x2)^2,logs_sd_re,vcov_matrix)
      stderr_sigma <- msm::deltamethod(~exp(x4)^2,logs_sd_re,vcov_matrix)
      results <- base::data.frame(Components=base::c("Var(v0j)","Var(v1j)","Var(e)"),
                                  Estimatives= base::c(base::exp(logs_sd_re)[[1]]^2,
                                                       base::exp(logs_sd_re[[2]])^2,
                                                       base::exp(logs_sd_re[[4]])^2),
                                  Std_Err=base::c(stderr_tau00,
                                                  stderr_tau01,
                                                  stderr_sigma),
                                  z=base::c(base::exp(logs_sd_re)[[1]]^2/stderr_tau00,
                                            base::exp(logs_sd_re[[2]])^2/stderr_tau01,
                                            base::exp(logs_sd_re[[4]])^2/stderr_sigma),
                                  `p-value`=base::round(stats::pnorm(q=base::c(base::exp(logs_sd_re)[[1]]^2/stderr_tau00,
                                                                               base::exp(logs_sd_re[[2]])^2/stderr_tau01,
                                                                               base::exp(logs_sd_re[[4]])^2/stderr_sigma),
                                                                     lower.tail=F)*2,3))
      return(results)
    }
  }
  if(m.type == "HLM3"){
    vcov_matrix <- model$apVar
    logs_sd_re <-  base::attr(vcov_matrix,"Pars")
    if(base::length(logs_sd_re) == 3){
      stderr_tau_r000 <- msm::deltamethod(~exp(x1)^2,logs_sd_re,vcov_matrix)
      stderr_tau_u000 <- msm::deltamethod(~exp(x2)^2,logs_sd_re,vcov_matrix)
      stderr_sigma <- msm::deltamethod(~exp(x3)^2,logs_sd_re,vcov_matrix)
      results <- base::data.frame(Components=base::c("Var(t00k)","Var(v0jk)","Var(e)"),
                                  Estimatives=base::c(base::exp(logs_sd_re)[[2]]^2,
                                                      base::exp(logs_sd_re)[[1]]^2,
                                                      base::exp(logs_sd_re)[[3]]^2),
                                  Std_Err=base::c(stderr_tau_u000,
                                                  stderr_tau_r000,
                                                  stderr_sigma),
                                  z=base::c(base::exp(logs_sd_re)[[2]]^2/stderr_tau_u000,
                                            base::exp(logs_sd_re)[[1]]^2/stderr_tau_r000,
                                            base::exp(logs_sd_re)[[3]]^2/stderr_sigma),
                                  `p-value`=base::round(stats::pnorm(q=base::c(base::exp(logs_sd_re)[[2]]^2/stderr_tau_u000,
                                                                               base::exp(logs_sd_re)[[1]]^2/stderr_tau_r000,
                                                                               base::exp(logs_sd_re)[[3]]^2/stderr_sigma),
                                                                     lower.tail=F)*2,3))
      return(results)
    } 
    else{
      stderr_tau_r000 <- msm::deltamethod(~exp(x1)^2,logs_sd_re,vcov_matrix)
      stderr_tau_r100 <- msm::deltamethod(~exp(x2)^2,logs_sd_re,vcov_matrix)
      stderr_tau_u000 <- msm::deltamethod(~exp(x4)^2,logs_sd_re,vcov_matrix)
      stderr_tau_u100 <- msm::deltamethod(~exp(x5)^2,logs_sd_re,vcov_matrix)
      stderr_sigma <- msm::deltamethod(~exp(x7)^2,logs_sd_re,vcov_matrix)
      results <- base::data.frame(`RE_Components`=base::c("Var(t00k)","Var(t10k)",
                                                          "Var(v0jk)","Var(v1jk)",
                                                          "Var(e)"),
                                  `Variance Estimatives`=base::c(base::exp(logs_sd_re)[[4]]^2,
                                                                 base::exp(logs_sd_re)[[5]]^2,
                                                                 base::exp(logs_sd_re)[[1]]^2,
                                                                 base::exp(logs_sd_re)[[2]]^2,
                                                                 base::exp(logs_sd_re)[[7]]^2),
                                  `Std Err.`=base::c(stderr_tau_u000,
                                                     stderr_tau_u100,
                                                     stderr_tau_r000,
                                                     stderr_tau_r100,
                                                     stderr_sigma),
                                  z=base::c(base::exp(logs_sd_re)[[4]]^2/stderr_tau_u000,
                                            base::exp(logs_sd_re)[[5]]^2/stderr_tau_u100,
                                            base::exp(logs_sd_re)[[1]]^2/stderr_tau_r000,
                                            base::exp(logs_sd_re)[[2]]^2/stderr_tau_r100,
                                            base::exp(logs_sd_re)[[7]]^2/stderr_sigma),
                                  `p-value`=base::round(stats::pnorm(q=base::c(base::exp(logs_sd_re)[[4]]^2/stderr_tau_u000,
                                                                               base::exp(logs_sd_re)[[5]]^2/stderr_tau_u100,
                                                                               base::exp(logs_sd_re)[[1]]^2/stderr_tau_r000,
                                                                               base::exp(logs_sd_re)[[2]]^2/stderr_tau_r100,
                                                                               base::exp(logs_sd_re)[[7]]^2/stderr_sigma),
                                                                     lower.tail=F)*2,3))
      return(results)
    }
  }
}


# Analise Exploratória ----------------------------------------------------

# Carregando a base de dados
load(file = "raw_datas_exemplos/tempo_estudante_escola.RData")

# Visualização da base de dados
tempo_estudante_escola %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 22)

# Estatísticas descritivas e tabelas de frequências
summary(tempo_estudante_escola)

# Estudo sobre o balanceamento dos dados em relação à quantidade de alunos 
# por período analisado
tempo_estudante_escola %>%
  rename(Mês = 3,
         `Quantidade de Alunos` = 2) %>% 
  group_by(Mês) %>% 
  summarise(`Quantidade de Alunos` = n()) %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 25)

# Estudo sobre o desbalanceamento da quantidade de alunos aninhados em 
# escolas
tempo_estudante_escola %>% 
  rename(Escola = 1,
         `Quantidade de Alunos` = 2) %>% 
  group_by(Escola) %>% 
  summarise(`Quantidade de Alunos` = n()/4) %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 25)

#Gráfico da evolução temporal média do desempenho escolar dos estudantes
#(ajuste linear)
ggplotly(
  tempo_estudante_escola %>%
    ggplot(aes(x = mes, y = desempenho, group = 1, label = estudante)) +
    geom_point(color = "gold", size = 2, alpha = 0.2) +
    geom_smooth(color = "#440154FF", method = "lm", formula = "y ~ x",
                se = F, size = 2) +
    labs(x = "Mês",
         y = "Desempenho Escolar") +
    theme_bw()
)

#Kernel density estimation (KDE) - função densidade de probabilidade da
#variável dependente (desempenho), com histograma
ggplotly(
  ggplot(tempo_estudante_escola, aes(x = desempenho)) +
    geom_density(aes(x = desempenho), 
                 position = "identity", color = "black", size = 1) +
    geom_histogram(aes(y = ..density..), color = "white", fill = "deepskyblue",
                   bins = 30) +
    theme_classic()
)

#Gráfico com distribuições da variável 'desempenho' para as escolas
#função 'geom_density_ridges_gradient' do pacote 'ggridges'
ggplot(tempo_estudante_escola, aes(x = desempenho, y = escola, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis(name = "Desempenho", option = "turbo", direction = -1) +
  labs(
    title = "Distribuições da variável 'desempenho' para as escolas",
    x = "Desempenho",
    y = "Escola") +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 5)
  )

#Gráfico com distribuições da variável 'desempenho' para os meses
#função 'geom_density_ridges_gradient' do pacote 'ggridges'
ggplot(tempo_estudante_escola, aes(x = desempenho, y = as.factor(mes), fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis(name = "Desempenho", option = "cividis") +
  labs(
    title = "Distribuições da variável 'desempenho' para os meses",
    x = "Desempenho",
    y = "Mês") +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 5)
  )

#Kernel density estimation (KDE) - função densidade de probabilidade da
#variável dependente (desempenho) por escola
ggplotly(
  ggplot(tempo_estudante_escola, aes(x = desempenho)) +
    geom_density(aes(color = escola, fill = escola), 
                 position = "identity", alpha = 0.2) +
    scale_color_viridis_d() +
    scale_fill_viridis_d() +
    theme_classic()
)

#Kernel density estimation (KDE) - função densidade de probabilidade da
#variável dependente (desempenho), com histograma e por escola separadamente
#(função facet_wrap)
tempo_estudante_escola %>% 
  group_by(escola) %>% 
  mutate(linhas = 1:n()) %>% 
  mutate(x = unlist(density(desempenho, n = max(linhas))["x"]),
         y = unlist(density(desempenho, n = max(linhas))["y"])) %>%
  ggplot() +
  geom_area(aes(x = x, y = y, group = escola, fill = escola), color = "black", alpha = 0.3) +
  geom_histogram(aes(x = desempenho, y = ..density.., fill = escola), 
                 color = "black", position = 'identity', alpha = 0.1) +
  facet_wrap(~ escola) +
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  theme_bw()

#Exploração visual da evolução temporal do desempenho dos 50 primeiros
#estudantes da base de dados (50 estudantes em razão da visualização no gráfico)
tempo_estudante_escola %>%
  filter(estudante %in% 1:50) %>% 
  ggplot(aes(group = estudante)) +
  geom_point(aes(x = mes, y = desempenho, color = estudante), size = 3) +
  geom_line(aes(x = mes, y = desempenho, color = estudante), size = 1) +
  guides(color = "none") +
  scale_colour_viridis_d() +
  labs(x = "Mês",
       y = "Desempenho Escolar") +
  theme_bw()

#Gráfico de desempenho x mês (OLS) por escola separadamente
#(funções transition_states e animate do pacote gganimate)
ggplot(tempo_estudante_escola, aes(x=mes, y=desempenho, color=escola)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = F) +
  transition_states(escola, transition_length = 1, state_length = 2) + 
  enter_fade() + 
  exit_shrink() +
  ease_aes('linear') + 
  labs(x = "Mês",
       y = "Desempenho Escolar") +
  scale_color_viridis_d() +
  ggtitle("Desempenho escolar por escola", subtitle = "Escola: {closest_state}") +
  theme_bw() -> p

animate(p, nframes = 150, fps = 6)

#Gráfico da evolução temporal do desempenho médio por escola (ajustes lineares)
ggplotly(
  tempo_estudante_escola %>%
    ggplot(aes(x = mes, y = desempenho, color = escola)) +
    geom_smooth(method = "lm", formula = y ~ x, se = F) +
    geom_point(size = 3, alpha = 0.2) +
    guides(color = "none") +
    scale_colour_viridis_d() +
    labs(x = "Mês",
         y = "Desempenho Escolar") +
    theme_bw()
)



# Estimação do Modelo Nulo HLM3 ------------------------------------------------
# Estimação do modelo nulo (função lme do pacote nlme)
modelo_nulo_hlm3 <- lme(fixed = desempenho ~ 1,
                        random = list(escola = ~1, estudante = ~1),
                        data = tempo_estudante_escola,
                        method = "REML")

# Parâmetros do modelo
summary(modelo_nulo_hlm3)

# Erros-padrão do modelo por meio da função 'stderr_nlme' desenvolvida
stderr_nlme(modelo_nulo_hlm3)


# Comparaçao do HLM3 Nulo com um OLS nulo ---------------------------------
# Para estimarmos o modelo OLS nulo, podemos comandar o seguinte
modelo_ols_nulo <- lm(formula = desempenho ~ 1,
                      data = tempo_estudante_escola)

# Parâmetros do modelo OLS nulo
summary(modelo_ols_nulo)

# Para comparar os LLs dos modelos, vamos utilizar a função lrtest do pacote lmtest
lrtest(modelo_ols_nulo, modelo_nulo_hlm3)

# Comparação entre os LLs dos modelos
data.frame(OLS_Nulo = logLik(modelo_ols_nulo),
           HLM3_Nulo = logLik(modelo_nulo_hlm3)) %>%
  rename(`OLS Nulo` = 1,
         `HLM3 Nulo` = 2) %>%
  melt() %>%
  ggplot(aes(x = variable, y = (abs(-value)), fill = factor(variable))) +
  geom_bar(stat = "identity") +
  geom_label(aes(label = (round(value,3))), hjust = 1.1, color = "white", size = 7) +
  labs(title = "Comparação do LL", 
       y = "LogLik", 
       x = "Modelo Proposto") +
  coord_flip() +
  scale_fill_manual("Legenda:",
                    values = c("grey25","grey45")) +
  theme(legend.title = element_blank(), 
        panel.background = element_rect("white"),
        legend.position = "none",
        axis.line = element_line())



# Estimação do Modelo HLM3:interceptos e inclinações aleatórias --------

# Estimação do modelo com Tendência Linear e Interceptos e Inclinações Aleatórios
modelo_intercept_inclin_hlm3 <- lme(fixed = desempenho ~ mes,
                                    random = list(escola = ~mes, estudante = ~mes),
                                    data = tempo_estudante_escola,
                                    method = "REML")

# Parâmetros do modelo
summary(modelo_intercept_inclin_hlm3)

# Erros-padrão do modelo por meio da função 'stderr_nlme' desenvolvida
stderr_nlme(modelo_intercept_inclin_hlm3)

#Função lrtest para comparar os LLs dos modelos
lrtest(modelo_nulo_hlm3, modelo_intercept_inclin_hlm3)

#Comparação entre os LLs dos modelos
data.frame(OLS_Nulo = logLik(modelo_ols_nulo),
           HLM3_Nulo = logLik(modelo_nulo_hlm3),
           HLM3_Inclinacoes_Aleatorios = logLik(modelo_intercept_inclin_hlm3)) %>%
  rename(`OLS Nulo` = 1,
         `HLM3 Nulo` = 2,
         `HLM3 com Int. e Incl. Aleat.` = 3) %>% 
  melt() %>%
  ggplot(aes(x = variable, y = (abs(-value)), fill = factor(variable))) +
  geom_bar(stat = "identity") +
  geom_label(aes(label = (round(value,3))), hjust = 1.1, color = "white", size = 6) +
  labs(title = "Comparação do Ganho de LL", 
       y = "LogLik", 
       x = "Modelo Proposto") +
  coord_flip() +
  scale_fill_manual("Legenda:",
                    values = c("grey25","grey45","coral4")) +
  theme(legend.title = element_blank(), 
        panel.background = element_rect("white"),
        legend.position = "none",
        axis.line = element_line())


# Estimação HLM3: Intercepto e Inclinação aleatorio -----------------------
# e as variáveis 'ativ' de nível 2 e 'text' de nível 3


# Estimação do modelo com Tendência Linear, Interceptos e Inclinações Aleatórios
# e as variáveis 'ativ' de Nível 2 e 'texp' de Nível 3
modelo_completo_hlm3 <- lme(fixed = desempenho ~ mes + ativ + texp +
                              ativ:mes + texp:mes,
                            random = list(escola = ~mes, estudante = ~mes),
                            data = tempo_estudante_escola,
                            method = "REML")

#Parâmetros do modelo
summary(modelo_completo_hlm3)

#Erros-padrão do modelo por meio da função 'stderr_nlme' desenvolvida
stderr_nlme(modelo_completo_hlm3)

#Função lrtest para comparar os LLs dos modelos
lrtest(modelo_intercept_inclin_hlm3, modelo_completo_hlm3)

#Visualização do ganho de LogLik entre as estimações:
data.frame(OLS_Nulo = logLik(modelo_ols_nulo),
           HLM3_Nulo = logLik(modelo_nulo_hlm3),
           HLM3_Inclinacoes_Aleatorios = logLik(modelo_intercept_inclin_hlm3),
           HLM3_Completo = logLik(modelo_completo_hlm3)) %>%
  rename(`OLS Nulo` = 1,
         `HLM3 Nulo` = 2,
         `HLM3 com Int. e Incl. Aleat.` = 3,
         `HLM3 Completo` = 4) %>% 
  melt() %>%
  ggplot(aes(x = variable, y = (abs(-value)), fill = factor(variable))) +
  geom_bar(stat = "identity") +
  geom_label(aes(label = (round(value,3))), hjust = 1.1, color = "white", size = 6) +
  labs(title = "Comparação do Ganho de LL", 
       y = "LogLik", 
       x = "Modelo Proposto") +
  coord_flip() +
  scale_fill_manual("Legenda:",
                    values = c("grey25","grey45","coral4","#440154FF")) +
  theme(legend.title = element_blank(), 
        panel.background = element_rect("white"),
        legend.position = "none",
        axis.line = element_line())


#Para acessarmos os valores de v0jk e v1jk (efeitos aleatórios de intercepto e 
#de inclinação no nível estudante, respectivamente) e de t00k e t10k (efeitos
#aleatórios de intercepto e de inclinação no nível escola, respectivamente),
#podemos proceder com os seguintes códigos:

#Nível estudante:
random.effects(modelo_completo_hlm3)[["estudante"]] %>% 
  rename(v0jk = 1,
         v1jk = 2) %>% 
  rownames_to_column("Estudante") %>% 
  mutate(Estudante = gsub("^.*?\\/","",Estudante)) %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 25)

#Nível escola:
random.effects(modelo_completo_hlm3)[["escola"]] %>% 
  rename(t00k = 1,
         t10k = 2) %>% 
  rownames_to_column("Escola") %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 25)

#Para observarmos graficamente o comportamento dos valores de v0jk, ou seja,
#dos interceptos aleatórios por estudante, podemos comandar:
ggplotly(
  random.effects(modelo_completo_hlm3)[["estudante"]] %>% 
    rename(v0jk = 1,
           v1jk = 2) %>% 
    rownames_to_column("Estudante") %>% 
    mutate(Estudante = gsub("^.*?\\/","",Estudante)) %>% 
    group_by(Estudante) %>% 
    summarise(v0jk = mean(v0jk)) %>% 
    ggplot(aes(x = fct_rev(Estudante), y = v0jk, label = Estudante)) +
    geom_bar(stat = "identity", color = "gold") +
    coord_flip() +
    labs(x = "Estudante",
         y = "v0jk") +
    theme(legend.title = element_blank(), 
          panel.background = element_rect("white"),
          legend.position = "none",
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank())
)


#Para observarmos graficamente o comportamento dos valores de v1jk, ou seja,
#das inclinações aleatórias por estudante, podemos comandar:
ggplotly(
  random.effects(modelo_completo_hlm3)[["estudante"]] %>% 
    rename(v0jk = 1,
           v1jk = 2) %>% 
    rownames_to_column("Estudante") %>% 
    mutate(Estudante = gsub("^.*?\\/","",Estudante)) %>% 
    group_by(Estudante) %>% 
    summarise(v1jk = mean(v1jk)) %>% 
    ggplot(aes(x = fct_rev(Estudante), y = v1jk, label = Estudante)) +
    geom_bar(stat = "identity", color = "darkorchid4") + 
    coord_flip() +
    labs(x = "Estudante",
         y = "v1jk") +
    theme(legend.title = element_blank(), 
          panel.background = element_rect("white"),
          legend.position = "none",
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank())
)

#Para observarmos graficamente o comportamento dos valores de t00k, ou seja,
#dos interceptos aleatórios por escola, podemos comandar:
random.effects(modelo_completo_hlm3)[["escola"]] %>% 
  rename(t00k = 1,
         t10k = 2) %>% 
  rownames_to_column("Escola") %>% 
  mutate(color_t00k = ifelse(t00k < 0, "A", "B"),
         color_t10k = ifelse(t10k < 0, "A", "B"),
         hjust_t00k = ifelse(t00k > 0, 1.15, -0.15),
         hjust_t10k = ifelse(t10k > 0, 1.15, -0.15)) %>% 
  arrange(Escola) %>% 
  ggplot(aes(label = round(t00k, digits = 3), 
             hjust = hjust_t00k)) +
  geom_bar(aes(x = fct_rev(Escola), y = t00k, fill = color_t00k),
           stat = "identity", color = "black") +
  geom_text(aes(x = Escola, y = 0), size = 4.1, color = "black") +
  coord_flip() +
  labs(x = "Escola",
       y = expression(t[0][0][k])) +
  scale_fill_manual(values = c("firebrick1","green1")) +
  theme(panel.background = element_rect("white"),
        panel.border = element_rect(NA),
        panel.grid = element_line("grey95"),
        legend.position = "none")

#Para observarmos graficamente o comportamento dos valores de t10k, ou seja,
#das inclinações aleatórias por escola, podemos comandar:
random.effects(modelo_completo_hlm3)[["escola"]] %>% 
  rename(t00k = 1,
         t10k = 2) %>% 
  rownames_to_column("Escola") %>% 
  mutate(color_t00k = ifelse(t00k < 0, "A", "B"),
         color_t10k = ifelse(t10k < 0, "A", "B"),
         hjust_t00k = ifelse(t00k > 0, 1.15, -0.15),
         hjust_t10k = ifelse(t10k > 0, 1.15, -0.15)) %>% 
  arrange(Escola) %>% 
  ggplot(aes(label = round(t10k, digits = 3), 
             hjust = hjust_t10k)) +
  geom_bar(aes(x = fct_rev(Escola), y = t10k, fill = color_t10k),
           stat = "identity", color = "black") +
  geom_text(aes(x = Escola, y = 0), size = 4.1, color = "black") +
  coord_flip() +
  labs(x = "Escola",
       y = expression(t[1][0][k])) +
  scale_fill_manual(values = c("firebrick1","green1")) +
  theme(panel.background = element_rect("white"),
        panel.border = element_rect(NA),
        panel.grid = element_line("grey95"),
        legend.position = "none")

# Visualizando os fitted values do modelo
#Visualizando os fitted values por estudante e por escola
predict(modelo_completo_hlm3, level = 0:2) %>% 
  mutate(estudante = gsub("^.*?\\/","",estudante),
         estudante = as.factor(as.numeric(estudante)),
         escola = as.factor(as.numeric(escola)),
         mes = tempo_estudante_escola$mes,
         desempenho = tempo_estudante_escola$desempenho,
         etjk = resid(modelo_completo_hlm3)) %>% #função resid gera os termos etjk
  rename("fitted fixed" = 3,
         "fitted escola" = 4,
         "fitted estudante" = 5) %>%
  select(escola, estudante, mes, desempenho, everything()) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = F,
                font_size = 18)

#Efetuando predições
#Exemplo: Quais os valores previstos de desempenho escolar no primeiro mês (mes = 1)
#para o estudante "1" da escola "1", sabendo-se que esta escola oferece tempo médio
#de experiência de seus professores igual a 2 anos?
predict(modelo_completo_hlm3, level = 0:2,
        newdata = data.frame(escola = "1",
                             estudante = "1",
                             mes = 1,
                             ativ = c("não","sim"),
                             texp = 2))

#Gráfico com os valores previstos do desempenho escolar ao longo do tempo para os 47 
#primeiros estudantes da amostra (47 estudantes que estão na escola 1)
predict(modelo_completo_hlm3, level = 0:2) %>% 
  mutate(estudante = gsub("^.*?\\/","",estudante),
         estudante = as.factor(as.numeric(estudante)),
         mes = tempo_estudante_escola$mes) %>% 
  rename(fitted_fixed = 3,
         fitted_escola = 4,
         fitted_estudante = 5) %>% 
  right_join(tempo_estudante_escola, 
             by = c("escola","estudante","mes")) %>%
  filter(estudante %in% 1:47) %>%
  ggplot(aes(x = mes, y = fitted_estudante, color = estudante)) +
  geom_smooth(method = "lm", formula = y ~ x, se = F) +
  geom_point(size = 4, alpha = 0.4) +
  guides(color = "none") +
  scale_colour_viridis_d() +
  labs(x = "Mês",
       y = "Desempenho Escolar") +
  theme_bw()


# Finalizando: Comparação com modelo OLS Dummies --------------------------
# Estimando um modelo OLS com as mesmas variáveis do modelo HLM3

# Procedimento n-1 dummies para escolas
base_dummizada <- dummy_cols(.data = tempo_estudante_escola,
                             select_columns = "escola",
                             remove_first_dummy = T,
                             remove_selected_columns = TRUE)

# Estimando um modelo OLS com as mesmas variáveis do modelo HLM3
modelo_ols_dummies <- lm(formula = desempenho ~ . + ativ:mes + texp:mes - estudante,
                         data = base_dummizada)

# Parâmetros do modelo ols_final
summary(modelo_ols_dummies)

# Procedimento stepwise
modelo_ols_dummies_step <- step(object = modelo_ols_dummies,
                                k = qchisq(p = 0.05, df = 1, lower.tail = FALSE))

# Parâmetros do modelo OLS final
summary(modelo_ols_dummies_step)

# Comparando os LL dos modelos HLM3 Completo e OLS com Dummies e Procedimento Stepwise
data.frame(OLS_Final = logLik(modelo_ols_dummies_step),
           HLM3_Completo = logLik(modelo_completo_hlm3)) %>%
  rename(`OLS Final com Stepwise` = 1,
         `HLM3 Completo` = 2) %>% 
  melt() %>%
  ggplot(aes(x = variable, y = (abs(-value)), fill = factor(variable))) +
  geom_bar(stat = "identity") +
  geom_label(aes(label = (round(value,3))), hjust = 1.1, color = "white", size = 6) +
  labs(title = "Comparação do Ganho de LL", 
       y = "LogLik", 
       x = "Modelo Proposto") +
  coord_flip() +
  scale_fill_manual("Legenda:",
                    values = c("orange","#440154FF")) +
  theme(legend.title = element_blank(), 
        panel.background = element_rect("white"),
        legend.position = "none",
        axis.line = element_line())

# Likelihood ratio test (lrtest) para comparação entre os modelos HLM3 Completo
# e OLS com Dummies e Procedimento Stepwise
lrtest(modelo_ols_dummies_step, modelo_completo_hlm3)

# Comparação entre os parãmetros dos modelos (atente-se para a quantidade de
# parâmetros estimados em cada um deles!)
export_summs(modelo_ols_dummies_step, modelo_completo_hlm3,
             model.names = c("OLS com Dummies", "HLM3 Completo"))

# Gráfico para a comparação entre os fitted values dos modelos HLM3 Completo e
# OLS com Dummies e Procedimento Stepwise
predict(modelo_completo_hlm3, level = 0:2) %>% 
  mutate(estudante = gsub("^.*?\\/","",estudante),
         estudante = as.factor(as.numeric(estudante)),
         mes = tempo_estudante_escola$mes) %>% 
  rename(fitted_fixed = 3,
         fitted_escola = 4,
         fitted_estudante = 5) %>% 
  right_join(tempo_estudante_escola, 
             by = c("escola","estudante","mes"))  %>% 
  mutate(fitted_ols = modelo_ols_dummies_step$fitted.values) %>% 
  ggplot() +
  geom_line(aes(x = desempenho, y = desempenho)) +
  geom_smooth(aes(x = desempenho, y = fitted_ols,
                  color = "OLS"), se = F, size = 2)  +
  geom_smooth(aes(x = desempenho, y = fitted_estudante,
                  color = "HLM3"), se = F, size = 2) +
  geom_point(aes(x = desempenho, y = fitted_ols,
                 color = "OLS"), shape = 1, size = 4, alpha = 4)  +
  geom_point(aes(x = desempenho, y = fitted_estudante,
                 color = "HLM3"), shape = 0, size = 4, alpha = 0.4) +
  labs(x = NULL,
       y = NULL) +
  scale_color_manual("Modelos:", values = c("#440154FF","orange")) +
  theme(legend.title = element_blank(), 
        panel.border = element_rect(NA),
        panel.background = element_rect("white"),
        panel.grid = element_line("grey80"),
        legend.position = "bottom")


# ♥ ☺ ♥ ☺ ♥ ☺ ♥ ☺ ♥ ☺ ♥ ☺ ♥ ☺ ♥ ☺ ♥ ☺ FIM ☺ ♥ ☺ ♥ ☺ ♥ ☺ ♥ ☺ ♥ ☺ ♥ ☺ ♥ ☺ ♥ ☺ ♥ #
