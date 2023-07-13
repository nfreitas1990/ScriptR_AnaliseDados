


# Script Organização ------------------------------------------------------


# Pacotes -----------------------------------------------------------------
library(devtools)
library(usethis)
library(pkgdown)
library(testthat)
library(roxygen2)
library(knitr)
library(tidyverse)

# Checando se o pc está pronto
devtools::has_devel()
# Your system is ready to build packages!



# 1. Controle de Versões -----------------------------------------------------
#     Devemos configurar o R para controle de versões com o Github
#     Olhar scrpit de GitHub


# 2. Criar Projeto -----------------------------------------------------------
usethis::create_project() #colocar o caminho absoluto



