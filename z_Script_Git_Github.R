
# Script organizacao
# Aula 1
# Natalia Freitas
# 16.04.2022



devtools::session_info()


# Desabilitar o controle do R-Studio --------------------------------------
usethis::use_blank_slate()


# Alterar -----------------------------------------------------------------
# tools - Global options - General
# desmarcar Restore .RData
# Save workspace to .Rdata on exit - trocar para "Never" 

# Instalar|Configurar: GIT e GITHUB --------------------------------------------------
# Essa configuracao soh faz uma vez na vida, nao precisa fazer sempre

# Passo 1: Crie e configure o projeto
usethis:: create_project("../Projeto_cursoR/")

# Passo 2: Configure o GitHub
usethis::use_git_config(user.name = "Natália Freitas", user.email = "nfreitas1990@gmail.com")



# Versionamento -----------------------------------------------------------
# Esses passos faremos sempre ao abrir um novo projeto

# Passo 3: Adicione o Git
# Rodando o comando abaixo na pasta do projeto  voce adiciona controle de versao
usethis::use_git()


# Passo 4: criar um personalacess token (nao precisa refazer ao abrir novo projeto)
# O comando abaixo vai criar uma senha para que vc nao precise colocar sua senha net
usethis::create_github_token()

# Esse comando serah usado para armazenar o token criado
gitcreds::gitcreds_set()


# Passo 5: Add o Github
# O comando abaixo sincroniza a pasta com o GitHub
usethis::use_github()

# obs: o argumento "protocol= ssh" pode ser usado caso queira trocar caso a configuracao tenha sido feita para esse protocolo. Esse protocolo eh indicado para quem usa linux.


# Passo 6: Stage e Commit
# clica no botao commit e seleciona as mudancas que vao ser enviadas

# Passo 7: PUSH
# Push (ou dar push) significa atualizar o seu repositório remoto (GitHub) com os arquivos que você commitou no passo anterior.

# Passo 8: PULL
# Pull é a ação inversa do Push: você trará a versão mais recente dos arquivos do seu repositório remoto (GitHub) para a sua máquina (caso você tenha subido uma versão de um outro computador ou uma outra pessoa tenha subido uma atualização).



# Solucionando Erro -------------------------------------------------------

# Caso ocorra algum erro sempre usar o resultado dessa funcao caso vah pedir ajuda
usethis::git_sitrep()

# Criar arquivo read me para facilitar entendimento
usethis::use_readme_md()



# Cuidados ----------------------------------------------------------------
# Arquivos q não quer q seja versionado

usethis::edit_git_ignore(scope = )



# Trabalhando com Projetos ------------------------------------------------


    # Formas de Trabalhar:

#    1. Criar um projeto no R e iniciar o versionamento:
#       Ao abrir novo projeto devemos conectar o Git e Github entao usaremos apenas:
            usethis::use_git()
            usethis::use_github()


#    2. Clonando um repositorio existente no GITHUB usando o Rstudio


#    3. A partir de um repositório existente no GitHub, fazer um fork e clonar usando funções do usethis.
            usethis::create_from_github(repo_spec = "R-Ladies-Sao-Paulo/RLadies-Brasil",
                                        destdir = "C:/Users/Natalia/Documents/0_CursoR/Projetos",
                                        fork = TRUE)



