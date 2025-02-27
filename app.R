options(repos = c(CRAN = "https://cloud.r-project.org"))

if (!require(shiny)) { install.packages("shiny") }
library(shiny)

if (!require(plotly)) { install.packages("plotly") }
library(plotly)

if (!require(readxl)) { install.packages("readxl") }
library(readxl)

if (!require(RColorBrewer)) { install.packages("RColorBrewer") }
library(RColorBrewer)

if (!require(googlesheets4)) { install.packages("googlesheets4") }
library(googlesheets4)

if (!require(tidyverse)) { install.packages("tidyverse") }
library(tidyverse)

if (!require(openssl)) { install.packages("openssl") }
library(openssl)

if (!require(jsonlite)) { install.packages("jsonlite") }
library(jsonlite)

if (!require(shinybusy)) { install.packages("shinybusy") }
library(shinybusy)

if (!require(shinyjs)) { install.packages("shinyjs") }
library(shinyjs)


#=============================================================================
# Variáveis
# Link para o obsrevatório do gás
link_observatorio <- "https://hml-observatoriodogas.fgv.br/"
# Caminho da logo
path_logo <- "logo-observatorio-gas.png"


#=============================================================================
# Imports das abas da calculadora.
# Importando aba da calculadora de tarifas.
source("calculadora/calculadora_tarifas/ui.R")
source("calculadora/calculadora_tarifas/server.R")

# Importando aba de estrutura tarifária.
source("calculadora/estrutura_tarifaria/ui.R")
source("calculadora/estrutura_tarifaria/server.R")

# Função para descriptografar chave
source("decrypt_key.R")

#===============================================================================
# Autenticando com credenciais criptografadas
# Recuperando chave e iv codificados em hexadecimal
chave_codificada <- Sys.getenv("CHAVE")
iv_codificado <- Sys.getenv("IV")

conteudo_descriptografado_txt <- decript_key(chave_codificada, iv_codificado)

# Colocando no arquivo temporário para ser usado na autenticação
credenciais_temp <- tempfile(fileext = ".json")
writeLines(conteudo_descriptografado_txt, credenciais_temp)

# Finalmente fazendo a autenticação
gs4_auth(path = credenciais_temp)


#=============================================================================
# Definindo a interface do usuário (UI)
ui <- fluidPage(
  includeCSS("custom.css"),
  
  navbarPage(
    # Colocando logo e botão de home no começo da barra superior
    title = div(
      class = "logo-div",
      # Logo como link para site do observatório
      tags$a(
        href = link_observatorio,
        target = "_blank",
  
        tags$img(
          src = path_logo,
          height = "100%",
        )
      ),
      # Botão "Home" também como link para página do observatório
      tags$a(
        href = link_observatorio,
        target = "_blank",
        tags$span("Home", style = "color: inherit; text-decoration: none; cursor: pointer;")
      )
    ),
  
    # Abas da calculadora.
    tabPanel("Calculadora de tarifas",
             fluidPage(
               calculadora_tarifas_ui("tarifas_module"),
             )
    ),
    tabPanel("Estrutura tarifária",
             fluidPage(
               estrutura_tarifaria_ui("estrutura_module")
             )
    ),
  )
)

#=============================================================================
# Servidor.
server <- function(input, output, session) {
  #=============================================================================
  # Calculadora de tarifas.
  calculadora_tarifas_server("tarifas_module")
  
  #=============================================================================
  # Estrutura tarifária.
  estrutura_tarifaria_server("estrutura_module")
}


#=============================================================================
# Execute o aplicativo
shinyApp(ui, server)