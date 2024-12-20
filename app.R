options(repos = c(CRAN = "https://cloud.r-project.org"))

if (!require(shiny)) { install.packages("shiny") }
library(shiny)

#library(shinythemes)

if (!require(plotly)) { install.packages("plotly") }
library(plotly)

if (!require(readxl)) { install.packages("readxl") }
library(readxl)

if (!require(tidyverse)) { install.packages("tidyverse") }
library(tidyverse)

#library(shinydashboard)

if (!require(mapview)) { install.packages("mapview") }
library(mapview)

if (!require(leaflet)) { install.packages("leaflet") }
library(leaflet)

if (!require(bslib)) { install.packages("bslib") }
library(bslib)

if (!require(RColorBrewer)) { install.packages("RColorBrewer") }
library(RColorBrewer)

if (!require(googlesheets4)) { install.packages("googlesheets4") }
library(googlesheets4)

if (!require(tidyverse)) { install.packages("tidyverse") }
library(tidyverse)

if (!require(openssl)) { install.packages("openssl") }
library(openssl)

# if (!require(shinycssloaders)) { install.packages("shinycssloaders") }
# library(shinycssloaders)

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

#===============================================================================
# Autenticando com credenciais criptografadas

# Recuperando chave e iv codificados em hexadecimal
chave_codificada <- Sys.getenv("CHAVE")
iv_codificado <- Sys.getenv("IV")

# Descodificando de hexadecimal para formato original
chave <- base64_decode(chave_codificada)
# chave <- as.raw(chave)
iv <- base64_decode(iv_codificado)

# Lendo arquivo criptografado
path_arquivo_criptografado <- "credentials/encrypted-key.bin"
conteudo_criptografado <- readBin(path_arquivo_criptografado,
                                  what = "raw",
                                  n = file.info(path_arquivo_criptografado)$size)

# Descriptografando credenciais
conteudo_descriptografado <- aes_cbc_decrypt(conteudo_criptografado,
                                             key = chave,
                                             iv = iv)
# Transformando em texto
conteudo_descriptografado_txt <- rawToChar(conteudo_descriptografado)

# Colocando no arquivo temporário para sre usado na autenticação
credenciais_temp <- tempfile(fileext = ".json")
writeLines(conteudo_descriptografado_txt, credenciais_temp)

# Finalmente fazendo a autenticação
gs4_auth(path = credenciais_temp)


#=============================================================================
# Definindo a interface do usuário (UI)
ui <- navbarPage(
  includeCSS("custom.css"),
  
  title = div(
    class = "logo-div",
    # style = "display: flex; align-items: center; gap: 10px; margin: 0;",
    tags$a(
      href = link_observatorio,
      target = "_blank",
      
      tags$img(
        src = path_logo,
        height = "100%",  # Adjust the height as needed
      )
    ),
    tags$a(
      href = link_observatorio,  # Replace with the second link
      target = "_blank",  # Opens the link in a new tab
      tags$span("Home", style = "color: inherit; text-decoration: none; cursor: pointer;")
    )
  ),
  
  # Set window title to avoid warning
  windowTitle = "Calculadora",
  
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