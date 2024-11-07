library(plotly)
library(googlesheets4)
library(tidyverse)
library(googledrive)

# Importanto função que busca dados na google sheet.
source("calculadora/modulo_tarifas.R")

message("Rodando aba de comparação tarifária para o segmento comercial")

SEGMENTO_COMERCIAL <- "comercial"


#===============================================================================
# Servidor.
comp_comercial_server <- function(id) {
  message("===================================================================")
  message("Servidor da aba de tarifas para valor fixo do segmento comercial \n")
  
  comparacao_server(id, SEGMENTO_COMERCIAL, CONSUMO_PADRAO_COMERCIAL)
}