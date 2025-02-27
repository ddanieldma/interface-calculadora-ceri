library(shiny)
library(plotly)
library(googlesheets4)
library(tidyverse)
library(googledrive)

source("calculadora/modulo_tarifas.R")

SEGMENTO_RESIDENCIAL <- "residencial"

#===============================================================================
# UI.
comp_residencial_ui <- function(id){
  message("===================================================================")
  message("UI da aba de tarifas para valor fixo do segmento residencial \n")
  
  comparacao_ui(id, SEGMENTO_RESIDENCIAL, CONSUMO_PADRAO_RESIDENCIAL)
}