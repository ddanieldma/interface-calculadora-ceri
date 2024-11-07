library(shiny)
library(plotly)
library(googlesheets4)
library(tidyverse)
library(googledrive)

source("calculadora/modulo_tarifas.R")

SEGMENTO_INDUSTRIAL <- "industrial"

#===============================================================================
# UI.
comp_industrial_ui <- function(id){
  message("===================================================================")
  message("UI da aba de tarifas para valor fixo do segmento industrial \n")
  
  comparacao_ui(id, SEGMENTO_INDUSTRIAL, CONSUMO_PADRAO_INDUSTRIAL)
}