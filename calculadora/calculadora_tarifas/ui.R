library(shiny)

source("calculadora/modulo_tarifas.R")

calculadora_tarifas_ui <- function(id) {
  ns <- NS(id)
  
  tabPanel(
    "Calculadora de tarifas",
    fluidPage(
      add_busy_spinner(),
      tagList(
        titlePanel(
          h1("Calculadora de tarifas* do mercado de gás natural no Brasil",
             align = "center")
        ),
        fluidRow(
          column(2,
                 tags$div(
                   h4("Tarifas de distribuidoras"),
                   wellPanel(
                     selectInput(ns("classe_consumo_tarifas"),
                                 "Classe de Consumo:",
                                 choices = c("Residencial", "Industrial", "Comercial")),
                     numericInput(ns("nivel_consumo_tarifas"),
                                  "Nível de consumo (m³):",
                                  value = 500),
                     div(
                       actionButton(ns("update_tarifas"), "Criar visualizações"),
                       style = "display: flex; justify-content: center;"
                     )
                   ),     
                   style = 
                     "display: flex;
                                     flex-direction: column;
                                     align-items: center;"
                 )
          ),
          column(10,
                 # UI dinâmica para o botão de download dos dados.
                 # uiOutput(ns("download_button_ui")),
                 # withSpinner(plotlyOutput(ns('grafico_tarifas')), type = 8)
                 plotlyOutput(ns('grafico_tarifas'))
          )
        ),
        fluidRow(
          column(12,
                 uiOutput(ns('tabela_ui_tarifas'))
          )
        ),
        fluidRow(
          column(12,
                 h4("* Tarifas com impostos.")
          )
        )
      )
    )
  )
}