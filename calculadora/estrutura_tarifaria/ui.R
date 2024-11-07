# UI da aba de estrutura tarifária.

library(shiny)
        
#===============================================================================
# UI
estrutura_tarifaria_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    useShinyjs(),
    
    titlePanel(
      h1("Estrutura tarifária da distribuição", align = "center")
    ),
    sidebarLayout(
      sidebarPanel(
        width = 2,
        selectInput(ns("nome_distribuidora_estrutura"), "Distribuidora:", choices = NULL),
        div(
          actionButton(ns("atualizar_estrutura"), "Criar tabelas"),
          style = "display: flex; justify-content: center"
        )
      ),
      mainPanel(
        width = 9,  # Painel principal maior.
        # UI dinâmica para o botão de download dos dados.
        uiOutput(ns("download_button_ui")),
        uiOutput(ns("tabelas"))
      )
    )
  )
  
  # tabPanel(
  #   "Estrutura tarifária",
  #   fluidPage(
  #     add_busy_spinner(),
      
      # tagList(
      #   # Inicializando shinyjs
      #   # useShinyjs(),
      #   
      #   
      #   )
    #   )
    # )
}