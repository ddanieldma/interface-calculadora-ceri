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
# Imports das abas da calculadora.
# Importando aba da calculadora de tarifas.
source("calculadora/calculadora_tarifas/ui.R")
source("calculadora/calculadora_tarifas/server.R")

# Importando aba de estrutura tarifária.
source("calculadora/estrutura_tarifaria/ui.R")
source("calculadora/estrutura_tarifaria/server.R")

# Importando aba de comparação de tarifas.
# Comercial
source("calculadora/comparacao_tarifas_comercial/ui.R")
source("calculadora/comparacao_tarifas_comercial/server.R")
# Industrial
source("calculadora/comparacao_tarifas_industrial/ui.R")
source("calculadora/comparacao_tarifas_industrial/server.R")
# Residencial
source("calculadora/comparacao_tarifas_residencial/ui.R")
source("calculadora/comparacao_tarifas_residencial/server.R")


#=============================================================================
# Importando página de sobre.
source("sobre.R")

#=============================================================================
# Importando gráficos.
source("graficos.R")


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
  #theme = shinytheme("cerulean"),  # Escolha um tema base
  includeCSS("custom.css"),    # Inclua o CSS customizado
  
  title = "Dashboard do Gás",
  
  # Abertura
  tabPanel("Abertura",
           fluidPage(
             #h2("Contratos Vigentes"),
             tabsetPanel(
               tabPanel("Contratos Vigentes", h3(class = "text-blue", "Número total de contratos vigentes por ano"),
                        plotlyOutput('grafico_vig')  # Onde o gráfico será exibido
               ),
               tabPanel("Vigências por tipo de Comprador", h3(class = "text-blue", "Vigências por tipo de comprador"),
                        plotlyOutput("fig_vigc")  # Onde o gráfico será exibido
               ),   
               tabPanel("Assinaturas", h3(class = "text-blue", "Quantidade de contratos assinados por tipo de comprador"),
                        plotlyOutput("fig_ass")  # Onde o gráfico será exibido
               )
             ))),
  
  # Regulação
  tabPanel("Regulação",
           fluidPage(
             #h2("Conteúdo da Página 2"),
             tabsetPanel(
               tabPanel("Assinaturas", h3(class = "text-blue", "Limite para enquadramento como consumidor livre"),
                        plotlyOutput("fig_cl")  # Onde o gráfico será exibido
               )
             )
           )),
  
  # Oferta e Demanda
  tabPanel("Oferta e Demanda",
           fluidPage(
             #h2("Conteúdo da Página 3"),
             tabsetPanel(
               tabPanel("Oferta Nacional", h3(class = "text-blue", "Oferta nacional"),
                        plotlyOutput("fig_ofertanac")  # Onde o gráfico será exibido
               ),
               tabPanel("Oferta Internacional", h3(class = "text-blue", " Oferta importada"),
                        plotlyOutput("fig_ofertaimp")  # Onde o gráfico será exibido
               ),
               tabPanel("Reservas", h3(class = "text-blue", "Reservas relativas"),
                        plotlyOutput("fig_res_prod")  # Onde o gráfico será exibido
               ),
               tabPanel("Produção", h3(class = "text-blue", "Produção de petróleo e gás natural (2022)"),
                        fluidRow(
                          column(6, plotlyOutput("fig_teste")),   # Coloca o primeiro gráfico na metade esquerda
                          column(6, plotlyOutput("fig_teste1"))   # Coloca o segundo gráfico na metade direita
                        )),
               tabPanel("Oferta Interna", h3(class = "text-blue", "Oferta interna de gás natural"),
                        plotlyOutput("fig_ofertaint")  # Onde o gráfico será exibido
               ),
               tabPanel("Demanda por segmento", h3(class = "text-blue", "Demanda por segmento de consumo"),
                        plotlyOutput("fig_demandaseg")  # Onde o gráfico será exibido
               ),
               tabPanel("Demanda por distribuidora", h3(class = "text-blue", "Demanda por distribuidora (com termelétricas)"),
                        plotlyOutput("fig_demandist")  # Onde o gráfico será exibido
               ),
               tabPanel("Consumo não termelétrico", h3(class = "text-blue", "Consumo por fonte"),
                        plotlyOutput("fig_consumo70")  # Onde o gráfico será exibido
               ),
               tabPanel("Balanço Mensal", h3(class = "text-blue", "Balanço de gás nacional mensal"),
                        plotlyOutput("fig_balanco")  # Onde o gráfico será exibido
               ),
               tabPanel("Balanço Anual", h3(class = "text-blue", "Balanço de gás nacional anual"),
                        plotlyOutput("fig_balanco_anual")  # Onde o gráfico será exibido
               )
             )
           )),
  
  # Comercialização
  tabPanel("Comercialização",
           fluidPage(
             #h2("Conteúdo da Página 4"),
             tabsetPanel(
               tabPanel("Quantidade Contratada por Região", h3(class = "text-blue", "Quantidade contratada diarimente por região"),
                        plotlyOutput("fig_qdcreg")  # Onde o gráfico será exibido
               ),
               tabPanel("Quantidade Contratada por UF", h3(class = "text-blue", "Quantidade contratada diarimente por UF"),
                        plotlyOutput("fig_qdcuf")  # Onde o gráfico será exibido
               ),
               tabPanel("Comercialização entre Produtores", h3(class = "text-blue", "Quantidade diária contratada por fornecedor"),
                        plotlyOutput("fig_qdcfor")  # Onde o gráfico será exibido
               ),
               tabPanel("Contratação das Distribuidoras", h3(class = "text-blue", "Quantidade média diariamente contratada por modalidade de contratação"),
                        plotlyOutput("fig_qdcdist")  # Onde o gráfico será exibido
               ),
               tabPanel("Modalidades de Contratação", h3(class = "text-blue", "Quantidade média diariamente contratada por modalidade de contratação"),
                        plotlyOutput("fig_qdcmod")  # Onde o gráfico será exibido
               ),
             )
           )),
  
  # Distribuição
  tabPanel("Distribuição",
           fluidPage(
             #h2("Conteúdo da Página 4"), htmlOutput("map")
             tabsetPanel(
               tabPanel("Áreas de Concessão", h3(class = "text-blue", "Áreas de concessão"),
                        leafletOutput("map")
               ),
               tabPanel("Extensão das Redes", h3(class = "text-blue", "Extensões das redes de transporte e distribuição"),
                        plotlyOutput("fig_ext")  # Onde o gráfico será exibido
               ),
             )
           )),
  
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
  tabPanel("Tarifas para consumo médio",
           fluidPage(
             tabsetPanel(
               tabPanel("Residencial",
                        comp_residencial_ui("comp_residencial_module")
               ),
               tabPanel("Industrial",
                        comp_industrial_ui("comp_industrial_module")
               ),
               tabPanel("Comercial",
                        comp_comercial_ui("comp_comercial_module")
               )
             )
           )
  ),
  
  # Página 5
  tabPanel("Sobre",
           fluidPage(
             sobre_ui("sobre_module")
           )
  )
)

#=============================================================================
# Servidor.
server <- function(input, output, session) {
  # Renderize o gráfico na primeira página, opção 1
  output$grafico_vig <- plotly::renderPlotly({
    fig_vig
  })
  
  # Renderize o gráfico na primeira página, opção 1
  output$fig_vigc <- plotly::renderPlotly({
    fig_vigc
  })
  
  # Renderize o gráfico na primeira página, opção 1
  output$fig_ass <- plotly::renderPlotly({
    fig_ass
  })
  
  # Renderize o gráfico na primeira página, opção 1
  output$fig_cl <- plotly::renderPlotly({
    fig_cl
  })
  
  # Renderize o gráfico na primeira página, opção 1
  output$fig_ofertanac <- plotly::renderPlotly({
    fig_ofertanac
  })
  
  # Renderize o gráfico na primeira página, opção 1
  output$fig_ofertaimp <- plotly::renderPlotly({
    fig_ofertaimp
  })
  
  # Renderize o gráfico na primeira página, opção 1
  output$fig_res_prod <- plotly::renderPlotly({
    fig_res_prod
  })
  
  # Renderize o gráfico na primeira página, opção 1
  output$fig_teste <- plotly::renderPlotly({
    fig_teste
  })
  
  # Renderize o gráfico na primeira página, opção 1
  output$fig_teste1 <- plotly::renderPlotly({
    fig_teste1
  })
  
  # Renderize o gráfico na primeira página, opção 1
  output$fig_ofertaint <- plotly::renderPlotly({
    fig_ofertaint
  })
  
  # Renderize o gráfico na primeira página, opção 1
  output$fig_demandaseg <- plotly::renderPlotly({
    fig_demandaseg
  })
  
  # Renderize o gráfico na primeira página, opção 1
  output$fig_demandist <- plotly::renderPlotly({
    fig_demandist
  })
  
  # Renderize o gráfico na primeira página, opção 1
  output$fig_demandist_st <- plotly::renderPlotly({
    fig_demandist_st
  })
  
  # Renderize o gráfico na primeira página, opção 1
  output$fig_consumo70 <- plotly::renderPlotly({
    fig_consumo70
  })
  
  # Renderize o gráfico na primeira página, opção 1
  output$fig_balanco <- plotly::renderPlotly({
    fig_balanco
  })
  
  # Renderize o gráfico na primeira página, opção 1
  output$fig_balanco_anual <- plotly::renderPlotly({
    fig_balanco_anual
  })
  
  # Renderize o gráfico na primeira página, opção 1
  output$fig_qdcreg <- plotly::renderPlotly({
    fig_qdcreg
  })
  
  # Renderize o gráfico na primeira página, opção 1
  output$fig_qdcuf <- plotly::renderPlotly({
    fig_qdcuf
  })
  
  # Renderize o gráfico na primeira página, opção 1
  output$fig_qdcfor <- plotly::renderPlotly({
    fig_qdcfor
  })
  
  # Renderize o gráfico na primeira página, opção 1
  output$fig_qdcdist <- plotly::renderPlotly({
    fig_qdcdist
  })
  
  # Renderize o gráfico na primeira página, opção 1
  output$fig_qdcmod <- plotly::renderPlotly({
    fig_qdcmod
  })
  
  
  # Distribuição
  areas_concedidas <- sf::read_sf('areas_concedidas/areas_concedidas.shp')
  rename(areas_concedidas, Distribuidora = dstrbdr) -> areas_concedidas
  ##tinha esquecido da distribuidora do RN##
  areas_concedidas[14,1] <- "Potigás"
  m <- mapview::mapview(areas_concedidas, zcol = "Distribuidora", layer.name = "Distribuidora")
  output$map <- renderLeaflet({
    m@map
  })
  
  # Renderize o gráfico na primeira página, opção 1
  output$fig_ext <- plotly::renderPlotly({
    fig_ext
  })
  
  #=============================================================================
  # Calculadora de tarifas.
  calculadora_tarifas_server("tarifas_module")
  
  #=============================================================================
  # Estrutura tarifária.
  estrutura_tarifaria_server("estrutura_module")
  
  #=============================================================================
  # Comparação de tarifas.
  
  # Aba de residencial
  comp_residencial_server("comp_residencial_module")
  # Aba de industrial
  comp_industrial_server("comp_industrial_module")
  # Aba de comercial
  comp_comercial_server("comp_comercial_module")
}


#=============================================================================
# Execute o aplicativo
shinyApp(ui, server)