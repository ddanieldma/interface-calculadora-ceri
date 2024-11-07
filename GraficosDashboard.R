########## Instalação de Pacotes
#install.packages("geobr")
library(geobr)
#install.packages("censobr")
library(censobr)
#########
#install.packages("mapview")
library(mapview)
#install.packages("dplyr")
library(dplyr)
#install.packages("ggplot2")
library(ggplot2)
#install.packages("plotly")
library(plotly)
#install.packages("expss")
#library(expss)
#install.packages("raster")
library(raster)
###
library(leafsync)
#install.packages("leaflet.extras2")
library(leaflet.extras2)
#install.packages("rgdal")
#require("rgdal")
#install.packages("htmlwidgets")
library(htmlwidgets)
library(wesanderson)
library(leafpop)

#install.packages("stringr")
library(stringr)

rm(list=ls())

###############################################################################
###############################################################################
###############################################################################
### Gráficos do dasboard do gás

install.packages("pacman")
install.packages("lubridate")
pacman::p_load(tidyverse, plotly, lubridate, sf, geobr, dplyr)

rm(list = ls())

#------------------------------------------------------------------------------#
#Gráfico de extensão das redes de transporte e distribuição
ext_rede<-read.csv2("graph_files/ext_rede.csv")
l <- list(
  font = list(
  family = "sans-serif",
  #size = 12,
  color = "#000"),
  bgcolor = "#E2E2E2",
  bordercolor = "#FFFFFF",
  borderwidth = 2,
  orientation = "h",   # show entries horizontally
  xanchor = "right",  # use center of legend as anchor
  x = 0.2, y =  - 0.2) 


# THIS CHANGE
m <- list(
  l = 10,
  r = 10,
  b = 100,
  t = 10,
  pad = 20
)

fig_ext <- plotly::plot_ly(ext_rede, 
                   x = ~ano, 
                   y = ~transport,
                   type = 'scatter',
                   mode = 'lines', 
                   name = 'Transporte')

fig_ext <- fig_ext %>% plotly::add_trace(y = ~distribuicao, 
                                 name = 'Distribuição', 
                                 mode = 'lines') 

fig_ext <- fig_ext %>% plotly::layout(title = 'Extensões das Redes de Transporte e Distribuição',
                              xaxis = list(title = 'Ano'),
                              yaxis = list (title = 'Quilômetros'),
                              #legend = list(x = 0, y = 0.8),
                              legend = list(x = 0.5, y = 1, 
                              xanchor = 'right', yanchor = 'top'),
                              margin= m) %>%
  layout(autosize = TRUE)

fig_ext

htmlwidgets::saveWidget(as_widget(fig_ext), "ext_rede.html")
#------------------------------------------------------------------------------#

# Gráfico - Índice HHI das fornecedoras de gás para distribuidoras

hhi<-read.csv2("graph_files/hhi.csv")

hhi$data <-stringr::str_replace_all(hhi$data, "/", "-")

hhi$data <- lubridate::as_date(hhi$data, format = '%d-%m-%Y')

hhi %>%
  filter(data >= "2024-01-01") -> hhi



fig_hhi <- plotly::plot_ly(hhi,
                           type = 'scatter',
                           mode = 'lines')%>% add_trace(x = ~data, 
                                                        y = ~hhi,
                                                        name = 'índice')


fig_hhi <- fig_hhi %>% plotly::layout(title = 'Índice HHI QDC',
                                      xaxis = list(title = 'Ano'),
                                      yaxis = list (title = 'HHI'), 
                                      showlegend = FALSE)

htmlwidgets::saveWidget(as_widget(fig_hhi), "hhi.html")
#------------------------------------------------------------------------------#

#Market Share da Petrobras (no mercado de gás qdc)

ms_petro<-read.csv2("graph_files/marketshare_petro.csv")

ms_petro$data <- stringr::str_replace_all(ms_petro$data, "/", "-")

ms_petro$data <- lubridate::as_date(ms_petro$data, format = '%d-%m-%Y')

ms_petro %>%
  dplyr::filter(data >= "2024-01-01") -> ms_petro

fig_ms <- plotly::plot_ly(ms_petro,
                          type = 'scatter',
                          mode = 'lines')%>% add_trace(x = ~data, 
                                                       y = ~ms,
                                                       name = 'Market Share')

fig_ms <- fig_ms %>% plotly::layout(title = 'Market Share da Petrobras - QDC',
                                      xaxis = list(title = 'Ano'),
                                      yaxis = list (title = '%'),
                                      showlegend = FALSE)

fig_ms
htmlwidgets::saveWidget(as_widget(fig_ms), "fig_ms.html")
#------------------------------------------------------------------------------#

#QDC empilhado - por fornecedor#


qdc_for <- read.csv2("graph_files/qdc_fornecedor.csv")


qdc_for$date <- stringr::str_replace_all(qdc_for$date, "/", "-")
qdc_for$date <- lubridate::as_date(qdc_for$date, format = '%d-%m-%Y')

qdc_for %>%
  dplyr::filter(date >= "2024-01-01" ) -> qdc_for

fig_qdcfor <- plotly::plot_ly(qdc_for,
                              x = ~date,
                              y = ~petrobras,
                              name = 'Petrobrás', type = 'scatter', mode = 'none', stackgroup = 'one')

fig_qdcfor <- fig_qdcfor %>% add_trace (y = ~Alvopetro, name = "Alvopetro")
fig_qdcfor <- fig_qdcfor %>% add_trace (y = ~Compass, name = "Compass")
fig_qdcfor <- fig_qdcfor %>% add_trace (y = ~ERG, name = "ERG")
fig_qdcfor <- fig_qdcfor %>% add_trace (y = ~Eagle, name = "Eagle")
fig_qdcfor <- fig_qdcfor %>% add_trace (y = ~Equinor, name = "GALP")
fig_qdcfor <- fig_qdcfor %>% add_trace (y = ~NFE, name = "NFE")
fig_qdcfor <- fig_qdcfor %>% add_trace (y = ~Origem, name = "Origem")
fig_qdcfor <- fig_qdcfor %>% add_trace (y = ~tresR, name = "3R")
fig_qdcfor <- fig_qdcfor %>% add_trace (y = ~Petroreconcavo, name = "Petrorecôncavo")
fig_qdcfor <- fig_qdcfor %>% add_trace (y = ~Potiguar, name = "Potiguar")
fig_qdcfor <- fig_qdcfor %>% add_trace (y = ~Shell, name = "Shell")
fig_qdcfor <- fig_qdcfor %>% add_trace (y = ~Tradener, name = "Tradener")


fig_qdcfor <- fig_qdcfor %>% plotly::layout(title = 'Volume de QDC por Fornecedor',
                                            xaxis = list(title = 'Ano'),
                                            yaxis = list (title = 'Volume em m³/dia'))

htmlwidgets::saveWidget(as_widget(fig_qdcfor), "fig_qdcfor.html")
#------------------------------------------------------------------------------#
#QDC médio#

qdc_med <- read.csv2("graph_files/qdc_medio.csv")

qdc_med$data <- stringr::str_replace_all(qdc_med$data, "/", "-")
qdc_med$data <- lubridate::as_date(qdc_med$data, format = '%d-%m-%Y')

qdc_med %>% filter(data >= "2024-01-01") -> qdc_med

fig_qdcmed <- plotly::plot_ly(qdc_med,
                              x = ~data,
                              y = ~qdc,
                              type = 'scatter',
                              mode = 'lines',
                              name = 'qdc medio')

fig_qdcmed <- fig_qdcmed %>%  plotly::layout(title = 'QDC Médio das Distribuidoras',
                                             xaxis = list(title = 'Ano'),
                                             yaxis = list (title = 'Volume em m³/dia'))

fig_qdcmed

htmlwidgets::saveWidget(as_widget(fig_qdcmed), "fig_qdcmed.html")


#------------------------------------------------------------------------------#
#QDC por modalidade de contratação#
qdc_mod<- read.csv2("graph_files/qdc_mod.csv")

qdc_mod$data <- str_replace_all(qdc_mod$data, "/", "-")

qdc_mod$data <- lubridate::as_date(qdc_mod$data, format = '%d-%m-%Y')

qdc_mod%>%
  dplyr::filter(data >= "2024-01-01")-> qdc_mod

fig_qdcmod <- plotly::plot_ly(qdc_mod,
                              x = ~data,
                              y = ~firme.inflexivel,
                              name = 'Firme Inflexível', type = 'scatter', mode = 'none', stackgroup = 'one')

fig_qdcmod <- fig_qdcmod %>% add_trace (y = ~firme, name = "Firme")
fig_qdcmod <- fig_qdcmod %>% add_trace (y = ~interruptivel, name = "Interruptivel")
fig_qdcmod <- fig_qdcmod %>% add_trace (y = ~interruptivel, name = "Interruptivel")


fig_qdcmod <- fig_qdcmod %>%  plotly::layout(title = 'QDC Médio por Modalidade de Contratação',
                                             xaxis = list(title = 'Ano'),
                                             yaxis = list (title = 'Volume em m³/dia'),
                                             legend=list(title=list(text='<b> Modalidade </b>'))) %>%
  layout(autosize = TRUE)

htmlwidgets::saveWidget(as_widget(fig_qdcmod), "fig_qdcmod.html")


#------------------------------------------------------------------------------#
#QDC empilhada por região#

qdc_reg<- read.csv2("graph_files/qdc_reg.csv")

qdc_reg %>% dplyr::rename(data = regiao) -> qdc_reg

qdc_reg$data <- stringr::str_replace_all(qdc_reg$data, "/", "-")


qdc_reg$data <- lubridate::as_date(qdc_reg$data, format = '%d-%m-%Y')

qdc_reg%>%
  dplyr::filter(data >= "2024-01-01")-> qdc_reg

fig_qdcreg <- plotly::plot_ly(qdc_reg,
                              x = ~data,
                              y = ~sudeste,
                              name = 'Sudeste', type = 'scatter', mode = 'none', stackgroup = 'one')


fig_qdcreg <- fig_qdcreg %>% add_trace (y = ~sul, name = "Sul")
fig_qdcreg <- fig_qdcreg %>% add_trace(y = ~nordeste, name = "Nordeste")
fig_qdcreg <- fig_qdcreg %>% add_trace(y = ~centrooeste, name = "Centro-Oeste")



fig_qdcreg <- fig_qdcreg %>%  plotly::layout(title = 'QDC contratada por região',
                                             xaxis = list(title = 'Ano'),
                                             yaxis = list (title = 'Volume em m³/dia'),
                                             legend=list(title=list(text='<b> Região </b>'))) %>%
  layout(autosize = TRUE)


htmlwidgets::saveWidget(as_widget(fig_qdcreg), "fig_qdcreg.html")

#------------------------------------------------------------------------------#
#QDC for estado#
qdc_uf<- read.csv2("graph_files/qdc_uf.csv")

qdc_uf$data <- stringr::str_replace_all(qdc_uf$data, "/", "-")


qdc_uf$data <- lubridate::as_date(qdc_uf$data, format = '%d-%m-%Y')

qdc_uf%>%
  dplyr::filter(data >= "2024-01-01")-> qdc_uf


fig_qdcuf <- plotly::plot_ly(qdc_uf,
                             x = ~data,
                             y = ~SP,
                             name = 'São Paulo', type = 'scatter', mode = 'none', stackgroup = 'one')


fig_qdcuf <- fig_qdcuf %>% add_trace (y = ~RJ, name = "Rio de Janeiro")
fig_qdcuf <- fig_qdcuf %>% add_trace (y = ~ES, name = "Espírito Santo")
fig_qdcuf <- fig_qdcuf %>% add_trace (y = ~MG, name = "Minas Gerais")
fig_qdcuf <- fig_qdcuf %>% add_trace (y = ~PR, name = "Paraná")
fig_qdcuf <- fig_qdcuf %>% add_trace (y = ~SC, name = "Santa Catarina")
fig_qdcuf <- fig_qdcuf %>% add_trace (y = ~RS, name = "Rio Grande do Sul")
fig_qdcuf <- fig_qdcuf %>% add_trace (y = ~BA, name = "Bahia")
fig_qdcuf <- fig_qdcuf %>% add_trace (y = ~CE, name = "Ceará")
fig_qdcuf <- fig_qdcuf %>% add_trace (y = ~PB, name = "Paraíba")
fig_qdcuf <- fig_qdcuf %>% add_trace (y = ~PE, name = "Pernambuco")
fig_qdcuf <- fig_qdcuf %>% add_trace (y = ~AL, name = "Alagoas")
fig_qdcuf <- fig_qdcuf %>% add_trace (y = ~SE, name = "Sergipe")
fig_qdcuf <- fig_qdcuf %>% add_trace (y = ~RN, name = "Rio Grande do Norte")
fig_qdcuf <- fig_qdcuf %>% add_trace (y = ~MS, name = "Mato Grosso do Sul")


fig_qdcuf <- fig_qdcuf %>%  plotly::layout(title = 'QDC contratada por Estado',
                                           xaxis = list(title = 'Ano'),
                                           yaxis = list (title = 'Volume em m³/dia'),
                                           legend=list(title=list(text='<b> Estado </b>',
                                                       orientation = "h",   # show entries horizontally
                                                       xanchor = "center",  # use center of legend as anchor
                                                       x = 0.5))) %>% 
                                                       layout(margin = list(l = 50, r = 50, b = 50, t = 50, pad = 10))

fig_qdcuf 

htmlwidgets::saveWidget(as_widget(fig_qdcuf), "fig_qdcuf.html")

#------------------------------------------------------------------------------#
#QDC por distribuidora

qdc_dist<- read.csv2("graph_files/qdc_dist.csv")

qdc_dist$data <- stringr::str_replace_all(qdc_dist$data, "/", "-")


qdc_dist$data <- lubridate::as_date(qdc_dist$data, format = '%d-%m-%Y')

qdc_dist%>%
  dplyr::filter(data >= "2024-01-01")-> qdc_dist


fig_qdcdist <- plotly::plot_ly(qdc_dist,
                               x = ~data,
                               y = ~comgas,
                               name = 'Comgás', type = 'scatter', mode = 'none', stackgroup = 'one')



fig_qdcdist <- fig_qdcdist %>% add_trace (y = ~CEG, name = "CEG")
fig_qdcdist <- fig_qdcdist %>% add_trace (y = ~scgas, name = "SC Gás")
fig_qdcdist <- fig_qdcdist %>% add_trace (y = ~gasmig, name = "Gasmig")
fig_qdcdist <- fig_qdcdist %>% add_trace (y = ~CEG.RIO, name = "CEG Rio")
fig_qdcdist <- fig_qdcdist %>% add_trace (y = ~esgas, name = "ES Gás")
fig_qdcdist <- fig_qdcdist %>% add_trace (y = ~bahiagas, name = "Bahiagás")
fig_qdcdist <- fig_qdcdist %>% add_trace (y = ~copergas, name = "CoperGás")
fig_qdcdist <- fig_qdcdist %>% add_trace (y = ~sulgas, name = "Sulgás")
fig_qdcdist <- fig_qdcdist %>% add_trace (y = ~naturgy, name = "Naturgy SP")
fig_qdcdist <- fig_qdcdist %>% add_trace (y = ~compagas, name = "Compagás")
fig_qdcdist <- fig_qdcdist %>% add_trace (y = ~cegas, name = "Cegás")
fig_qdcdist <- fig_qdcdist %>% add_trace (y = ~necta, name = "Necta")
fig_qdcdist <- fig_qdcdist %>% add_trace (y = ~msgas, name = "MS Gás")
fig_qdcdist <- fig_qdcdist %>% add_trace (y = ~sergas, name = "Sergás")
fig_qdcdist <- fig_qdcdist %>% add_trace (y = ~pbgas, name = "PBGás")
fig_qdcdist <- fig_qdcdist %>% add_trace (y = ~algas, name = "Algás")
fig_qdcdist <- fig_qdcdist %>% add_trace (y = ~potigas, name = "Potigás")


fig_qdcdist <- fig_qdcdist %>%  plotly::layout(title = 'QDC contratada por Distribuidora',
                                               xaxis = list(title = 'Ano'),
                                               yaxis = list (title = 'Volume em m³/dia'),
                                               legend=list(title=list(text='<b> Distribuidora </b>'))) %>%
  layout(autosize = TRUE)

fig_qdcdist
htmlwidgets::saveWidget(as_widget(fig_qdcdist), "fig_qdcdist.html")

#------------------------------------------------------------------------------#
# Oferta Nacional#

oferta_nacional <- read.csv2("graph_files/oferta_nacional_mensal.csv")
oferta_nacional$data <- stringr::str_replace_all(oferta_nacional$data, "/", "-")
oferta_nacional$data <- lubridate::as_date(oferta_nacional$data, format = '%d-%m-%Y')
fig_ofertanac <- plotly::plot_ly(oferta_nacional,
                                 x = ~data,
                                 y = ~producao,
                                 name = 'Produção', type = 'scatter', mode = 'none', stackgroup = 'one')
fig_ofertanac <- fig_ofertanac %>% add_trace (y = ~reinjecao, name = "Reinjeção")
fig_ofertanac <- fig_ofertanac %>% add_trace (y = ~queima_perda, name = "Queimas e Perdas")
fig_ofertanac <- fig_ofertanac %>% add_trace (y = ~consumo_EeP, name = "Consumo E & P")
fig_ofertanac <- fig_ofertanac %>% add_trace (y = ~absorcao_upgn, name = "Absorção nas UPGN's")
fig_ofertanac <- fig_ofertanac %>% add_trace (y = ~oferta_liquida, name = "Oferta Líquida")
fig_ofertanac <- fig_ofertanac %>%  plotly::layout(title = 'Oferta Nacional',
                                                   xaxis = list(title = 'Ano'),
                                                   yaxis = list (title = 'Volume em m³/dia'),
                                                   legend=list(title=list(text='<b> Fontes</b>'))) %>%
  #layout(autosize = TRUE) 
  layout(margin = list(l = 50, r = 50, b = 50, t = 50, pad = 10))
fig_ofertanac
htmlwidgets::saveWidget(as_widget(fig_ofertanac), "fig_ofertanac.html")

#------------------------------------------------------------------------------#
# Oferta Importada#

oferta_imp <- read.csv2("graph_files/oferta_importada_mensal.csv")

oferta_imp$data <- stringr::str_replace_all(oferta_imp$data, "/", "-")


oferta_imp$data <- lubridate::as_date(oferta_imp$data, format = '%d-%m-%Y')

fig_ofertaimp<- plotly::plot_ly(oferta_imp,
                                x = ~data,
                                y = ~bolivia,
                                name = 'Boliviana', type = 'scatter', mode = 'none', stackgroup = 'one')

fig_ofertaimp <- fig_ofertaimp %>% add_trace (y = ~argentina, name = "Argentina")
fig_ofertaimp <- fig_ofertaimp %>% add_trace (y = ~regaseificacao_gnl, name = "Regaseificação (GNL)")

fig_ofertaimp <- fig_ofertaimp %>%  plotly::layout(title = 'Oferta Importada',
                                                   xaxis = list(title = 'Ano'),
                                                   yaxis = list (title = 'Volume em m³/dia'),
                                                   legend=list(title=list(text='<b> Fontes</b>'))) %>%
  layout(margin = list(l = 50, r = 50, b = 50, t = 50, pad = 10))
fig_ofertaimp 
htmlwidgets::saveWidget(as_widget(fig_ofertaimp), "fig_ofertaimp.html")

#------------------------------------------------------------------------------#
# Demanda por Segmento#

demanda_seg <- read.csv2("graph_files/demanda_segmento.csv")


demanda_seg$data <- stringr::str_replace_all(demanda_seg$data, "/", "-")


demanda_seg$data <- lubridate::as_date(demanda_seg$data, format = '%d-%m-%Y')

fig_demandaseg<- plotly::plot_ly(demanda_seg,
                                 x = ~data,
                                 y = ~industrial,
                                 name = 'Industrial', type = 'scatter', mode = 'none', stackgroup = 'one')

fig_demandaseg <- fig_demandaseg %>% add_trace (y = ~automotivo, name = "Automotivo")
fig_demandaseg <- fig_demandaseg %>% add_trace (y = ~ger_ele, name = "Geração Elétrica")
fig_demandaseg <- fig_demandaseg %>% add_trace (y = ~cogeracao, name = "Cogeração")
fig_demandaseg <- fig_demandaseg %>% add_trace (y = ~residencial, name = "Residencial")
fig_demandaseg <- fig_demandaseg %>% add_trace (y = ~comercial, name = "Comercial")
fig_demandaseg <- fig_demandaseg %>% add_trace (y = ~outros_mais_GNC, name = "Outros (incluindo GNC)")
fig_demandaseg <- fig_demandaseg %>% add_trace (y = ~consumo_gasbol, name = "Consumo GASBOL")
fig_demandaseg <- fig_demandaseg %>% add_trace (y = ~gasodutos_desequilibrio_perdas_ajustes
                                                , name = "Consumo de outros gasodutos, perdas, desequilíbrios e ajustes")


fig_demandaseg <- fig_demandaseg %>%  plotly::layout(title = 'Demanda',
                                                     xaxis = list(title = 'Ano'),
                                                     yaxis = list (title = 'Volume em m³/dia'),
                                                     legend=list(title=list(text='<b> Segmentos </b>'))) %>%
                                                    layout(margin = list(l = 50, r = 50, b = 50, t = 50, pad = 10))
fig_demandaseg

htmlwidgets::saveWidget(as_widget(fig_demandaseg), "fig_demandaseg.html")

#------------------------------------------------------------------------------#
#Demanda por distribuidora (com termeletricas)#

demanda_dist <- read.csv2("graph_files/demanda_distribuidora_com_termica.csv")
demanda_dist%>%dplyr::rename(data = X) -> demanda_dist

demanda_dist$data <- stringr::str_replace_all(demanda_dist$data, "/", "-")


demanda_dist$data <- lubridate::as_date(demanda_dist$data, format = '%d-%m-%Y')

fig_demandist<- plotly::plot_ly(demanda_dist,
                                x = ~data,
                                y = ~comgas,
                                name = 'Comgás', type = 'scatter', mode = 'none', stackgroup = 'one')

fig_demandist<- fig_demandist %>% add_trace (y = ~ceg, name = "CEG")
fig_demandist<- fig_demandist %>% add_trace (y = ~ceg_rio, name = "CEG Rio")
fig_demandist<- fig_demandist %>% add_trace (y = ~bahiagas, name = "BahiaGás")
fig_demandist<- fig_demandist %>% add_trace (y = ~copergas, name = "Copergás")
fig_demandist<- fig_demandist %>% add_trace (y = ~cigas, name = "Cigás")
fig_demandist<- fig_demandist %>% add_trace (y = ~gasmig, name = "Gasmig")
fig_demandist<- fig_demandist %>% add_trace (y = ~esgas, name = "ES Gás")
fig_demandist<- fig_demandist %>% add_trace (y = ~gasmar, name = "Gasmar")
fig_demandist<- fig_demandist %>% add_trace (y = ~sulgas, name = "Sul Gás")
fig_demandist<- fig_demandist %>% add_trace (y = ~scgas, name = "SC Gás")
fig_demandist<- fig_demandist %>% add_trace (y = ~compagas, name = "Compagas")
fig_demandist<- fig_demandist %>% add_trace (y = ~msgas, name = "MS Gás")
fig_demandist<- fig_demandist %>% add_trace (y = ~cegas, name = "Cegás")
fig_demandist<- fig_demandist %>% add_trace (y = ~naturgy_sp, name = "Naturgy SP")
fig_demandist<- fig_demandist %>% add_trace (y = ~gas_brasiliano, name = "Gas Brasiliano")
fig_demandist<- fig_demandist %>% add_trace (y = ~algas, name = "Algás")
fig_demandist<- fig_demandist %>% add_trace (y = ~potigas, name = "Potigás")
fig_demandist<- fig_demandist %>% add_trace (y = ~pbgas, name = "PB Gás")
fig_demandist<- fig_demandist %>% add_trace (y = ~cebgas, name = "Cebgás")
fig_demandist<- fig_demandist %>% add_trace (y = ~mtgas, name = "MT Gás")
fig_demandist<- fig_demandist %>% add_trace (y = ~goiasgas, name = "Goiás Gás")
fig_demandist<- fig_demandist %>% add_trace (y = ~gaspisa, name = "Gaspisa")

fig_demandist <- fig_demandist %>%  plotly::layout(title = 'Demanda (Com termelétricas)',
                                                   xaxis = list(title = 'Ano'),
                                                   yaxis = list (title = 'Volume em m³/dia'),
                                                   legend=list(title=list(text='<b> Distribuidoras </b>'))) %>%
  layout(margin = list(l = 50, r = 50, b = 50, t = 50, pad = 10))


fig_demandist

htmlwidgets::saveWidget(as_widget(fig_demandist), "fig_demandist.html")

#------------------------------------------------------------------------------#
#Demanda por distribuidora (sem termeletricas)#

demanda_dist_st <- read.csv2("graph_files/demanda_distribuidora_sem_termica.csv")


demanda_dist_st$data <- stringr::str_replace_all(demanda_dist_st$data, "/", "-")


demanda_dist_st$data <- lubridate::as_date(demanda_dist_st$data, format = '%d-%m-%Y')

fig_demandist_st<- plotly::plot_ly(demanda_dist_st,
                                   x = ~data,
                                   y = ~comgas,
                                   name = 'Comgás', type = 'scatter', mode = 'none', stackgroup = 'one')

fig_demandist_st<- fig_demandist_st %>% add_trace (y = ~ceg, name = "CEG")
fig_demandist_st<- fig_demandist_st %>% add_trace (y = ~ceg_rio, name = "CEG Rio")
fig_demandist_st<- fig_demandist_st %>% add_trace (y = ~bahiagas, name = "BahiaGás")
fig_demandist_st<- fig_demandist_st %>% add_trace (y = ~copergas, name = "Copergás")
fig_demandist_st<- fig_demandist_st %>% add_trace (y = ~cigas, name = "Cigás")
fig_demandist_st<- fig_demandist_st %>% add_trace (y = ~gasmig, name = "Gasmig")
fig_demandist_st<- fig_demandist_st %>% add_trace (y = ~esgas, name = "ES Gás")
fig_demandist_st<- fig_demandist_st %>% add_trace (y = ~gasmar, name = "Gasmar")
fig_demandist_st<- fig_demandist_st %>% add_trace (y = ~sulgas, name = "Sul Gás")
fig_demandist_st<- fig_demandist_st %>% add_trace (y = ~scgas, name = "SC Gás")
fig_demandist_st<- fig_demandist_st %>% add_trace (y = ~compagas, name = "Compagas")
fig_demandist_st<- fig_demandist_st %>% add_trace (y = ~msgas, name = "MS Gás")
fig_demandist_st<- fig_demandist_st %>% add_trace (y = ~cegas, name = "Cegás")
fig_demandist_st<- fig_demandist_st %>% add_trace (y = ~naturgy_sp, name = "Naturgy SP")
fig_demandist_st<- fig_demandist_st %>% add_trace (y = ~gas_brasiliano, name = "Gas Brasiliano")
fig_demandist_st<- fig_demandist_st %>% add_trace (y = ~algas, name = "Algás")
fig_demandist_st<- fig_demandist_st %>% add_trace (y = ~potigas, name = "Potigás")
fig_demandist_st<- fig_demandist_st %>% add_trace (y = ~pbgas, name = "PB Gás")
fig_demandist_st<- fig_demandist_st %>% add_trace (y = ~cebgas, name = "Cebgás")
fig_demandist_st<- fig_demandist_st %>% add_trace (y = ~mtgas, name = "MT Gás")
fig_demandist_st<- fig_demandist_st %>% add_trace (y = ~goiasgas, name = "Goiás Gás")
fig_demandist_st<- fig_demandist_st %>% add_trace (y = ~gaspisa, name = "Gaspisa")

fig_demandist_st <- fig_demandist_st %>%  plotly::layout(title = 'Demanda (Com termelétricas)',
                                                         xaxis = list(title = 'Ano'),
                                                         yaxis = list (title = 'Volume em m³/dia'),
                                                         legend=list(title=list(text='<b> Distribuidoras </b>'))) %>%
                                                        layout(margin = list(l = 50, r = 50, b = 50, t = 50, pad = 10))
fig_demandist_st
htmlwidgets::saveWidget(as_widget(fig_demandist_st), "fig_demandist_st.html")

#------------------------------------------------------------------------------#
#Mapa das distribuidoras


areas_concedidas <- sf::read_sf('areas_concedidas/areas_concedidas.shp')

rename(areas_concedidas, Distribuidora = Dstrbdr) -> areas_concedidas


mapa_concessao <- plotly::ggplotly(
  ggplot(areas_concedidas) +
    geom_sf(aes(fill = Distribuidora))
  )%>% style(hoveron = 'fill')


mapa_concessao

htmlwidgets::saveWidget(as_widget(mapa_concessao), "mapa_concessao.html")



mun <- geobr::read_municipality(code_muni = "all",
                                year = 2010,
                                simplified = TRUE,
                                showProgress = TRUE)

distribuidoras <- read.csv2("graph_files/municipios_concedidos.csv")

distribuidoras$distribuidora <-replace(distribuidoras$distribuidora, distribuidoras$distribuidora == "", NA)

mun <- dplyr::left_join(mun,
                        distribuidoras,
                        by = "code_muni")

mun %>% dplyr::rename(Distribuidora = distribuidora) -> mun

mapa_dist <- plotly::ggplotly(
  ggplot(mun, aes(text = paste("Município:",name_muni))) +
    geom_sf(aes(fill = Distribuidora))
)

mapa_dist






