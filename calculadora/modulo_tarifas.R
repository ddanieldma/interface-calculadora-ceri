# URL da planilha
sheet_url = "https://docs.google.com/spreadsheets/d/1f0IC0tKz4_0O0PTsqqv4_lLc-jDEiFT5Rpx-uALiReM/edit?usp=sharing"


# Valores padrão de consumo para cada segmento.
CONSUMO_PADRAO_COMERCIAL <- 800
CONSUMO_PADRAO_INDUSTRIAL <- as.integer(600000)
CONSUMO_PADRAO_RESIDENCIAL <- 12

# Paleta de cores dos gráficos.
paleta_grafico <- c("#AFAEB4", "#0C63AA", "#73BFE8", "#002D4D", "#5C5B5F")

# ==============================================================================
# Função que adquire os dados do Google Sheets.
get_dados_tarifas <- function(valor_classe, valor_nivel){
  message("Buscando dados das abas de tarifas")
  
  # String da classe de consumo com minúsculas.
  valor_classe <- tolower(valor_classe)
  
  # Alterando string para respectivo valor de classe de consumo.
  # Vetor que funciona como dicionário.
  map_classes <- c(
    "residencial" = 1,
    "industrial" = 2,
    "comercial" = 3
  )
  
  # Convertendo classe recebida na função.
  valor_classe <- map_classes[valor_classe]
  
  # Atualizando células de input.
  # Input da classe de consumo.
  sheet_url %>% range_write(data = data.frame(valor_classe),
                            sheet = 2,
                            range = "D1",
                            col_names = FALSE)
  # Input da classe do nível de consumo mensal.
  sheet_url %>% range_write(data = data.frame(valor_nivel),
                            sheet = 2,
                            range = "E6",
                            col_names = FALSE)
  
  # Lendo retorno da calculadora.
  df <- read_sheet(sheet_url,
                   sheet = 2,
                   range = "B9:E27",
                   col_names = c("Distribuidora", "Tarifa", "Estado", "Regiao"))
  
  # Transformando nome das regiões de abreviação para o nome real.
  map_regioes <- c(
    "SE" = "Sudeste",
    "N" = "Norte",
    "NE" = "Nordeste",
    "S" = "Sul",
    "CO" = "Centrooeste"
  )
  
  df <- df %>%
    # Utilizando dicionário para mudar os nomes das regiões na coluna.
    mutate(Regiao = recode(Regiao, !!!map_regioes)) %>% 
    # Retirando coluna de estado.
    subset(select = -Estado) %>% 
    # Renomeando coluna de tarifa.
    rename("Tarifa\n(em R$/m³)" = Tarifa)
  
  # Adicionando coluna com estado da distribuidora
  df$Estado <- gsub(".*\\((.*)\\).*", "\\1", df$Distribuidora)
  
  return(df)
}

# ==============================================================================
# Funções auxiliares do servidor.

# ===================================================
# Filtra dados pra cada região.
filtra_dados_regiao <- function(dados_tarifas, regiao) {
  reactive({
    dados_tarifas() %>%
      filter(Regiao == regiao) %>%
      subset(select = -Regiao)
  })
}

# ====================================================
# Cria tabela com dados para cada região e estilização
cria_tabela_div <- function(nome_regiao, id_tabela, ns) {
  tags$div(
    h4(nome_regiao),
    tableOutput(ns(id_tabela)),
    class = "table-div"
  )
}

# ====================================================
# Configura para que as tabelas sejam criadas com a estilização e com os dados corretamente.
cria_output_tabela <- function(regiao, ns, dados_tarifas, output) {
  # Renderiza UI da tabela
  output[[paste0("tabela_", tolower(regiao))]] <- renderUI({
    tableOutput(ns(paste0("tabela_", tolower(regiao))))
  })
  
  # Renderiza tabelas com dados filtrados por região
  output[[paste0("tabela_", tolower(regiao))]] <- renderTable({
    filtra_dados_regiao(dados_tarifas, regiao)()
  }, bordered = TRUE, striped = TRUE, hover = TRUE)
}

# ====================================================
# Configura o output de todas as tabelas de uma vez.
cria_output_tabelas <- function(ns, dados_tarifas, output) {
  cria_output_tabela("Norte", ns, dados_tarifas, output)
  cria_output_tabela("Nordeste", ns, dados_tarifas, output)
  cria_output_tabela("Sudeste", ns, dados_tarifas, output)
  cria_output_tabela("Sul", ns, dados_tarifas, output)
  cria_output_tabela("Centrooeste", ns, dados_tarifas, output)
}

# ====================================================
# Cria o gráfico da aba de tarifas.
cria_grafico_tarifas <- function (df, x_axis) {
  df <- df %>% 
    rename(Tarifa = "Tarifa\n(em R$/m³)")
  
  x_column <- switch(x_axis,
                     "Distribuidora" = "Distribuidora",
                     "Estado" = "Estado",
                     "Região" = "Regiao")
  
  
  if(x_axis == 'Estado'){
    # Agrupando dados por estado
    df_media <- df %>%
      group_by(Estado) %>%
      summarise(Tarifa = mean(Tarifa, na.rm = TRUE))
    
    # Adicionando coluna região de volta
    df_media <- df %>%
      select(Estado, Regiao) %>%
      distinct() %>%  # Ensure only unique Estado-Region pairs
      left_join(df_media, by = "Estado")
  
    # Definindo estilização do gráfico
    x_axis_title <- "Estado"
    y_axis_title <- "Tarifa média (em R$/m³)"
    title_text <- "Tarifa média por estado <br><sup>em R$/m³</sup>"
  }
  else if(x_axis == 'Região'){
    df_media <- df %>%
      group_by(Regiao) %>%
      summarise(Tarifa = mean(Tarifa, na.rm = TRUE))
    
    x_axis_title <- "Região"
    y_axis_title <- "Tarifa média (em R$/m³)"
    title_text <- "Tarifa média por região <br><sup>em R$/m³</sup>"
  }
  else {
    # For "Distribuidora" option
    df_media <- df
    
    x_axis_title <- "Distribiudora"
    y_axis_title <- "Tarifa (em R$/m³)"
    title_text <- "Tarifa por distribuidora <br><sup>em R$/m³</sup>"
  }
  
  print("df_media")
  print(df_media)
  
  fig <- plot_ly() %>% 
    add_bars(
      data = df_media, 
      x = ~get(x_column), 
      y = ~Tarifa, 
      color = ~Regiao, 
      colors = paleta_grafico,
      marker = list(
        line = list(color = "white", width = 1)  # Adds a border to simulate rounded bars
      ),
      text = ~round(Tarifa, 2),  # Display values with two decimal places
      textposition = 'outside'  # Position text above the bars
    ) %>% 
    layout(
      title = list(
        # text = "Tarifa por distribuidora <br><sup>em R$/m³</sup>",
        text = title_text,
        font = list(family = "Arial", size = 16, color = "#333")
      ),
      xaxis = list(
        title = list(text = x_axis_title, standoff = 25),
        categoryorder = "total descending",
        tickfont = list(family = "Arial", size = 12),
        tickangle = -45  # Angle the labels for better readability
      ),
      yaxis = list(
        title = y_axis_title,
        tickfont = list(family = "Arial", size = 12),
        range = c(0, max(df$Tarifa) * 1.1)  # Slightly increase the y-axis range
      ),
      margin = list(t = 50, b = 150),  # Adjust margins for better spacing
      showlegend = TRUE,  # Keep the legend to clarify regions
      legend = list(
        title = list(text = "Região"),
        orientation = "v",  # Keep the legend vertical
        xanchor = "right",
        x = 1.1,  # Move the legend to the upper-right corner
        y = 1
      )
    )

  return(fig)
}

# Gráfico dois apenas com valores de média
cria_grafico_tarifas_2 <- function (df) {
    df <- df %>%
      rename(Tarifa = "Tarifa\n(em R$/m³)")
  
  # Calcular a média das tarifas por região
  df_media <- df %>%
    group_by(Regiao) %>%
    summarise(Media_Tarifa = mean(Tarifa, na.rm = TRUE))
  
  # Criar o gráfico de barras
  fig <- plot_ly(
    data = df_media,
    x = ~Regiao,
    y = ~Media_Tarifa,
    type = "bar",
    color = ~Regiao,
    colors = paleta_grafico,
    text = ~round(Media_Tarifa, 2),  # Mostrar valores arredondados
    textposition = 'outside'  # Exibir os valores acima das barras
  ) %>%
    layout(
      title = "Tarifa Média por Região",
      xaxis = list(
        title = "Região",
        tickfont = list(family = "Arial", size = 12)
      ),
      yaxis = list(
        title = "Tarifa média (em R$/m³)",
        tickfont = list(family = "Arial", size = 12),
        range = c(0, max(df_media$Media_Tarifa) * 1.1)  # Ajustar o range do eixo Y
      ),
      margin = list(t = 50, b = 100),  # Ajustar margens
      showlegend = FALSE  # Ocultar a legenda, já que as regiões estão nos rótulos
    )
  
  return(fig)
}

# Função que remove colunas duplicadas de Faixa Inicial e Faixa Final se existirem
remove_duplicate_faixas <- function(df) {
  # Verificando se estão realmente duplicadas as colunas
  faixa_inicial_duplicated <- sum(grepl("Faixa_inicial", names(df))) > 1
  faixa_final_duplicated <- sum(grepl("Faixa_final", names(df))) > 1
  
  print("faixa_inicial_duplicated")
  print(faixa_inicial_duplicated)
  print("faixa_final_duplicated")
  print(faixa_final_duplicated)
  
  # Se estiverem
  if (faixa_inicial_duplicated & faixa_final_duplicated) {
    # Removendo as 8 primeiras colunas, inúteis
    columns_to_remove <- 0:8
    df <- df[, -columns_to_remove]
    
    # E adicionando coluna categoria consumo novamente
    novos_nomes_colunas <- c("Categoria_consumo", colnames(df)[-1])
    colnames(df) <- novos_nomes_colunas
  }
  
  return (df)
}

# ====================================================
# Renderizando UI do botão quando os dados estiverem disponíveis.
renderiza_botao_download <- function(input, output, ns, dados){
  output$download_button_ui <- renderUI({
    # Garantindo que os dados estão disponíveis antes de renderizar o botão.
    req(dados())
    downloadButton(ns("download_dados"), "Baixar Dados (CSV)")
  })
}


# ==============================================================================
# Código reutilizável para servidor das abas de comparação de tarifas.
comparacao_server <- function(id, segmento, consumo_padrao) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- NS(id)
      
      # Buscando valores de tarifas na planilha de acordo com a aba em questão
      dados_tarifas <- reactive({
        message("Buscando dados")
        get_dados_tarifas(segmento, consumo_padrao)
      })
      
      # Criando gráfico das tarifas.
      output$grafico_tarifas <- renderPlotly({
        message("Renderizando gráfico")
        
        df <- req(dados_tarifas())
        
        fig <- cria_grafico_tarifas(df)
        fig
      })
      
      # Criação do handler para o download dos dados.
      output$download_dados <- downloadHandler(
        # nivel_consumo <- req(input$nivel_consumo_tarifas)
        
        filename = function() {
          paste("dados_tarifas_gas_consumo-", consumo_padrao, "m3_segmento-", segmento, "_", Sys.Date(), ".csv", sep = "")
        },
        content = function(file) {
          write.csv(dados_tarifas(), file, row.names = FALSE)
        }
      )
      
      # Criação das tabelas de tarifas das distribuidoras por região
      observe({
        output$tabela_ui_tarifas <- renderUI({
          message("Criando tabelas")
          tagList(
            tags$div(
              cria_tabela_div("Norte", "tabela_norte", ns),
              cria_tabela_div("Nordeste", "tabela_nordeste", ns),
              tags$div(
                h4("Sudeste"),
                tableOutput(ns("tabela_sudeste")),
                h5("(Todas as tarifas estão em R$/m³)"),
                class = "table-div"
              ),
              cria_tabela_div("Sul", "tabela_sul", ns),
              cria_tabela_div("Centro-oeste", "tabela_centrooeste", ns),
              class = "tables-div"
            )
          )
        })
      })
      
      cria_output_tabelas(ns, dados_tarifas, output)
    }
  )
}


# ==============================================================================
# Código reutilizável para UI's das abas de comparação de tarifas.
comparacao_ui <- function(id, segmento, consumo_padrao_segmento) {
  ns <- NS(id)
  
  tabPanel(
    "",
    fluidPage(
      add_busy_spinner(),
      tagList(
        titlePanel(paste("Tarifas para o consumo médio do setor", segmento, " no mês atual")),
        h4(paste("Valor padrão para o setor ", segmento, ": ", consumo_padrao_segmento, "m³", sep="")),
        
        fluidRow(
          column(12,
                 downloadButton(ns("download_dados"), "Baixar Dados (CSV)"),
                 plotlyOutput(ns('grafico_tarifas'))
          )
        ),
        fluidRow(
          column(12,
                 # withSpinner(uiOutput(ns('tabela_ui_tarifas')), type = 8)
                 uiOutput(ns('tabela_ui_tarifas'))
          )
        )
      )
    )
  )
}