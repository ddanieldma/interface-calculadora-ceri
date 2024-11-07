# Servidor da aba de estrutura tarifária.

library(shiny)
library(googlesheets4)
library(jsonlite)

# Importando módulo com funções auxiliares da calculadora.
source("calculadora/modulo_tarifas.R")

# URL da planilha
sheet_url = "https://docs.google.com/spreadsheets/d/1f0IC0tKz4_0O0PTsqqv4_lLc-jDEiFT5Rpx-uALiReM/edit?usp=sharing"

#===============================================================================
# Função de obtenção dos dados.
obter_dados_estrutura <- function(nome_distribuidora) {
  message("Buscando dados")
  # Lendo aba da distribuidora recebida.
  df <- range_read(sheet_url, sheet = nome_distribuidora)
  
  # Definindo primeira linha do df inicial como novas colunas.
  novos_nomes_colunas <- as.character(df[1, ])
  
  # Adicionando nome para coluna com categoria da tarifa e retirando coluna com nome vazio.
  novos_nomes_colunas <- c("Categoria_consumo", novos_nomes_colunas[-1])
  colnames(df) <- novos_nomes_colunas
  
  # Retirando primeira linha do df.
  df <- df[-1, ]
  
  # Retirando linhas que não são dados.
  indice_fim <- which(df$Categoria_consumo == "Dados acabam aqui")
  df <- df[1:(indice_fim - 2), ]
  
  # Limpando nomes das coluna retirando o 'list(\"...\")'.
  nomes_limpos <- gsub("list\\(\"(.*?)\"\\)", "\\1", names(df))
  
  # Retirando \ e tudo pra frente dele nos nomes.
  nomes_limpos <- gsub("\\\\.*", "", nomes_limpos)
  
  # Colocando nomes das colunas limpos nas colunas do dataframe.
  names(df) <- nomes_limpos
  
  # Renomeando colunas que vamos usar, com exceção da coluna de Parte Fixa que ainda
  # não temos qual a correta.
  colnames(df)[colnames(df) == "Tarifa com tributos (R$/m³)"] <- 'Tarifa_tributos'
  colnames(df)[colnames(df) == "Faixa Inicial"] <- 'Faixa_inicial'
  colnames(df)[colnames(df) == "Faixa Final"] <- 'Faixa_final'
  
  # Selecionando colunas que já queremos e uma coluna depois da coluna Tarifa_tributos,
  # que é a coluna de Parte fixa que queremos.
  
  df <- df %>%
    select(one_of("Categoria_consumo", "Faixa_inicial", "Faixa_final", "Tarifa_tributos"),
           which(names(df) == 'Tarifa_tributos') + 1)
  
  # Agora que temos todas as colunas que queremos, transformamos o nome da coluna de 
  # parte fixa.
  novos_nomes_colunas <- c(colnames(df)[1:(length(colnames(df)) - 1)], "Parte_fixa")
  names(df) <- novos_nomes_colunas
  
  colunas_converter <- c("Faixa_inicial", "Faixa_final")
  df[colunas_converter] <- lapply(df[colunas_converter], function(col) sapply(col, function(x) as.integer(x[[1]])))
  df[colunas_converter] <- lapply(df[colunas_converter], as.integer)
  colunas_converter <- c("Tarifa_tributos", "Parte_fixa")
  df[colunas_converter] <- lapply(df[colunas_converter], function(col) sapply(col, function(x) as.numeric(x[[1]])))
  df[colunas_converter] <- lapply(df[colunas_converter], as.numeric)
  
  # Ajustando as faixas.
  # Se tivermos as duas faixas como NA (caso em que faixa inicial é faixa única e
  # faixa final está vazio), colocamos faixa única. Se tivermos a faixa final como
  # NA ou um número maior que 1000000, colocamos o -, que indica o "fim" da faixa.
  # As duas operações em sequência nessa ordem garantem que ficará do jeito necessário.
  df <- df %>%
    mutate(
      Faixa_inicial = ifelse(is.na(Faixa_inicial) & is.na(Faixa_final), "Faixa única", Faixa_inicial),
      Faixa_final = ifelse(is.na(Faixa_final) | Faixa_final > 10000000, "-", as.character(Faixa_final))
    )
  
  # Renomando colunas para nomes mais legíveis.
  novos_nomes_colunas <- c("Faixa inicial", "Faixa final", "Tarifa com tributos (R$/m³)", "Parte fixa")
  names(df)[2:length(names(df))] <- novos_nomes_colunas
  
  
  return(df)
}

#===============================================================================
# Servidor
estrutura_tarifaria_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- NS(id)
    
    # ====================================================
    # Mensagem inicial
    message("===================================================================")
    message("Servidor da aba de estrutura tarifária \n")
    
    # ====================================================
    # Valor reativo para armazenar o dataframe.
    df_estrutura_tarifaria <- reactiveVal(NULL)
    
    # ====================================================
    # Barra de carregamento.
    
    # Botão pressionado.
    observeEvent(input$atualizar_estrutura, {
      # Garante que temos o nome da distribuidora.
      req(input$nome_distribuidora_estrutura)

      # Mostra barra de progresso para melhorar UX.
      withProgress(message = "Carregando dados", value = 0, {
        df <- obter_dados_estrutura(input$nome_distribuidora_estrutura)
        df_estrutura_tarifaria(df)

        # Incrementa toda a barra.
        incProgress(1)
      })
    })
    
    # ====================================================
    # Donwload dos dados
    renderiza_botao_download(input, output, ns, df_estrutura_tarifaria)
    
    # Criando o handler para o download dos dados.
    output$download_dados <- downloadHandler(
      filename = function() {
        paste("dados-estrutura-tarifaria-gas_distribuidora-", input$nome_distribuidora_estrutura, Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        write.csv(df_estrutura_tarifaria(), file, row.names = FALSE)
      }
    )
    
    
    # ====================================================
    # Tabelas
    # Renderizando tabelas.
    output$tabelas <- renderUI({
      message("Renderizando tabelas")
      
      req(df_estrutura_tarifaria())
      
      categorias <- unique(df_estrutura_tarifaria()$Categoria_consumo)
      
      # Cria uma lista de tabelas para cada categoria.
      lista_tabelas <- lapply(categorias, function(categoria) {
        subset_dados <- df_estrutura_tarifaria() %>%
          filter(Categoria_consumo == categoria) %>%
          select(-Categoria_consumo)
        
        tagList(
          div(class = "flex-item",
              h3(categoria),
              tableOutput(ns(paste0("tabela_", categoria)))
          )
        )
      })
      
      # Colocando as tabelas dentro do container flex
      div(class = "flex-container", do.call(tagList, lista_tabelas))
    })
    
    # Configurando os outputs das tabelas.
    observe({
      message("Criando tabelas")
      # Garantindo que a função funcionou e temos o df.
      req(df_estrutura_tarifaria())

      # Obtendo as categorias de consumo.
      categorias <- unique(df_estrutura_tarifaria()$Categoria_consumo)

      # Criando tabelas, uma pra cada categoria de consumo.
      lapply(categorias, function(categoria) {
        # Fazendo segmentação dos dados por categoria.
        subset_dados <- df_estrutura_tarifaria() %>%
          filter(Categoria_consumo == categoria) %>%
          # Removendo coluna de categoria.
          select(-Categoria_consumo) %>%
          # Removendo linhas onde todos os dados são NA.
          select(where(~ !all(is.na(.))))

        # Criando tabela para aquela categoria
        output[[paste0("tabela_", categoria)]] <- renderTable({
          subset_dados
        })
      })
    })
    
    # ====================================================
    # Input
    
    # Atualizando o menu dropdown com os nomes das abas (distribuidoras) da planilha.
    observe({
      message("Atualizando seletor com os nomes das abas")
      # Garantindo que a planilha está disponível.
      req(sheet_url)
      
      nomes_abas <- sheet_names(sheet_url)
      # Retirando os primeiros nomes, que não são abas de distribuidoras.
      nomes_abas <- nomes_abas[4:length(nomes_abas)]
      
      updateSelectInput(session, "nome_distribuidora_estrutura", choices = nomes_abas)
    })
  })
}