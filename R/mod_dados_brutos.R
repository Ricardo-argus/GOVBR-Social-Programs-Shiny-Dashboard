#' mod_dados_brutos UI Function
#'
#' @description UI para a aba de Dados Completos (Tabela DT).
#' @param id O ID interno do Shiny para este módulo.
#' @import shiny
#' @import DT
#' @noRd
mod_dados_brutos_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      box(
        title = "Dados Consolidados dos Beneficiários", status = "primary", solidHeader = TRUE,
        width = 12,

        tabsetPanel(
          id = ns("tabset_dados"),

          # PROUNI
          tabPanel(
            title = "PROUNI",
            icon = icon("table"),

            fluidRow(
              column(4,
                     selectInput(ns("uf"), "Selecione a UF: ",
                                 choices = c("Todos","AC", "AL", "AP", "AM", "BA", "CE", "DF", "ES", "GO",
                                             "MA", "MT", "MS", "MG", "PA", "PB", "PR", "PE", "PI",
                                             "RJ", "RN", "RS", "RO", "RR", "SC", "SP", "SE", "TO"),
                                 selected = "Todos")
              ),
              column(4,
                     selectInput(ns("raca"), "Raça do Beneficiário: ",
                                 choices = c("Todas","Branca", "Parda", "Preta", "Amarela", "Nao Informada", "Indigena"),
                                 selected = "Todas")
              ),
              column(4,
                     selectInput(ns("ano"), "Ano da Bolsa: ",
                                 choices = c(2018,2019,2020),
                                 selected = "2020")
              )
            ),

            DT::dataTableOutput(ns("PROUNI")),

            fluidRow(
              column(4,
                  div(style = "max-height: 300px; overflow-y: auto;",
                     selectizeInput(ns("id_bolsista"), "Selecione o Id do Bolsista:", choices = NULL)
                  )
              ),
              column(4,
                  div(style = "max-height: 300px; overflow-y: auto;",
                     selectizeInput(ns("coluna_alvo"), "Coluna a ser alterada:", choices = NULL, multiple = FALSE,
                                    options = list(selectOnTab = TRUE, openOnFocus = TRUE, closeAfterSelect = TRUE))
                     )
              ),
              column(4,
                  div(style = "max-height: 300px; overflow-y: auto;",
                     selectizeInput(ns("valor_novo"), "Novo Valor para Coluna Selecionada:", choices = NULL, multiple = FALSE)
                  )
              )
            ),

            actionButton(ns("atualizar_valor"), "Atualizar Valores", icon = icon("sync"), class = "btn-primary"),

            tags$style(HTML("
              .selectize-dropdown-content {
                max-height: 300px;
                overflow-y: auto;
                background-color: white;
                color: black;
              }
            "))
          ),

          # BOLSA FAMÍLIA
          tabPanel(
            title = "BOLSA FAMÍLIA",
            icon = icon("table"),

            fluidRow(
              column(4,
                     selectInput(ns("estado"), "Selecione o Estado:",
                                 choices = c("Todos","AC", "AL", "AP", "AM", "BA", "CE", "DF", "ES", "GO",
                                             "MA", "MT", "MS", "MG", "PA", "PB", "PR", "PE", "PI",
                                             "RJ", "RN", "RS", "RO", "RR", "SC", "SP", "SE", "TO"),
                                 selected = "Todos")
              ),
              column(4,
                     selectInput(ns("ano_bf"), "Ano de concessão do benefício:",
                                 choices = c(2023,2024,2025),
                                 selected = "Todos")
              )
            ),

            DT::dataTableOutput(ns("bolsafamilia"))
          ),

          # LUZ PARA TODOS
          tabPanel(
            title = "Luz Para Todos",
            icon = icon("table"),

            fluidRow(
              column(4,
                     selectInput(ns("estado"), "Selecione o Estado:",
                                 choices = c("Todos","AC", "AL", "AP", "AM", "BA", "CE", "DF", "ES", "GO",
                                             "MA", "MT", "MS", "MG", "PA", "PB", "PR", "PE", "PI",
                                             "RJ", "RN", "RS", "RO", "RR", "SC", "SP", "SE", "TO"),
                                 selected = "Todos")
              ),
              column(4,
                     selectInput(ns("mes_atendimento"), "Selecione o mês do atendimento:",
                                 choices = c("Janeiro", "Fevereiro", "Março", "Abril", "Maio", "Junho",
                                             "Julho", "Agosto", "Setembro", "Outubro", "Novembro", "Dezembro"))
              )
            ),

            DT::dataTableOutput(ns("luzpt"))
          )
        )
      )
    )
  )
}
#' mod_dados_brutos Server Function
#'
#' @description Server para a aba de Dados Completos.
#' @param id O ID interno do Shiny para este módulo.
#' @param dados_filtrados Um reativo com os dados já filtrados.
#' @import shiny
#' @import DT
#' @import dplyr
#' @noRd
mod_dados_brutos_server <- function(id, dados_filtrados, dados_luz, dados_bf, con){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$PROUNI <- DT::renderDataTable({
      df <- dados_filtrados()
      req(nrow(df) > 0)

      df_filtrado <- df

      if(input$uf != "Todos"){
        df_filtrado <- df_filtrado %>% filter(UF_BENEFICIARIO == input$uf)
      }

      if(input$raca != "Todas"){
        df_filtrado <- df_filtrado %>% filter(RACA_BENEFICIARIO == input$raca)
      }


      DT::datatable(
        df_filtrado,
        filter = 'top',
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          language = list(url = '//cdn.datatables.net/plug-ins/1.10.25/i18n/Portuguese-Brasil.json')
        ),
        rownames = FALSE,
        class = "stripe hover"
      )
    })

    coluna_tabela_map <- list(
      id_bolsista            = "dados_bolsistas",
      ANO_CONCESSAO_BOLSA    = "dados_bolsistas",
      SEXO_BENEFICIARIO      = "dados_bolsistas",
      RACA_BENEFICIARIO      = "dados_bolsistas",
      UF_BENEFICIARIO        = "UF_bolsas",
      UNIVERSIDADE_BOLSA     = "bolsas_universidades",
      TIPO_BOLSA             = "tipo_bolsa",
      MODALIDADE_ENSINO_BOLSA= "modalidade_bolsa",
      TURNO_BOLSA            = "turno_bolsa",
      CURSO_BOLSISTA         = "curso_bolsista"
    )



    #atualiza tabela prouni
    observe({
      req(nrow(dados_filtrados()) > 0)
      ids <- dados_filtrados()$id_bolsista
      ids<- unique(na.omit(ids))
      updateSelectizeInput(session, "id_bolsista", choices = ids, server=TRUE)
    })

    observe({
      df <- dados_filtrados()
      req(nrow(df) > 0)
      updateSelectizeInput(session, "coluna_alvo", choices = names(coluna_tabela_map), server = TRUE)
    })

    observeEvent(input$coluna_alvo, {
      df <- dados_filtrados()
      req(input$coluna_alvo %in% names(df))
      valores <- unique(na.omit(df[[input$coluna_alvo]]))
      updateSelectizeInput(session, "valor_novo", choices = valores, server = TRUE)

    })


    observeEvent(input$atualizar_valor, {
      req(input$id_bolsista, input$coluna_alvo, input$valor_novo)

      tabela <- coluna_tabela_map[[input$coluna_alvo]]

      tabela_sql <- DBI::dbQuoteIdentifier(con,tabela)
      coluna_sql <- DBI::dbQuoteIdentifier(con, input$coluna_alvo)


          query <- paste0("UPDATE prouni_bolsas.", tabela_sql,
                          "SET ", coluna_sql, " = '", input$valor_novo,
                          "'WHERE id_bolsista = ", input$id_bolsista)

          DBI::dbExecute(con,query)

          showNotification(
            paste0("Tabela ",tabela," coluna ", input$coluna_alvo, " atualizada para ", input$valor_novo, " no bolsista ",input$id_bolsista,"!"),
            type = 'message')
    })


    output$luzpt <- DT::renderDataTable({
      df <- dados_luz()
      req(nrow(df) > 0)
      DT::datatable(
        df,
        filter = 'top',
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          language = list(url = '//cdn.datatables.net/plug-ins/1.10.25/i18n/Portuguese-Brasil.json')
        ),
        rownames = FALSE,
        class = "stripe hover"
      )
    })


    output$bolsafamilia <- DT::renderDataTable({
      df <- dados_bf()
      req(nrow(df) > 0)

      DT::datatable(
        df,
        filter = 'top',
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          language = list(url = '//cdn.datatables.net/plug-ins/1.10.25/i18n/Portuguese-Brasil.json')
        ),
        rownames = FALSE,
        class = "stripe hover"
      )
    })

  })
}
