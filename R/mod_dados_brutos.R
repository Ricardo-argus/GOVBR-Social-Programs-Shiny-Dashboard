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
        title = "Dados Consolidados dos Bolsistas", status = "primary", solidHeader = TRUE,
        width = 12,
        DT::dataTableOutput(ns("tabela_completa"))
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
mod_dados_brutos_server <- function(id, dados_filtrados){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$tabela_completa <- DT::renderDataTable({
      df <- dados_filtrados()
      req(nrow(df) > 0)

      colunas_para_dropdown <- c(
        "SEXO_BENEFICIARIO",
        "RACA_BENEFICIARIO",
        "UF_BENEFICIARIO"
      )

      colunas_existentes <- intersect(colunas_para_dropdown, names(df))

      if (length(colunas_existentes) > 0) {
        df <- dplyr::mutate(
          df,
          dplyr::across(
            dplyr::all_of(colunas_existentes),
            as.factor
          )
        )
      }

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
