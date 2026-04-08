#' bolsas_overview UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_analyses_prouni_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(style = "overflow-x: auto;",
        fluidRow(
          box(
            title = "Distribuição por Raça/Cor", status = "primary", solidHeader = TRUE,
            collapsible = TRUE, width = 6,
            plotlyOutput(ns("bolsas_por_raca"))
          ),
          box(
            title = "Distribuição por Modalidade", status = "primary", solidHeader = TRUE,
            collapsible = TRUE, width = 6,
            plotlyOutput(ns("bolsas_por_modalidade"))
          )
        )
    )
  )
}

#' bolsas_overview Server Functions
#'
#' @noRd
mod_analyses_prouni_server <- function(id, dados_filtrados, filtros_selecionados){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    #plots in plotly
    output$bolsas_por_raca <- plotly::renderPlotly({
      df <- dados_filtrados()
      req(nrow(df) > 0)

      raca_counts <- df %>%
        count(RACA_BENEFICIARIO, sort = TRUE) %>%
        na.omit()

      p <- ggplot(raca_counts, aes(x = reorder(RACA_BENEFICIARIO, n), y = n, fill = RACA_BENEFICIARIO)) +
        geom_col(width = 1.0, show.legend = FALSE) +
        geom_text(aes(label = format(n, big.mark = ".")),
                  hjust = -0.1, color = "white", size = 4)+
        coord_flip() +
        labs(title = "Distribuição por Raça/Cor\ndo Beneficiário",
             x = "", y = "Número de Bolsas") +
        theme_minimal(base_size = 14) +
        theme(
          plot.title = element_text(hjust = 0.5, size = 12, color = "white"),
          axis.title = element_text(color = "white"),
          axis.text.y = element_text(color = "white"),
          axis.text.x = element_text(color = "white"),
          plot.background = element_rect(fill = "transparent", color = NA),
          panel.background = element_rect(fill = "transparent", color = NA),
          panel.grid.major = element_line(color = "#555555"),
          panel.grid.minor = element_blank()
        )


      plotly::ggplotly(p) %>%
        plotly::layout(
          template = "plotly_dark",
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)",
          legend = list(font = list(color = "white"))
        )
    })

    output$bolsas_por_modalidade <- plotly::renderPlotly({
      df <- dados_filtrados()
      req(nrow(df) > 0)

      modalidade_counts <- df %>%
        count(MODALIDADE_ENSINO_BOLSA) %>%
        na.omit()

      p <- ggplot(modalidade_counts, aes(x = MODALIDADE_ENSINO_BOLSA, y = n, fill = MODALIDADE_ENSINO_BOLSA)) +
        geom_col(show.legend = FALSE) +
        geom_text(aes(label = format(n, big.mark = ".")),
                  vjust = -0.5, color = "white", size = 4) +
        labs(title = "Distribuição por Modalidade\nde Ensino",
             x = "Modalidade", y = "Número de Bolsas") +
        theme_minimal(base_size = 14) +
        theme(
          plot.title = element_text(hjust = 0.5, size = 12, color = "white"),
          axis.title = element_text(color = "white"),
          axis.text = element_text(color = "white"),
          plot.background = element_rect(fill = "transparent", color = NA),
          panel.background = element_rect(fill = "transparent", color = NA),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(color = "#555555"),
          panel.grid.minor = element_blank()
        )


      plotly::ggplotly(p) %>%
        plotly::layout(
          template = "plotly_dark",
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)",
          legend = list(font = list(color = "white"))
        )
    })

  })
}


