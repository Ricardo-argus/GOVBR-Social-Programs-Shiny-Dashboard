#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_analyses_bolsafamilia_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(style = "overflow-x: auto;",
        fluidRow(
          box(
            title = "Top 5 Estados com mais Beneficiários do BPI", status = "primary", solidHeader = TRUE,
            collapsible = TRUE, width = 6,
            plotlyOutput(ns("bpi_topfive"))
          ),
          box(
            title = "10 Estados com mais Beneficiários do BVG", status = "primary", solidHeader = TRUE,
            collapsible = TRUE, width = 6,
            plotlyOutput(ns("bvg_topten"))
          )
        )
    )
  )
}

#' bolsafamilia_overview Server Functions
#'
#' @noRd
mod_analyses_bolsafamilia_server <- function(id, con){
  moduleServer(id, function(input, output, session){
    ns <- session$ns


    output$bpi_topfive <- plotly::renderPlotly({

      # Chama a função que retorna os dados já normalizados
      df <- fct_norm_bolsa_familia_values(con)
      req(nrow(df) > 0)

      # Monta o gráfico com ggplot
      p <- ggplot(df, aes(x = reorder(estado, percentual_bpi),
                          y = percentual_bpi,
                          fill = estado)) +
        geom_col(width = 0.6, show.legend = FALSE) +
        geom_text(aes(label = sprintf("%.2f%%", percentual_bpi)),
                  hjust = -0.1, color = "white", size = 4) +
        coord_flip() +
        scale_y_continuous(labels = scales::label_percent(scale = 1, accuracy = 0.1)) +
        labs(title = "Top 5 Estados com Maior Percentual\nBeneficiários Primeira Infância (BPI)",
             x = "Estado", y = "% da População Atendida") +
        theme_minimal(base_size = 14) +
        theme(
          plot.title = element_text(hjust = 0.5, size = 14, color = "white", face = "bold"),
          axis.title = element_text(color = "white"),
          axis.text.y = element_text(color = "white"),
          axis.text.x = element_text(color = "white"),
          plot.background = element_rect(fill = "transparent", color = NA),
          panel.background = element_rect(fill = "transparent", color = NA),
          panel.grid.major.y = element_line(color = "#555555"),
          panel.grid.major.x = element_line(color = "#333333"),
          panel.grid.minor = element_blank()
        )

      # Converte para plotly
      plotly::ggplotly(p) %>%
        plotly::layout(
          template = "plotly_dark",
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)",
          margin = list(l = 150, r = 50, t = 80, b = 60),
          showlegend = FALSE
        )
    })


    output$bvg_topten <- plotly::renderPlotly({
      query <- "
    SELECT top.estado, top.total_bvg FROM (
      SELECT SUM(bsf.qtd_ben_bvg) as total_bvg, bf.estado
      FROM bolsa_familia_ibge bf
      LEFT JOIN benef_somafamiliares bsf ON bsf.id_familias = bf.id_familias
      GROUP BY bf.estado
      ORDER BY total_bvg DESC
      LIMIT 10
    ) top;
  "

      df <- dbGetQuery(con, query)
      req(nrow(df) > 0)

      p <- ggplot(df, aes(x = reorder(estado, total_bvg),
                          y = total_bvg,
                          fill = estado)) +
        geom_col(width = 0.6, show.legend = FALSE) +
        geom_text(aes(label = format(total_bvg, big.mark = ".")),
                  hjust = -0.1, color = "white", size = 4) +
        coord_flip() +
        scale_y_continuous(labels = scales::label_comma(big.mark = ".", decimal.mark = ",")) +
        labs(title = "Top 10 Estados com Mais Beneficiários\nBenefício Variável Familiar (BVG)",
             x = "Estado", y = "Total de Familias Atendidas") +
        theme_minimal(base_size = 14) +
        theme(
          plot.title = element_text(hjust = 0.5, size = 14, color = "white", face = "bold"),
          axis.title = element_text(color = "white"),
          axis.text = element_text(color = "white"),
          legend.position = "bottom",
          legend.text = element_text(color = "white"),
          legend.title = element_text(color = "white"),
          plot.background = element_rect(fill = "transparent", color = NA),
          panel.background = element_rect(fill = "transparent", color = NA),
          panel.grid.major.y = element_line(color = "#555555"),
          panel.grid.major.x = element_line(color = "#333333"),
          panel.grid.minor = element_blank()
        )

      plotly::ggplotly(p) %>%  lm
      plotly::layout(
        template = "plotly_dark",
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)",
        margin = list(l = 150, r = 50, t = 80, b = 60),
        showlegend = FALSE
      )
    })


  })
}

