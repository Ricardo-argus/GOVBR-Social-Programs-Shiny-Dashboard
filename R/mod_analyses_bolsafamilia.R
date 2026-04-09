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
            title = "Top 5 Estados com mais Beneficiários do BPI (Percentual)", status = "primary", solidHeader = TRUE,
            collapsible = TRUE, width = 6,
            plotlyOutput(ns("bpi_topfive"))
          ),
          box(
            title = "10 Estados com mais Beneficiários do BVG (Percentual)", status = "primary", solidHeader = TRUE,
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
                  hjust = 1.1, color = "white", size = 4) +
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
          panel.grid.minor = element_blank(),
          clip = "off"
        )

      # Converte para plotly
      plotly::ggplotly(p) %>%
        plotly::layout(
          template = "plotly_dark",
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)",
          margin = list(l = 150, r = 250, t = 80, b = 60),
          showlegend = FALSE
        )
    })


    output$bvg_topten <- plotly::renderPlotly({

      df <- fct_norm_bolsa_familia_values_2(con)
      req(nrow(df) > 0)

      p <- ggplot(df, aes(x = reorder(estado, percentual_bvg),
                          y = percentual_bvg,
                          fill = estado)) +
        geom_col(width = 0.6, show.legend = FALSE) +
        geom_text(aes(label = sprintf("%.2f%%", percentual_bvg)),
                  hjust = -0.1, color = "white", size = 4) +
        coord_flip() +
        scale_y_continuous(labels = scales::label_comma(big.mark = ".", decimal.mark = ",")) +
        labs(title = "Top 10 Estados com Mais Beneficiários BVG Percentual",
             x = "Estado", y = "% Familias Atendidas (População x Beneficiarios)") +
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

      plotly::ggplotly(p) %>%
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

