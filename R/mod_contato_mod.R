#' contato_mod UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_contato_mod_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(6,
            sliderInput("obs", "Number", min = 1, max = 1000, value = 500)),
      column(8,
             plotOutput("testplot"))
    )
  )
}

#' contato_mod Server Functions
#'
#' @noRd
mod_contato_mod_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    output$testplot <- renderPlot({
      hist(rnorm(input$obs))
    })

  }
  )
}

## To be copied in the UI
# mod_contato_mod_ui("contato_mod_1")

## To be copied in the server
# mod_contato_mod_server("contato_mod_1")
