#' Add_Mof UI Function
#'
#' @description A shiny Module for adding a Measurement or Fact.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Add_Mof_ui <- function(id) {
  ns <- NS(id)
  tagList(
    useShinyjs(),

    fluidRow(
      style = "display:inline-block;width:100%;text-align: center;",
      actionButton(
        ns("add_mof"),
        "Add a measurement of fact",
        icon = icon("plus-circle")
      )
    )



  )

}


#' Add_Mof Server Function
#'
#' @noRd
mod_Add_Mof_server <- function(input, output, session, r, r1) {
  ns <- session$ns

  #Show the 'delete row' button when there is at least one row in in the table xtable
  toggle("add_mof", condition = !is.null(r))

  observe({
    r1$btn <- input$add_mof

  })



  return(r1) #return the info whether the delete button has been clicked, and which row is selected

}
