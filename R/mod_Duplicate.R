#' Duplicate UI Function
#'
#' @description A shiny Module to duplicate columns.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Duplicate_ui <- function(id) {
  ns <- NS(id)
  tagList(
    useShinyjs(),

    fluidRow(style = "display:inline-block;width:100%;text-align: center;",
             actionButton(
               ns("launch_dupli"),
               "Duplicate column(s)",
               icon = icon("plus-circle")
             ))



  )

}


#' Duplicate Server Function
#'
#' @noRd
mod_Duplicate_server <- function(input, output, session, df, r) {
  ns <- session$ns

  #Show the 'delete row' button when there is at least one row in in the table xtable
  toggle("launch_dupli", condition = !is.null(df$user_table))

  observe({
    r$btn <- input$launch_dupli

  })

  return(r) #return the info whether the delete button has been clicked, and which row is selected

}
