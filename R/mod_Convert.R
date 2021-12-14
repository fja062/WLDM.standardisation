#' Convert UI Function
#'
#' @description A shiny Module to convert column's values
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Convert_ui <- function(id) {
  ns <- NS(id)
  tagList(
    useShinyjs(),

    fluidRow(style = "display:inline-block;width:100%;text-align: center;",
             actionButton(
               ns("launch_convert"),
               "Convert column",
               icon = icon("plus-circle")
             ))



  )

}


#' Convert Server Function
#'
#' @noRd
mod_Convert_server <-
  function(input, output, session, convert_values, r) {
    ns <- session$ns

    #Show the 'delete row' button when there is at least one row in in the table xtable
    toggle("launch_convert",
           condition = !is.null(convert_values$convert_df))

    observe({
      r$btn <- input$launch_convert

    })

    return(r) #return the info whether the delete button has been clicked, and which row is selected

  }
