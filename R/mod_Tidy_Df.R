#' Delete_Row UI Function
#'
#' @description A shiny Module creating the red box "delete a row in the table".
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Tidy_Df_ui <- function(id){
  ns <- NS(id)
  tagList(

    useShinyjs(),

          fluidRow(
              style="display:inline-block;width:100%;text-align: center;",
              actionButton(ns("launch_tidy"), "Tidy", icon = icon("plus-circle")))



    )

}


#' Delete_Row Server Function
#'
#' @noRd
mod_Tidy_Df_server <- function(input, output, session, r, r1){
  ns <- session$ns

  #Show the 'delete row' button when there is at least one row in in the table xtable
  toggle("launch_tidy", condition = !is.null(r$untidy_table))

  observe({
    r1$btn <- input$launch_tidy

  })



  return(r1) #return the info whether the delete button has been clicked, and which row is selected

}


