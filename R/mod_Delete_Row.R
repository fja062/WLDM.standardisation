#' Delete_Row UI Function
#'
#' @description A shiny Module creating the red box "delete a row in the table".
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Delete_Row_ui <- function(id) {
  ns <- NS(id)
  tagList(useShinyjs(),

          fluidRow(div(
            id = ns("delete_row"),
            box(
              width = 12,
              status = "danger",
              title = "Supprimer",
              collapsible = T,
              solidHeader = T,
              fluidRow(
                column(
                  8,
                  align = "center",
                  offset = 2,
                  selectInput(ns("which_row"), label = "Ligne à supprimer", choices = c("")),
                  tags$style(type = "text/css", "#which_row { text-align:center; display: block;}")
                )
              ),


              fluidRow(
                div(style = "display:inline-block;width:100%;text-align: center;",
                    actionButton(
                      ns("delete_btn"), "Supprimer", icon = icon("minus-circle")
                    ))
              )
            )
          )))
}


#' Delete_Row Server Function
#'
#' @noRd
mod_Delete_Row_server <-
  function(input, output, session, r, val, r2) {
    ns <- session$ns

    #Show the 'delete row' button when there is at least one row in in the table xtable
    toggle("delete_row", condition = !is.null(r$untidy_table))

    x <-
      r$untidy_table[, val] #For example in the import page, val = 1 (1rst column of the table = Column name)


    #If the list xvalue is empty, then there is nothing to select in the delete box
    if (is.null(x))
      x <- character(0)

    #If there are rows to delete, then the select input is given the list
    updateSelectInput(session,
                      "which_row",
                      choices = x,
                      selected = "")



    observe({
      r2$btn <- input$delete_btn
      r2$todelete <- input$which_row
    })



    return(r2) #return the info whether the delete button has been clicked, and which row is selected

  }
