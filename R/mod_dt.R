mod_dt_ui <- function(id){ # UI module
  ns = NS(id)
  shiny::dataTableOutput(ns('x1'))
}


mod_dt_server <-  function(input, output, session, data){ # Server module

  output$x1 <- DT::renderDataTable(data, selection = 'none', editable = TRUE, server = TRUE)
  proxy <- dataTableProxy('x1')

  updatedData <- eventReactive(input$x1_cell_edit, {
    info = input$x1_cell_edit
    if (!is.null(info)) {
      str(info)
      data[info$row, info$col] <<- DT::coerceValue(info$value,
                                                   data[info$row, info$col])
    }
    data
  }, ignoreNULL = FALSE)

  return(updatedData)
}

