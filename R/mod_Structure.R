#' Strcuture UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Structure_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h1("Structure of the dataset (events/occurrence)"),
    fluidRow(box(
      width = 12,
      status = "primary",
      collapsible = T,
      tags$div(
        HTML('<i class="fas fa-info-circle"></i>'),
        HTML(
          '<i>  <b> Divide the columns between the Event, Occurrence, Event measurement and Occurrence measurement boxes. </b>
             <br> Within each box, select one or more column names, then click on </i>'
        ),
        HTML('<i class="fas fa-arrow-circle-right">.</i>'),
        HTML(
          '<i><br> The result is displayed in <b> Output </b>. When finished, click on <b> Validate structure </b>.
                     </i>'
        )
      )
    )),


    tags$br(),

    fluidRow(# Event box
      box(width = 16,

          # 2nd section
          fluidRow(
            box(
              title = "1. Event fieldnames",
              status = "warning",
              solidHeader = TRUE,


              #------- INPUT COLUMNS --------------

              column(8,


                     shiny::dataTableOutput(ns(
                       "darwinized_event"
                     )),),
              column(3,
                     div(
                       tags$br(),
                       # Add some space in the page
                       tags$br(),
                       tags$br(),


                       tags$br(),
                       # Add some space in the page

                       actionButton(
                         ns("add_event"),
                         "Add fieldname",
                         icon("arrow-circle-right"),
                         class = "readyButton shadow"
                       )
                     ))
            ),


            box(
              title = "2. Event measurement/facts",
              status = "info",
              solidHeader = TRUE,


              #------- INPUT COLUMNS --------------

              column(8,


                     shiny::dataTableOutput(ns(
                       "measurement_event"
                     )),),
              column(3,
                     div(
                       tags$br(),
                       # Add some space in the page
                       tags$br(),
                       tags$br(),


                       tags$br(),
                       # Add some space in the page

                       actionButton(
                         ns("add_mof_event"),
                         "Add fieldname",
                         icon("arrow-circle-right"),
                         class = "readyButton shadow"
                       )
                     ))
            )
          ))),
    # End event box


    fluidRow(# Occurrence box
      box(width = 16,

          fluidRow(
            box(
              title = "3. Occurrence fieldnames",
              status = "success",
              solidHeader = TRUE,


              #------- INPUT COLUMNS --------------

              column(8,


                     shiny::dataTableOutput(ns(
                       "darwinized_occurrence"
                     )),),
              column(3,
                     div(
                       tags$br(),
                       # Add some space in the page
                       tags$br(),
                       tags$br(),


                       tags$br(),
                       # Add some space in the page

                       actionButton(
                         ns("add_occ"),
                         "Add fieldname",
                         icon("arrow-circle-right"),
                         class = "readyButton shadow"
                       )
                     ))
            ),

            box(
              title = "4. Occurrence measurement/facts",
              status = "info",
              solidHeader = TRUE,


              #------- INPUT COLUMNS --------------

              column(8,


                     shiny::dataTableOutput(ns(
                       "measurement_occurrence"
                     )),),
              column(3,
                     div(
                       tags$br(),
                       # Add some space in the page
                       tags$br(),
                       tags$br(),


                       tags$br(),
                       # Add some space in the page

                       actionButton(
                         ns("add_mof_occ"),
                         "Add fieldname",
                         icon("arrow-circle-right"),
                         class = "readyButton shadow"
                       )
                     ))
            )

          ))),



    fluidRow(
      box(
        width = 16,
        title = "Output",
        collapsible = TRUE,
        collapsed = FALSE,
        column(6,
               shiny::dataTableOutput(ns(
                 "structure_table"
               ))),
        actionButton(ns("del_entry"), "Delete fieldname", icon = icon("plus-circle"))
      )
    ),
    tags$br(),
    tags$br(),
    fluidRow(style = "display:inline-block;width:100%;text-align: center;",
             actionButton(
               ns("valid_structure"),
               "Validate structure",
               icon = icon("readyButton shadow")
             ))
  )
}

#' Structure Server Function
#' @description A shiny Module.
#' @param id,input,output,session,dtf_darwin, PreFilled.dtf Internal parameters for {shiny}.
#' @noRd
mod_Structure_server <-
  function(input,
           output,
           session,
           dtf_darwin,
           PreFilled.dtf) {
    ns <- session$ns



    # ----------------------------------------------------------------------------
    #   Variables
    # ----------------------------------------------------------------------------

    # Initialize list of fieldnames

    list1 <- reactiveValues(l = NULL)
    observe({
      if (!is.null(dtf_darwin$darwin_table)) {
        list1$l = dtf_darwin$darwin_table[['fieldname']]
      }

    })

    df_from_darwin <- reactiveValues(dtf = NULL)
    observe({
      if (!is.null(dtf_darwin$darwin_table)) {
        df_from_darwin$dtf <- filter(dtf_darwin$darwin_table,
                                     fieldname %in% list1$l)
      }
    })

    df_combine <- reactiveValues(dtf_combine = NULL)


    #Show the 'add' button when there is at least one row selected
    observe({
      toggle("add_event", condition = !is.null(df_from_darwin$dtf))
    })
    observe({
      toggle("add_occ", condition = !is.null(df_from_darwin$dtf))
    })
    observe({
      toggle("add_mof_event", condition = !is.null(df_from_darwin$dtf))
    })
    observe({
      toggle("add_mof_occ", condition = !is.null(df_from_darwin$dtf))
    })


    # ----------------------------------------------------------------------------
    #  Update tables if add an entry in the event fields
    # ----------------------------------------------------------------------------
    observeEvent(input$add_event, {
      from <- input$darwinized_event_rows_selected
      fieldname_tmp <- pull(df_from_darwin$dtf[from, ], fieldname)
      standard_tmp <- pull(df_from_darwin$dtf[from,], standard)
      concept_tmp <- pull(df_from_darwin$dtf[from,], concept)

      list1$l <- list1$l[!list1$l %in% fieldname_tmp]

      if (is.null(df_combine$dtf_combine)) {
        df_combine$dtf_combine <- data.frame(fieldname = fieldname_tmp,
                                             standard = standard_tmp,
                                             concept = concept_tmp) %>%
          mutate(type = "Event")
      } else{
        df_combine$dtf_combine <- rbind(
          df_combine$dtf_combine,
          data.frame(
            fieldname = fieldname_tmp,
            standard = standard_tmp,
            concept = concept_tmp
          ) %>%
            mutate(type = "Event")
        )
      }

      df_from_darwin$dtf <-
        filter(dtf_darwin$darwin_table, fieldname %in% list1$l)
    })

    #

    #
    #
    # ----------------------------------------------------------------------------
    #   Update tables if add a manual column in the event table
    # ----------------------------------------------------------------------------

    observeEvent(input$add_manual_fieldname, {
      dtf_darwin$darwin_table <- rbind(
        dtf_darwin$darwin_table,
        data.frame(
          fieldname = input$ev_col,
          standard = input$ev_col_std,
          concept = NA
        )
      )
    })

    # ----------------------------------------------------------------------------
    #   Update tables if add an entry in the occurrence table
    # ----------------------------------------------------------------------------
    observeEvent(input$add_occ, {
      from <- input$darwinized_occurrence_rows_selected
      fieldname_tmp <- pull(df_from_darwin$dtf[from, ], fieldname)
      standard_tmp <- pull(df_from_darwin$dtf[from,], standard)
      concept_tmp <- pull(df_from_darwin$dtf[from,], concept)

      list1$l <- list1$l[!list1$l %in% fieldname_tmp]

      if (is.null(df_combine$dtf_combine)) {
        df_combine$dtf_combine <- data.frame(fieldname = fieldname_tmp,
                                             standard = standard_tmp,
                                             concept = concept_tmp) %>%
          mutate(type = "Occurrence")
      } else{
        df_combine$dtf_combine <- rbind(
          df_combine$dtf_combine,
          data.frame(
            fieldname = fieldname_tmp,
            standard = standard_tmp,
            concept = concept_tmp
          ) %>%
            mutate(type = "Occurrence")
        )
      }

      df_from_darwin$dtf <-
        filter(dtf_darwin$darwin_table, fieldname %in% list1$l)

    })

    # ----------------------------------------------------------------------------
    #   Event measurements
    # ----------------------------------------------------------------------------
    observeEvent(input$add_mof_event , {
      from <- input$measurement_event_rows_selected
      fieldname_tmp <- pull(df_from_darwin$dtf[from, ], fieldname)
      standard_tmp <- pull(df_from_darwin$dtf[from,], standard)
      concept_tmp <- pull(df_from_darwin$dtf[from,], concept)


      list1$l <- list1$l[!list1$l %in% fieldname_tmp]

      if (is.null(df_combine$dtf_combine)) {
        df_combine$dtf_combine <- data.frame(fieldname = fieldname_tmp,
                                             standard = standard_tmp,
                                             concept = concept_tmp) %>%
          mutate(type = "Event Mof")
      } else{
        df_combine$dtf_combine <- rbind(
          df_combine$dtf_combine,
          data.frame(
            fieldname = fieldname_tmp,
            standard = standard_tmp,
            concept = concept_tmp
          ) %>%
            mutate(type = "Event Mof")
        )
      }

      df_from_darwin$dtf <-
        filter(dtf_darwin$darwin_table, fieldname %in% list1$l)

    })





    # ----------------------------------------------------------------------------
    #   Occurrence measurements
    # ----------------------------------------------------------------------------
    occurrence_mof <- reactiveValues(occurrence_mof_table = NULL)
    observeEvent(input$add_mof_occ, {
      from <- input$measurement_occurrence_rows_selected
      fieldname_tmp <- pull(df_from_darwin$dtf[from, ], fieldname)
      standard_tmp <- pull(df_from_darwin$dtf[from,], standard)
      concept_tmp <- pull(df_from_darwin$dtf[from,], concept)


      list1$l <- list1$l[!list1$l %in% fieldname_tmp]

      if (is.null(df_combine$dtf_combine)) {
        df_combine$dtf_combine <- data.frame(fieldname = fieldname_tmp,
                                             standard = standard_tmp,
                                             concept = concept_tmp) %>%
          mutate(type = "Occurrence Mof")
      } else{
        df_combine$dtf_combine <- rbind(
          df_combine$dtf_combine,
          data.frame(
            fieldname = fieldname_tmp,
            standard = standard_tmp,
            concept = concept_tmp
          ) %>%
            mutate(type = "Occurrence Mof")
        )
      }

      df_from_darwin$dtf <-
        filter(dtf_darwin$darwin_table, fieldname %in% list1$l)

    })


    # ----------------------------------------------------------------------------
    #   Output dataframe
    # ----------------------------------------------------------------------------

    # List to select event fields
    output$darwinized_event <- DT::renderDataTable(DT::datatable({
      input$add_event
      input$add_occ
      input$del_entry
      input$add_mof_event
      input$add_mof_occ

      df_from_darwin$dtf

    },
    options = list(
      paging = FALSE,
      autoWidth = TRUE,
      scrollY = TRUE,
      dom = 't'
    ),
    rownames = FALSE,
    escape = F))

    # List to select occurrence fields
    output$darwinized_occurrence <-
      DT::renderDataTable(DT::datatable({
        input$add_event
        input$add_occ
        input$del_entry
        input$add_mof_event
        input$add_mof_occ

        df_from_darwin$dtf

      },
      options = list(
        paging = FALSE,
        autoWidth = TRUE,
        scrollY = TRUE,
        dom = 't'
      ),
      rownames = FALSE,
      escape = F))

    # List to select measurement event fields

    output$measurement_event <- DT::renderDataTable(DT::datatable({
      input$add_event
      input$add_occ
      input$del_entry
      input$add_mof_event
      input$add_mof_occ

      df_from_darwin$dtf

    },
    options = list(
      paging = FALSE,
      autoWidth = TRUE,
      scrollY = TRUE,
      dom = 't'
    ),
    rownames = FALSE,
    escape = F))


    # List to select measurement event fields

    output$measurement_occurrence <-
      DT::renderDataTable(DT::datatable({
        input$add_event
        input$add_occ
        input$del_entry
        input$add_mof_event
        input$add_mof_occ

        df_from_darwin$dtf

      },
      options = list(
        paging = FALSE,
        autoWidth = TRUE,
        scrollY = TRUE,
        dom = 't'
      ),
      rownames = FALSE,
      escape = F))



    #Show the 'delete row' button when there is at least one row in in the table xtable
    observe({
      toggle("del_entry", condition = !is.null(df_combine$dtf_combine))
    })

    observeEvent(input$del_entry, {
      if (is.null(input$structure_table_rows_selected)) {
        showNotification("Select a row to delete.",
                         duration = 6)
      } else{
        list1$l <-
          c(list1$l, df_combine$dtf_combine[input$structure_table_rows_selected, "fieldname"])
        df_combine$dtf_combine <-
          df_combine$dtf_combine[-input$structure_table_rows_selected, ]
        df_from_darwin$dtf <-
          filter(dtf_darwin$darwin_table, fieldname %in% list1$l)
      }


    })


    output$structure_table <- DT::renderDataTable(DT::datatable({
      df_combine$dtf_combine
    } ,
    options = list(
      paging = FALSE,
      autoWidth = TRUE,
      scrollY = TRUE,
      dom = 't'
    ),
    rownames = FALSE,
    escape = F))

    #----------------------------------------#
    #-------FINAL EXPORT  -----------------#
    #----------------------------------------#

    Structure_all <-
      reactiveValues(structure_table = NULL, move = NULL)
    observeEvent(input$valid_structure, {
      Structure_all$structure_table <- df_combine$dtf_combine
      Structure_all$move_structure <- input$valid_structure

    })

    return(Structure_all)

  }
