#' Fill_Event UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd

mod_Fill_Event_ui <- function(id){
  ns <- NS(id)
  EmptyBox <- data.frame()

  tagList(
    h1("Event levels"),
    fluidRow(box(width = 12, status = "primary", collapsible = T,
                 tags$div( HTML('<i class="fas fa-info-circle"></i>'),
                           HTML('<i>  <b> Create the Event levels with selected columns and add complementary information. </b>
             <br> Within each Event level, select one or more column names, then click on <b> Select fieldname </b> and <b> validate </b> the Event level. Navigate through the panels to fill additionnal fields.
              <br> The result is displayed in the <b> Events levels content </b> box. Masurements field names are not shown in this table (dedicated to the Event core). They appear in the panel "MEASUREMENT/FACTS". </i>')))
    ),
    box(width = 12, title = "Events levels content", status = "info", solidHeader = TRUE,

        column(3, shiny::dataTableOutput(ns("tbl_event_fields")))),


    mod_Fill_Event_Box_ui(ns("Fill_Event_Box_ui_1"), EmptyBox),

    div(style="display:inline-block;width:100%;text-align: center;",
        actionButton(ns("add_field"), "Add an Event level", icon = icon("plus-circle")),
        actionButton(ns("delete_field"), "Delete the Event level", icon = icon("minus-circle"))
    ),


  )

}

#' Fill_Event Server Function

mod_Fill_Event_server <- function(input, output, session, dtf_structure,
                                  dtf_import){
  ns <- session$ns

  # ----------------------------------------------------------------------------
  #   Variables
  # ----------------------------------------------------------------------------
  #

  # number of ui elements already created
  uiCount = reactiveVal(0)

  moduleOuts = reactiveValues()

  Events <- reactiveValues(fields = NULL, parameters = NULL)

  # In this observe(), the first box is built and filled.
  observe({

    if (!is.null(dtf_structure$structure_table)) {

      tab_event <- filter(dtf_structure$structure_table, type %in% c("Event", "Event Mof"))
    } else {

      tab_event <- data.frame()
    }


    moduleOuts[[as.character(1)]] = callModule(mod_Fill_Event_Box_server, "Fill_Event_Box_ui_1", tab_event,
                                               dtf_import, 1)

  })



  # When the button "Dupliquer" is clicked, then add a new box
  observeEvent(input$add_field,{
    if (!is.null(dtf_structure$structure_table)) {
      tab_event <- filter(dtf_structure$structure_table, type %in% c("Event", "Event Mof"))

      if (nrow(tab_event)>0) {

        tab_event <- filter(tab_event, !fieldname %in% Events$fields[["fieldname"]])
      }


    } else {
      tab_event <- data.frame()
    }

    # count the UI elements created to assign new IDs
    uiCount(uiCount()+1)

    # insert module UI before the add/remove buttons
    insertUI(sprintf("#%s",session$ns("add_field")),
             'beforeBegin',
             mod_Fill_Event_Box_ui(session$ns(paste0("Fill_Event_Box_ui_1",uiCount())), NULL),
             session = session)

    # Get the output
    #
    moduleOuts[[as.character(uiCount()+1)]] = callModule(mod_Fill_Event_Box_server,id = paste0("Fill_Event_Box_ui_1",uiCount()), tab_event,
                                                         dtf_import, as.character(uiCount()+1))


  })


  observe({toggle("delete_field", condition = uiCount()>0)})
  # When the button "Supprimer" is clicked, then delete the last box
  observeEvent(input$delete_field,{

    removeUI(
      selector = sprintf("#%s",session$ns(paste0("Fill_Event_Box_ui_1",uiCount())))

    )
    moduleOuts[[as.character(uiCount()+1)]] <- NULL
    uiCount(uiCount()-1)

  })



  observe({

    ## Add a new column called 'level', which inform on the box number
    if(!is.null(moduleOuts)){

      name_box <- names(reactiveValuesToList(moduleOuts))
      moduleOuts1 <- mapply(function(x, y) { if(!is.null(x$Field_list)) {cbind(x$Field_list,
                                                                               level = rep(y, dim(x$Field_list)[1])) }},
                            reactiveValuesToList(moduleOuts),
                            name_box, SIMPLIFY = F)

      # fields contain the fieldnames and the associated levels
      # parameters the additionnal info (temporal, spatial, misc)

      Events$fields <- do.call(rbind, moduleOuts1)
      Events$parameters <- moduleOuts



    }

  })



  output$tbl_event_fields <- DT::renderDataTable(DT::datatable({
    data.frame(Events$fields)},
    options = list(
      paging = FALSE,
      autoWidth = TRUE,
      scrollY = TRUE,
      dom = 't'
    ),
    rownames = FALSE,
    escape = F))


  return(Events)

}

