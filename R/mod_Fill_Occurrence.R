
mod_Fill_Occurrence_ui <- function(id){
  ns <- NS(id)
  EmptyBox <- data.frame()

  tagList(
    h1("Occurrence levels"),
    fluidRow(box(width = 12, status = "primary", collapsible = T,
                 tags$div( HTML('<i class="fas fa-info-circle"></i>'),
                           HTML('<i>  <b> Create the Occurrence levels with selected columns and add complementary information. </b>
             <br> Within each Occurrence level, select one or more column names, then click on <b> Select fieldname </b> and <b> validate </b>. Navigate through the panels to fill additionnal fields.
              <br> The result is displayed in the <b> Occurrence levels content </b> box. Masurements field names are not shown in this table (dedicated to the Occurrences). They appear in the panel "MEASUREMENT/FACTS". </i>')))
    ),
    box(width = 12, title = "Occurrences levels fieldnames", status = "info", solidHeader = TRUE,

        column(3, DT::dataTableOutput(ns("tbl_occurrence_fields")))),

    div(style="display:inline-block;width:100%;text-align: left;",
        mod_Fill_Occurrence_Box_ui(ns("Fill_Occurrence_Box_ui_1"), EmptyBox)),

    div(style="display:inline-block;width:100%;text-align: center;",
        actionButton(ns("add_field"), "Add an Occurrence level", icon = icon("plus-circle")),
        actionButton(ns("delete_field"), "Delete the Occurrence level", icon = icon("minus-circle"))
    )


  )

}

mod_Fill_Occurrence_server <- function(input, output, session, dtf_structure,
                                       dtf_import){
  ns <- session$ns

  # ----------------------------------------------------------------------------
  #   Variables
  # ----------------------------------------------------------------------------
  #

  # number of ui elements already created
  uiCount = reactiveVal(0)

  moduleOuts = reactiveValues()

  Occurrences <- reactiveValues(fields = NULL, parameters = NULL)

  # In this observe(), the first box is built and filled.
  observe({

    if (!is.null(dtf_structure$structure_table)) {

      tab_occurrence <- filter(dtf_structure$structure_table, type %in% c("Occurrence", "Occurrence Mof"))


    } else {

      tab_occurrence <- data.frame()
    }


    moduleOuts[[as.character(1)]] = callModule(mod_Fill_Occurrence_Box_server, "Fill_Occurrence_Box_ui_1", tab_occurrence,
                                               dtf_import, 1)

  })



  # When the button "Dupliquer" is clicked, then add a new box
  observeEvent(input$add_field,{
    if (!is.null(dtf_structure$structure_table)) {
      tab_occurrence <- filter(dtf_structure$structure_table, type %in% c("Occurrence", "Occurrence Mof"))
      if (nrow(tab_occurrence)>0) {
        tab_occurrence <- filter(tab_occurrence, !fieldname %in% Occurrences$fields[["fieldname"]]) }
    } else {
      tab_occurrence <- data.frame()
    }

    # count the UI elements created to assign new IDs
    uiCount(uiCount()+1)

    # insert module UI before the add/remove buttons
    insertUI(sprintf("#%s",session$ns("add_field")),
             'beforeBegin',
             mod_Fill_Occurrence_Box_ui(session$ns(paste0("Fill_Occurrence_Box_ui_1",uiCount())), NULL),
             session = session)

    # Get the output
    #
    moduleOuts[[as.character(uiCount()+1)]] = callModule(mod_Fill_Occurrence_Box_server,id = paste0("Fill_Occurrence_Box_ui_1",uiCount()), tab_occurrence,
                                                         dtf_import, as.character(uiCount()+1))


  })



  # When the button "Supprimer" is clicked, then delete the last box
  observeEvent(input$delete_field,{

    removeUI(
      selector = sprintf("#%s",session$ns(paste0("Fill_Occurrence_Box_ui_1",uiCount())))

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
      Occurrences$fields <- do.call(rbind, moduleOuts1)
      Occurrences$parameters <- moduleOuts



    }

  })



  output$tbl_occurrence_fields <- DT::renderDataTable(DT::datatable({
    data.frame(Occurrences$fields)},
    options = list(
      paging = FALSE,
      autoWidth = TRUE,
      scrollY = TRUE,
      dom = 't'
    ),
    rownames = FALSE,
    escape = F))

  return(Occurrences)

}
