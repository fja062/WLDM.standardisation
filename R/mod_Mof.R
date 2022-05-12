
mod_Mof_ui <- function(id, PreFilled.box){
  ns <- NS(id)


  tagList(
    tags$br(), # Add vertical spacing

    column(width = 12,
           tags$div( HTML('<i class="fas fa-info-circle"></i>'),
                     HTML('The columns previously identified as Measurements are shown in the box <b> Pre-filled measurements </b>.
             <br> <b> First level measurements </b> are measurements whose parent is either an Event or an Occurrence level.
             <br> <b> Measurements of measurements </b> are measurements whose parent is a Measurement previously validated.'),
                     tags$br(),
                     tags$br())),

    box(width = 12, status = "primary", title = "Pre-filled measurements",  solidHeader = TRUE,

        column(width = 5,
               DT::dataTableOutput(outputId = ns("df_mof")))),

    box(width = 12, status = "primary", title = "First level measurements",  solidHeader = TRUE,
        column(width = 12,
               tags$div( HTML('Enter a <b> measurement type </b> (compulsory) and a <b> measurement unit </b> (optionnal).
             <br>  As <b> measurement value </b> (compulsory), enter a free value or choose a column name previously identified as a Measurement.
             <br> Click on <b> Add a measurement/fact </b> to validate.'),
                         tags$br(),
                         tags$br())),
        column(2,
               textInput(
                 ns("measurementType"),
                 "Measurement type",
                 NULL)),

        column(3,
              div(
                fluidRow(textInput(
                  ns("measurementValue"),
                  "Measurement value",
                  NULL)),
              fluidRow(HTML("or")),
              fluidRow(uiOutput(ns("create_df"))),


              )),
        column(1,
               HTML("")),
        column(3,
               div(

                 fluidRow(textInput(
                   ns("measurementUnit"),
                   "Measurement unit",
                   NULL)),
                 fluidRow(HTML("or")),
                 fluidRow(uiOutput(ns("create_df_2"))),


               )),
        column(1,
               HTML("")),


        actionButton(ns("add_mof"), "Add a measurement/fact", icon = icon("plus-circle"))

               #mod_Add_Mof_ui(ns("Add_Mof_ui")) # Print the red box 'delete' option

       ),

    tags$br(),
    tags$br(),
    box(width = 12, status = "primary", title = "Measurements preview",  solidHeader = TRUE,
        column(width = 8,
               DT::dataTableOutput(outputId = ns("mof_output"))),
        column(width = 2, align = "center", offset = 1,
               # Print the red box 'delete' option
               actionButton(ns("delete_mof"), "Delete", icon = icon("minus-circle")))),

    box(width = 12, status = "primary", title = "Measurements of measurements",  solidHeader = TRUE,
        column(width = 12,
               tags$div( HTML('Chose the name of the measurement parent. Enter a <b> measurement type </b> (compulsory) and a <b> measurement unit </b> (optionnal).
             <br>  As <b> measurement value </b> (compulsory), enter a free value or choose a column name previously identified as a Measurement.
             <br> Click on <b> Add a measurement/fact </b> to validate.'),
                         tags$br(),
                         tags$br())),
        column(3,
        uiOutput(ns("mof_of_mof"))),
        column(2,
               textInput(
                 ns("measurementType_1"),
                 "Measurement type",
                 NULL)),

        column(3,

               div(
                 fluidRow(textInput(
                   ns("measurementValue_1"),
                   "Measurement value",
                   NULL)),
                 fluidRow(HTML("or")),
                 fluidRow(uiOutput(ns("create_df_bis"))),


               )),

        column(3, textInput(
                   ns("measurementUnit_1"),
                   "Measurement unit",
                   NULL)),
        column(1,
               HTML("")),


        actionButton(ns("add_mof_mof"), "Add a measurement/fact", icon = icon("plus-circle"))),
    box(width = 12, status = "primary", title = "Measurements of measurements preview",  solidHeader = TRUE,

        column(width = 8,
               DT::dataTableOutput(outputId = ns("mof_of_mof_df"))),
        column(width = 2, align = "center", offset = 1,
               # Print the red box 'delete' option
               actionButton(ns("delete_mof_mof"), "Delete", icon = icon("minus-circle")))
   ),
   fluidRow(
     style="display:inline-block;width:100%;text-align: center;",
     actionButton(ns("valid_mof"), "Validate", icon = icon("readyButton shadow")))


    )


}


mod_Mof_server <- function(input, output, session, dtf_mof,
                                dtf_import){
  ns <- session$ns

  output$df_mof <- DT::renderDataTable(DT::datatable({
    dtf_mof$dtf_mof
  },
  options = list(
    paging = FALSE,
    autoWidth = TRUE,
    scrollY = TRUE,
    dom = 't'
  ),
  rownames = FALSE,
  escape = F


  ))


  # ----------------------------------------------------------------------------
  #   Create a data.frame with MOF
  # ----------------------------------------------------------------------------


  # Selection of data.frame column names
  output$create_df <- renderUI({

    # If missing input, return to avoid error later in function
    if(is.null(dtf_mof$dtf_mof)) {
      fill_list <- NULL
    } else {
      fill_list <- c("", dtf_mof$dtf_mof[['fieldname']])

    }

    # Selection list of old colnames
    selectInput(ns("mof_col"), "Measurement value (fieldname)", choices= fill_list,
                selected = "",  multiple = TRUE, size = 3, selectize= FALSE)


  })

  # Selection of data.frame column names
  output$create_df_bis <- renderUI({

    # If missing input, return to avoid error later in function
    if(is.null(dtf_mof$dtf_mof)) {
      fill_list <- NULL
    } else {
      fill_list <- c("", dtf_mof$dtf_mof[['fieldname']])

    }

    # Selection list of old colnames
    selectInput(ns("mof_col_bis"), "Measurement value (fieldname)", choices= fill_list,
                selected = "",  multiple = TRUE, size = 3, selectize= FALSE)


  })

  # Selection of data.frame column names
  output$create_df_2 <- renderUI({

    # If missing input, return to avoid error later in function
    if(is.null(dtf_mof$dtf_mof)) {
      fill_list <- NULL
    } else {
      fill_list <- c("", dtf_mof$dtf_mof[['fieldname']])

    }

    # Selection list of old colnames
    selectInput(ns("mof_col_2"), "Measurement unit (fieldname)", choices= fill_list,
                selected = "",
                multiple = TRUE, size = 3, selectize= FALSE)


  })

  # Initialize
  res_tidy <- reactiveValues(
    lets_tidy = reactiveValues()
  )

  # Create the red box showing the 'Tidy' option
  observe({
    res_tidy$lets_tidy <- callModule(mod_Add_Mof_server, "Add_Mof_ui", dtf_mof$dtf_mof, res_tidy$lets_tidy)

  })


  #Initialize the object 'results_analyse' as a reactive object
  mof_output_df <- reactiveValues(mof_output_dtf = NULL)

  # Create or updtade the datafram each time
  # the user click on 'Ajouter l'analyse'.
  observeEvent(input$add_mof, {

    mval <- input$measurementValue
    munit <- input$measurementUnit
    mtype <- input$measurementType
    from <- "value"
    if (input$mof_col != "") {
      mval <- input$mof_col
      from <- "column" }

    if (input$mof_col_2 != "") {munit <- input$mof_col_2}

    if (mval == "" | mtype == "") {
      showNotification("Fill the measurementValue and measurementType.",
                       duration = 6)
    } else {
    if(is.null(mof_output_df$mof_output_dtf) || nrow(mof_output_df$mof_output_dtf) == 0 ) {
      mof_output_df$mof_output_dtf <- data.frame( measurementType = mtype,
                                                 measurementValue = mval,
                                                 measurementUnit = munit,
                                                 From = from)

    }else{

      mof_output_df$mof_output_dtf<- rbind(mof_output_df$mof_output_dtf,
                                           data.frame(measurementType = mtype,
                                                      measurementValue = mval,
                                                      measurementUnit = munit,
                                                      From = from))

    }
  }
    })

#Show the 'delete row' button when there is at least one row in in the table xtable
  observe({toggle("delete_mof", condition = !is.null(mof_output_df$mof_output_dtf) && nrow(mof_output_df$mof_output_dtf) > 0)})

  observeEvent(input$delete_mof, {

    if(is.null(input$mof_output_rows_selected)) {
      showNotification("Select a row to delete.",
                       duration = 6)
    }else{

      mof_output_df$mof_output_dtf <- mof_output_df$mof_output_dtf[-input$mof_output_rows_selected,]

    }


  })

# Display data.frame
output$mof_output= renderDT({mof_output_df$mof_output_dtf}, rownames= FALSE,  options = list(dom = ''), filter = "none")


# ----------------------------------------------------------------------------
#   Create a data.frame with MOF of MOF
# ----------------------------------------------------------------------------
mof_mof_df <- reactiveValues(dtf = NULL)
# Selection of data.frame column names
output$mof_of_mof <- renderUI({

  # If missing input, return to avoid error later in function
  if(is.null(mof_output_df$mof_output_dtf)) {
    fill_list <- NULL
  } else {
    fill_list <- c("", mof_output_df$mof_output_dtf[['measurementType']])

  }

  if(!is.null(mof_mof_df$dtf)) {
      fill_list <- c(fill_list, mof_mof_df$dtf[['measurementType']])

  }
  # Selection list of old colnames
  selectInput(ns("mof_parent"), "Parent measurement name", choices= fill_list,
              selected = "",  multiple = FALSE, size = 3, selectize= FALSE)


})


# Create or updtade the datafram each time
# the user click on 'Ajouter l'analyse'.

observeEvent(input$add_mof_mof, {

  mof_parent <- input$mof_parent
  mval <- input$measurementValue_1
  munit <- input$measurementUnit_1
  mtype <- input$measurementType_1

  from <- "value"
  if (input$mof_col_bis != "") {
    mval <- input$mof_col_bis
    from <- "column" }

  if (mof_parent == "" | mval == "" | mtype == "") {
    showNotification("Fill the parent name, the measurement value and type.",
                     duration = 6)
  } else {
  if(is.null(mof_mof_df$dtf)) {

    mof_mof_df$dtf <- data.frame( measurementParent= mof_parent,
                                  measurementType = mtype,
                                                measurementValue = mval,
                                                measurementUnit = munit,
                                                From = from)

  }else{
    mof_mof_df$dtf<- rbind(mof_mof_df$dtf,
                                         data.frame(measurementParent= mof_parent,
                                           measurementType = mtype,
                                                    measurementValue = mval,
                                                    measurementUnit = munit,
                                                    From = from))
  }
  }
})

#Show the 'delete row' button when there is at least one row in in the table xtable
observe({toggle("delete_mof_mof", condition = !is.null(mof_mof_df$dtf) && nrow(mof_mof_df$dtf) > 0)})

observeEvent(input$delete_mof_mof, {

  if(is.null(input$mof_of_mof_df_rows_selected)) {
    showNotification("Select a row to delete.",
                     duration = 6)
  }else{

    mof_mof_df$dtf <- mof_mof_df$dtf[-input$mof_of_mof_df_rows_selected,]

  }


})

# Display data.frame
output$mof_of_mof_df= renderDT({mof_mof_df$dtf}, rownames= FALSE,  options = list(dom = ''), filter = "none")

# ----------------------------------------------------------------------------
#                     Construction dataframe "Terrain"
# ----------------------------------------------------------------------------

# Step 1 - Initialization: before any user input, the dataframes are all NULL.

MofFields <- reactiveValues(Mof = NULL, MofMof = NULL) # Will store all text and selected values


# Step 2 - The user add a new value on the interface:
# replace the corresponding value in the corresponding dataframe.
observe({

  MofFields$Mof <- mof_output_df$mof_output_dtf

  })

observeEvent(input$valid_mof, {
  MofFields$MofMof <- mof_mof_df$dtf


})
return(MofFields)
 }


#
