#' Terrain_Box UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Fill_Occurrence_Box_ui <- function(id, PreFilled.box) {
  ns <- NS(id)
  tagList(
    # the module is enclosed within a div with it's own id to allow it's removal
    div(
      id = id,
      style = "display:inline-block;width:100%;text-align: left;",
      mainPanel(width = 12,
                tags$br(), # Add vertical spacing

                fluidRow(
                  box(
                    width = 12,
                    collapsible = T,
                    solidHeader = T,
                    status = "info",
                    title = textOutput(ns("title")),
                    tabsetPanel(
                      id = "occurrence_tabs",
                      #  height = "350px",
                      #  width = NULL,
                      selected = "FIELD SELECTION",

                      tabPanel(
                        "FIELD SELECTION",
                        tags$br(),
                        # Add vertical spacing
                        tags$br(),
                        # Add vertical spacing
                        column(width = 6,
                               DT::dataTableOutput(ns(
                                 "tbl_occurrence_in"
                               ))),
                        column(
                          1,
                          actionButton(
                            ns("insert_occurrence_field"),
                            "Add fieldname",
                            icon("arrow-circle-right"),
                            class = "readyButton shadow"
                          )
                        ),
                        column(width = 6,
                               DT::dataTableOutput(ns(
                                 "tbl_occurrence_out"
                               ))),
                        fluidRow(
                          style = "display:inline-block;width:100%;text-align: center;",
                          tags$br(),
                          tags$br(),
                          actionButton(
                            ns("all_valid"),
                            "Validate",
                            icon("arrow-circle-right"),
                            class = "readyButton shadow"
                          )

                        )
                      ),

                      tabPanel(
                        "TEMPORAL FIELDS",


                        tagList(
                          tags$br(),
                          # Add vertical spacing
                          column(width = 12,
                                 tags$div(
                                   HTML('<i class="fas fa-info-circle"></i>'),
                                   HTML(
                                     'The field names previously mapped with a temporal concept are shown in the box <b> Pre-filled standard </b>.
             <br> <b> Standards with controlled values </b> correspond to standard columns for which you will have to choose in a list of controlled values.
              <br> To generate the standard field <b> \"eventDate\" </b>, you have to indicate how the date will be extracted from your dataset in the box <b> \"Extraction of the date\" </b>.'
                                   ),
                                   tags$br(),
                                   tags$br()
                                 )),
                          box(
                            width = 12,
                            status = "primary",
                            title = "Pre-filled standards",
                            solidHeader = TRUE,


                            column(width = 5,
                                   DT::dataTableOutput(outputId = ns("temporal_out")))
                          ),

                          box(
                            width = 12,
                            status = "primary",
                            title = "Standards with controlled values",
                            solidHeader = TRUE,

                            selectInput(
                              ns("timeLevel"),
                              "Time level",
                              choices = c("", "day",
                                          "month",
                                          "year",
                                          "interval",
                                          "hunting season")
                            )
                          ),

                          box(
                            width = 12,
                            status = "primary",
                            title = "Extraction of date",
                            solidHeader = TRUE,
                            column(width = 12,
                                   tags$div(
                                     HTML('<i class="fas fa-info-circle"></i>'),
                                     HTML(
                                       '<b>Choice of the type of date extraction: </b>
                                                                <br> Select <b>"From a single column"</b> if your date (punctual or interval) is stored in one column, previously mapped with the standard "dataTime".
                                                                 <br> Select <b>"From two columns"</b> if your begin date and end date are stored in two columns, previously mapped with the standard "beginDate" and "endDate".
                                                                 <br> Select <b>"Fill the interval manually"</b> if you want to directly enter the time interval.'
                                     )
                                   )
                                   ,

                                   tags$br(),
                                   tags$br()),
                            selectInput(
                              ns("extract_date"),
                              "Type of date extraction",
                              choices = c(
                                "",
                                "From a single column (auto)" = "1_col_auto",
                                "From two columns" = "2_col_auto",
                                "Fill the interval manually" = "manu"
                              ),
                              selected = ""
                            ),
                            conditionalPanel(
                              condition = "input.extract_date == '1_col_auto'",
                              ns = NS(id),
                              textInput(ns("sep_interval"),
                                        "Separator (for intervals)",
                                        "")
                            ),
                            conditionalPanel(
                              condition = "input.extract_date == 'manu'",
                              ns = NS(id),
                              dateInput(ns("date_min_manual"),
                                        "Begin date:")
                            ),
                            conditionalPanel(
                              condition = "input.extract_date == 'manu'",
                              ns = NS(id),
                              dateInput(ns("date_max_manual"),
                                        "End date:")
                            ),

                            conditionalPanel(
                              condition = "input.extract_date == '1_col_manu'",
                              ns = NS(id),
                              DT::dataTableOutput(ns('ex_df'))
                            ),


                            conditionalPanel(
                              condition = "input.extract_date !== '1_col_manu' & input.extract_date !== 'manu' & input.extract_date !== ''",
                              ns = NS(id),
                              selectInput(
                                ns("format_date"),
                                "Date format",
                                choices = c(
                                  "",
                                  "d",
                                  "m",
                                  "y",
                                  "dm",
                                  "md",
                                  "my",
                                  "ym",
                                  "dmy",
                                  "mdy",
                                  "ymd",
                                  "ydm"
                                ),
                                selected = ""
                              )
                            )



                          ),

                          fluidRow(
                            style = "display:inline-block;width:100%;text-align: center;",
                            actionButton(
                              ns("valid_tempo"),
                              "Validation",
                              icon = icon("arrow-circle-right"),
                              class = "readyButton shadow"
                            )
                          )
                          ,
                          box(
                            width = 12,
                            title = "Output preview",
                            solidHeader = TRUE,
                            collapsible = TRUE,
                            collapsed = TRUE,
                            DTOutput(ns("tbl_temporal"))
                          )
                        )
                      ),

                      tabPanel("SPATIAL FIELDS",
                               tagList(
                                 fluidRow(
                                   tags$br(),
                                   # Add vertical spacing
                                   column(width = 12,
                                          tags$div(
                                            HTML('<i class="fas fa-info-circle"></i>'),
                                            HTML(
                                              'The field names previously mapped with a spatial concept are shown in the box <b> Pre-filled standard </b>.
             <br> <b> Standards with free values </b> correspond to standard columns for which you can enter a free value.
             <br> <b> Standards with controlled values </b> correspond to standard columns for which you will have to choose in a list of controlled values.'
                                            ),
                                            tags$br(),
                                            tags$br()
                                          )),
                                   tags$br(),
                                   box(
                                     width = 12,
                                     status = "primary",
                                     title = "Pre-filled standards",
                                     solidHeader = TRUE,
                                     column(width = 5,
                                            DT::dataTableOutput(outputId = ns("spatial_out")))
                                   ),

                                   box(
                                     width = 12,
                                     status = "primary",
                                     title = "Standards with free-text values values",
                                     solidHeader = TRUE,
                                     column(width = 12,
                                            splitLayout(
                                              uiOutput(ns("fill_free")),
                                              textInput(ns("value"), "Value", ""),
                                              actionButton(
                                                ns("add_free_spatial"),
                                                "Add standard value",
                                                icon = icon("plus-circle")
                                              )
                                            )),
                                     DTOutput(ns("tbl_free_spatial")) # Print the table

                                   ),
                                   box(
                                     width = 12,
                                     status = "primary",
                                     title = "Standards with controlled values",
                                     solidHeader = TRUE,
                                     column(
                                       width = 12,
                                       column(width = 4,
                                              selectInput(
                                                ns("countryCode"),
                                                "countryCode",
                                                choices = c(
                                                  "",
                                                  "AX",
                                                  "AL",
                                                  "AD",
                                                  "AT",
                                                  "BY",
                                                  "BE",
                                                  "BA",
                                                  "BG",
                                                  "CH",
                                                  "CZ",
                                                  "DE",
                                                  "DK",
                                                  "EE",
                                                  "ES",
                                                  "FO",
                                                  "FI",
                                                  "FR",
                                                  "GB",
                                                  "GI",
                                                  "GR",
                                                  "GG",
                                                  "HR",
                                                  "HU",
                                                  "IS",
                                                  "IE",
                                                  "IM",
                                                  "IT",
                                                  "JE",
                                                  "LV",
                                                  "LI",
                                                  "LT",
                                                  "LU",
                                                  "MK",
                                                  "MT",
                                                  "MD",
                                                  "MC",
                                                  "ME",
                                                  "NL",
                                                  "NO",
                                                  "PL",
                                                  "PT",
                                                  "RO",
                                                  "RU",
                                                  "SM",
                                                  "RS",
                                                  "SK",
                                                  "SI",
                                                  "SJ",
                                                  "SE",
                                                  "SW",
                                                  "TR",
                                                  "UA",
                                                  "VA"
                                                ))),
                                       column(width = 4,
                                              selectInput(
                                                ns("locationType"),
                                                "locationType",
                                                choices = c(
                                                  "",
                                                  "administrative unit",
                                                  "hunting ground",
                                                  "management unit",
                                                  "study area",
                                                  "biogeographical unit",
                                                  "grid"
                                                )
                                              )),
                                       column(width = 4,
                                              selectInput(
                                                ns("xyType"),
                                                "xyType",
                                                choices = c(
                                                  "",
                                                  "polygon",
                                                  "spatial line",
                                                  "EEA grid",
                                                  "UTM grid",
                                                  "grid (other)",
                                                  "NUTS" ,
                                                  "coordinates"
                                                )
                                              )),

                                       column(width = 4,
                                              selectInput(
                                                ns("xyUncertainty"),
                                                "xyUncertainty",
                                                choices = c(
                                                  "",
                                                  "0-10m",
                                                  "10-50m",
                                                  "50-100m",
                                                  "100m-1km",
                                                  ">1km",
                                                  "unknown"
                                                )
                                              ))


                                     )),

                                   fluidRow(
                                     style = "display:inline-block;width:100%;text-align: center;",
                                     actionButton(
                                       ns("valid_spatial"),
                                       "Validation",
                                       icon = icon("arrow-circle-right"),
                                       class = "readyButton shadow"
                                     )
                                   ),

                                   box(
                                     width = 12,
                                     title = "Output preview",
                                     solidHeader = TRUE,
                                     collapsible = TRUE,
                                     collapsed = TRUE,
                                     DTOutput(ns("tbl_spatial"))
                                   )


                                 )
                               )),

                      tabPanel(
                        "BIOLOGICAL FIELDS",



                        tagList(
                          tags$br(),
                          # Add vertical spacing


                          box(
                            width = 12,
                            status = "primary",
                            title = "Pre-filled standards",
                            solidHeader = TRUE,
                            # div(style="display:inline-block;width:100%;text-align: left;",
                            #      HTML("<b>Pre-filled standards</b>")),

                            column(width = 5,
                                   DT::dataTableOutput(outputId = ns("biology_out")))
                          ),

                          box(
                            width = 12,
                            status = "primary",
                            title = "Species identification",
                            solidHeader = TRUE,
                            column(width = 12,
                                   tags$div(
                                     HTML(
                                       '<b> Generate the standard field \"scientificName\". </b>
                                              <br> If you want to fill the scientific name manually (assigned to all the observations), select <b> "Fill the scientific name (single value)" </b>
                                              <br> If you want to convert non-standard values into scientific names (usefull if your dataset contains several species), select <b> "Fill the scientific name (convert a column)" </b>. You must have mapped a column to the standard "specificEpithet".
                                                             '
                                     ),
                                     tags$br(),
                                     tags$br()
                                   )),
                            selectInput(
                              ns("extract_species"),
                              "Type of species extraction",
                              choices = c(
                                "",
                                "Fill the scientific name (single value)",
                                "Fill the scientific name (convert a column)"
                              ),
                              selected = ""
                            ),
                            conditionalPanel(
                              condition = "input.extract_species == 'Fill the scientific name (single value)'",
                              ns = NS(id),
                              textInput(ns("manual_sn"),
                                        "Enter the scientific name")
                            ),
                            conditionalPanel(
                              condition = "input.extract_species == 'Fill the scientific name (convert a column)'",
                              ns = NS(id),
                              column(
                                width = 12,
                                splitLayout(
                                  uiOutput(ns("fill_sn")),
                                  textInput(ns("convert_sn_val"),
                                            "Enter the scientific name"),
                                  actionButton(
                                    ns("add_sn_biology"),
                                    "Add scientific name",
                                    icon = icon("plus-circle")
                                  )
                                ),
                                DTOutput(ns("tbl_sn_biology"))



                              )
                            )
                          ),

                          box(
                            width = 12,
                            status = "primary",
                            title = "Standards with controlled values",
                            solidHeader = TRUE,
                            selectInput(
                              ns("occurrenceType"),
                              "Type of occurrence",
                              choices = c("", "individual count",
                                          "presence/absence")
                            ),

                            conditionalPanel(
                              condition = "input.occurrenceType == 'individual count'",
                              ns = NS(id),
                              checkboxInput(
                                ns("sum_count"),
                                "Automatically (need partial occurrences)",
                                FALSE
                              )
                            ),
                            conditionalPanel(
                              condition = "input.sum_count == 1",
                              ns = NS(id),
                              uiOutput(ns("select_sum_col")),
                              textOutput(ns("occ_partial_num"))
                            ),


                            selectInput(
                              ns("basisRecord"),
                              "Basis of record",
                              choices = c(
                                "",
                                "human observation",
                                "machine observation",
                                "statistical estimation"

                              )
                            ),

                            selectInput(
                              ns("recordType"),
                              "Type of record",
                              choices = c("", "alive",
                                          "dead",
                                          "indirect sign",
                                          "other")
                            )

                          ),


                          box(
                            width = 12,
                            status = "primary",
                            title = "Occurrence records",
                            solidHeader = TRUE,
                            textOutput(ns("presence_std")),
                            tags$br(),
                            tags$br(),
                            conditionalPanel(
                              condition = "output.presence_std == 'Absence/presence values not standard.'",
                              ns = NS(id),
                              selectInput(
                                ns("convert_os"),
                                "Occurrence values standardisation",
                                choices = c(
                                  "",
                                  "Fill the presence/absence value",
                                  "Convert the occurrenceStatus column"
                                ),
                              )
                            ),
                            conditionalPanel(
                              condition = "input.convert_os == 'Fill the presence/absence value'",
                              ns = NS(id),
                              selectInput(
                                ns("manual_os"),
                                "Presence/absence value",
                                choices = c(
                                  "",
                                  "present",
                                  "absent",
                                  "present-stational",
                                  "present-inventorial"
                                ),
                                size = 4,
                                selectize =
                                  FALSE
                              )
                            ),


                            conditionalPanel(
                              condition = "input.convert_os == 'Convert the occurrenceStatus column'",
                              ns = NS(id),
                              column(
                                width = 12,
                                splitLayout(
                                  uiOutput(ns("fill_occ")),
                                  selectInput(
                                    ns("convert_os_val"),
                                    "Presence/absence value",
                                    choices = c(
                                      "",
                                      "present",
                                      "absent",
                                      "present-stational",
                                      "present-inventorial"
                                    )
                                  ),
                                  actionButton(
                                    ns("add_occ_biology"),
                                    "Add standard value",
                                    icon = icon("plus-circle")
                                  )
                                ),
                                DTOutput(ns("tbl_occ_biology"))


                              )
                            )
                          ),
                          fluidRow(
                            style = "display:inline-block;width:100%;text-align: center;",
                            actionButton(
                              ns("valid_biology"),
                              "Validation",
                              icon = icon("arrow-circle-right"),
                              class = "readyButton shadow"
                            )
                          ),

                        )





                      ),

                      tabPanel("MEASUREMENT/FACTS",

                               mod_Mof_ui(ns("Mof_ui")))


                    )
                  ) #End box


                ) #End fluidRow) #End mainPanel

      )
    )
  )
}

#' Terrain_Box Server Function
#'
#' @noRd
mod_Fill_Occurrence_Box_server <-
  function(input,
           output,
           session,
           dtf_occurrence,
           dtf_import,
           n) {
    ns <- session$ns
    output$title <- renderText({
      paste0("Level ", as.character(n))
    })
    #-------------------------------------------------------------------------------------#
    #####-------------------------      Main Panel     -------------------------------#####
    #-------------------------------------------------------------------------------------#
    fieldsF <- c(
      "locality",
      "decimalLongitude",
      "decimalLatitude",
      "footPrintWKT",
      "locationID",
      "locationAccordingTo",
      "referenceSystem"
    )

    fieldsC <- c("locationType",
                 "xyType",
                 "xyUncertainty",
                 "countryCode")


    occurrence_fields <- reactiveValues(fields = NULL)

    id_rows <- reactiveValues(rows = NULL)

    df_temporal <- reactiveValues(dtf_temporal = data.frame())
    df_mof <- reactiveValues(dtf_mof = data.frame())
    df_spatial <- reactiveValues(
      dtf_spatial = data.frame(),
      remF = c(),
      remC = c(),
      dataFree = data.frame()
    )
    df_biology <- reactiveValues(dtf_biology = data.frame())

    tab_occurrence_2 <- reactiveValues(dtf_selected = data.frame())


    #Show the 'delete row' button when there is at least one row in in the table xtable
    observe({
      toggle("insert_occurrence_field", condition = nrow(dtf_occurrence) > 0)
    })

    observeEvent(input$insert_occurrence_field,
                 {
                   id_rows$rows <- input$tbl_occurrence_in_rows_selected

                   if (is.null(id_rows$rows)) {
                     showNotification("Select a row each from two tables on the left to manually rename.",
                                      duration = 6)
                     id_rows$rows <- c()
                   }

                   if (nrow(dtf_occurrence) > 0) {
                     tab_occurrence_2$dtf_selected <- dtf_occurrence[id_rows$rows,]
                     dtf_2 <- dtf_occurrence[id_rows$rows,]

                   }
                 })

    #Show the 'delete row' button when there is at least one row in in the table xtable
    observe({
      toggle("all_valid",
             condition = nrow(tab_occurrence_2$dtf_selected) > 0)
    })
    observeEvent(input$all_valid,

                 {
                   dtf_2 <- tab_occurrence_2$dtf_selected
                   if (nrow(dtf_2) > 0) {
                     occurrence_fields$fields <-
                       select(filter(dtf_2, type == "Occurrence"), all_of(c('fieldname', 'standard')))


                     df_temporal$dtf_temporal <-
                       as.data.frame(filter(dtf_2, (
                         concept  %in% c("Time", NA) &
                           type ==
                           "Occurrence"
                       )))

                     df_spatial$dtf_spatial <-
                       as.data.frame(filter(dtf_2, (
                         concept %in% c("Location", NA) &
                           type ==
                           "Occurrence"
                       )))

                     df_biology$dtf_biology <-
                       as.data.frame(filter(dtf_2, (
                         concept %in% c("Biology", NA) &
                           type ==
                           "Occurrence"
                       )))

                     df_mof$dtf_mof <-
                       as.data.frame(filter(dtf_2, type == "Occurrence Mof"))

                     df_spatial$remC <-
                       fieldsC[!fieldsC %in% pull(filter(dtf_2, (
                         concept == "Location" &
                           type ==
                           "Occurrence"
                       )), standard)]

                     df_spatial$remF <-
                       fieldsF[!fieldsF %in% pull(filter(dtf_2, (
                         concept == "Location" &
                           type ==
                           "Occurrence"
                       )), standard)]


                   }
                 })


    Mof <- reactiveValues()
    observe({
      Mof[["mof_df"]] <- callModule(mod_Mof_server, "Mof_ui" , df_mof,
                                    dtf_import)

    })


    # DataFrame before fieldnames selection
    output$tbl_occurrence_in <- renderDT({
      if (nrow(dtf_occurrence) > 0) {
        select(dtf_occurrence,
               all_of(c(
                 "fieldname", "standard", "concept", "type"
               )))
      } else {
        dtf_occurrence
      }

    },
    options = list(
      paging = FALSE,
      autoWidth = TRUE,
      scrollY = TRUE,
      dom = 't'
    ),
    rownames = FALSE,
    escape = F)


    # DataFrame after fieldnames selection
    output$tbl_occurrence_out <- DT::renderDataTable(DT::datatable({
      tab_occurrence_2$dtf_selected
    },
    options = list(
      paging = FALSE,
      autoWidth = TRUE,
      scrollY = TRUE,
      dom = 't'
    ),
    rownames = FALSE,
    escape = F))

    # Temporal DataFrame after fieldnames selection
    output$temporal_out <- DT::renderDataTable(DT::datatable({
      df_temporal$dtf_temporal
    },
    options = list(
      paging = FALSE,
      autoWidth = TRUE,
      scrollY = TRUE,
      dom = 't'
    ),
    rownames = FALSE,
    escape = F))

    # Spatial DataFrame after fieldnames selection
    output$spatial_out <- DT::renderDataTable(DT::datatable({
      df_spatial$dtf_spatial
    },
    options = list(
      paging = FALSE,
      autoWidth = TRUE,
      scrollY = TRUE,
      dom = 't'
    ),
    rownames = FALSE,
    escape = F))


    # Biology DataFrame after fieldnames selection
    output$biology_out <- DT::renderDataTable(DT::datatable({
      df_biology$dtf_biology
    },
    options = list(
      paging = FALSE,
      autoWidth = TRUE,
      scrollY = TRUE,
      dom = 't'
    ),
    rownames = FALSE,
    escape = F))

    #-------------------------------------------------------------------------------------#
    #####------------------------- Panel 'Temporal' -------------------------------#####
    #-------------------------------------------------------------------------------------#
    # Initialize the object 'user_analyse' as a reactive object
    preview_temporal <- reactiveValues(temporal_table = NULL)
    to_convert <- reactiveValues(df_to_convert = NULL)

    list_out_tmp <- reactiveValues(
      type_extract  = NA,
      format_date = NA,
      sep_interval = NA,
      values = NA,
      tf_dic = NA,
      new_cols = NA
    )

    observeEvent({
      input$timeLevel
      input$extract_date
      input$format_date
      input$sep_interval
      input$all_valid
    }, {
      df_mapped <- df_temporal$dtf_temporal
      df_temporal_tmp <- as.data.frame(dtf_import$results_table)

      list_out_tmp[["new_cols"]] <-
        list("timeLevel" = input$timeLevel)
      df_temporal_tmp <-
        df_temporal_tmp %>% mutate(timeLevel = input$timeLevel)

      df_out <- data.frame()
      type_extract <- input$extract_date



      if (input$format_date == '') {
        format_date <- NA
      } else {
        format_date <- input$format_date
      }

      if (input$sep_interval == '') {
        sep_interval <- NA
      } else {
        sep_interval <- input$sep_interval
      }

      if (type_extract == "2_col_auto") {
        df_out <-
          make_temporal(
            df_temporal_tmp,
            df_mapped,
            type = type_extract,
            format_date = NA,
            sep_interval = NA,
            values = NA,
            tf_dic = NA
          )

        list_out_tmp[["type_extract"]] = type_extract

      } else if (type_extract == "1_col_auto") {
        df_out <-
          make_temporal(
            df_temporal_tmp,
            df_mapped,
            type = "1_col_auto",
            format_date = format_date,
            sep_interval = sep_interval,
            values = NA,
            tf_dic = NA
          )
        list_out_tmp[["sep_interval"]] = sep_interval
        list_out_tmp[["type_extract"]] = type_extract
        list_out_tmp[["format_date"]] = format_date

      } else if (type_extract == "manu") {
        values <- c(input$date_min_manual,
                    input$date_max_manual)

        df_out <-
          make_temporal(
            df_temporal_tmp,
            df_mapped,
            type = "manu",
            format_date = NA,
            sep_interval = NA,
            values = values,
            tf_dic = NA
          )
        list_out_tmp[["type_extract"]] = type_extract
        list_out_tmp[["values"]] = values

      }

      l_col_tempo <-
        c("timeLevel",
          "dataTime",
          "beginDate",
          "endDate",
          "eventDate")
      l_col <- intersect(colnames(df_out), l_col_tempo)

      df_out <- select(df_out, all_of(l_col))
      df_out <- df_out[1:min(nrow(df_out), 5), ]

      preview_temporal$temporal_table <- df_out
    })


    list_out <- reactiveValues(parameters = "")
    observeEvent(input$valid_tempo, {
      list_out$parameters <- list_out_tmp
      if (is.na(list_out_tmp[["type_extract"]])) {
        showNotification(
          "None type of extraction selected. Event date field cannot be calculated.",
          duration = 6
        )
      }
    })

    # Display data.frame
    output$tbl_temporal <- renderDT({
      if (!is.null(preview_temporal$temporal_table)) {
        preview_temporal$temporal_table

      }

    })



    #-------------------------------------------------------------------------------------#
    #####------------------------- Panel 'Spatial' -------------------------------#####
    #-------------------------------------------------------------------------------------#
    preview_spatial <- reactiveValues(spatial_table = NULL)

    list_out_sp_tmp <- reactiveValues(new_cols = NA,
                                      new_df = NA)


    # ----------------------------------------------------------------------------
    #   Create a data.frame with untidy columns info
    # ----------------------------------------------------------------------------

    #Initialize the object 'results_analyse' as a reactive object
    free_spatial <- reactiveValues(free_df = NULL)

    # Selection of data.frame column names
    output$fill_free <- renderUI({
      # If missing input, return to avoid error later in function
      if (is.null(df_spatial$remC)) {
        fill_list <- NULL
      } else {
        fill_list <- df_spatial$remF

        if (!is.null(free_spatial$free_df)) {
          fill_list <- fill_list[!fill_list %in% free_spatial$free_df$Standard]
        }
      }

      # Selection list of old colnames
      selectInput(
        ns("free_col"),
        "Standard names",
        choices = fill_list,
        multiple = TRUE,
        size = 3,
        selectize = FALSE
      )


    })


    # Create or updtade the datafram each time
    # the user click on 'Ajouter l'analyse'.
    observeEvent(input$add_free_spatial, {
      if (is.null(free_spatial$free_df)) {
        free_spatial$free_df <- data.frame(Standard = input$free_col,
                                           Value = input$value)
      } else{
        free_spatial$free_df <- rbind(
          free_spatial$free_df ,
          data.frame(Standard = input$free_col,
                     Value = input$value)
        )
      }
    })

    # Display data.frame
    output$tbl_free_spatial = renderDT({
      free_spatial$free_df
    }, rownames = FALSE,  options = list(dom = ''), filter = "none")



    list_out_sp <- reactiveValues(parameters = "")

    observeEvent(input$valid_spatial, {
      if (!is.null(free_spatial$free_df)) {
        l <-
          setNames(as.list(free_spatial$free_df[["Value"]]), free_spatial$free_df[["Standard"]])

      } else {
        l <- list()
      }
      list_out_sp_tmp[["new_cols"]] = c(
        list(
          "locationType" = input$locationType,
          "xyType" = input$xyType,
          "xyUncertainty" = input$xyUncertainty,
          "countryCode" = input$countryCode
        ),
        l
      )

      list_out_sp_tmp[["new_df"]] = free_spatial$free_df
      list_out_sp$parameters <- list_out_sp_tmp

    })


    #-------------------------------------------------------------------------------------#
    #####------------------------- Panel 'Biology    ' -------------------------------#####
    #-------------------------------------------------------------------------------------#

    # Initialize the object 'user_analyse' as a reactive object
    user_dico <- reactiveValues(user_table = NULL)

    biology_out <-
      reactiveValues(biology_out_df = dtf_import$results_table)

    observe({
      file <- input$user_file
      ext <- tools::file_ext(file$datapath)
      req(file)
      validate(need(ext %in% c("csv", "xlsx"), "Please upload a csv file"))

      if (ext == "csv") {
        df <- read.csv(file$datapath, header = T, sep = ";")
      } else {
        df <- read_excel(file$datapath)


      }
      user_dico$user_table <- df
    })


    df_conv <- reactiveValues(df_conv_tbl = NULL)

    # ------------------------------------------------
    #   Check if occurrenceStatus values are standard
    # ------------------------------------------------

    # observeEvent(    {input$extract_species
    #                  input$user_file},
    #                  {
    #
    #
    #   if (input$extract_species== "Convert a column (load a dictionnary)") {
    #
    #     df_mapped <- df_biology$dtf_biology
    #     df_conv_tbl_tmp  <- biology_out$biology_out_df
    #
    #     if (!is.null(user_dico$user_table)) {
    #
    #       sn <- df_mapped$fieldname[which(df_mapped$standard == "specificEpithet")]
    #
    #       if (is.null(sn)) {
    #         df_conv_tbl_tmp[['specificEpithet']] <- NA
    #       } else {
    #         colnames(df_conv_tbl_tmp)[colnames(df_conv_tbl_tmp) == sn] <- "specificEpithet"
    #       }
    #       df_conv_tbl_tmp <- merge(df_conv_tbl_tmp, user_dico$user_table, by.x = "specificEpithet", by.y =  "fieldname_species")
    #
    #     }
    #
    #   }
    #   biology_out$biology_out_df <- df_conv_tbl_tmp
    #
    # })
    #

    # ------------------------------------------------
    #   Check if occurrenceStatus values are standard
    # ------------------------------------------------

    is.std <- reactiveValues(std = NULL)
    observeEvent(input$occurrenceType, {
      input$occurrenceType

      if (input$occurrenceType == "presence/absence") {
        df_mapped <- df_biology$dtf_biology
        os <-
          df_mapped$fieldname[which(df_mapped$standard == "occurrenceStatus")]

        if (!is.null(os) & length(os) > 0) {
          v <- unique(dtf_import$results_table[os])

          if (length(v[!v %in% c("present",
                                 "absent",
                                 "present-stational",
                                 "present-inventorial")]) > 0) {
            is.std$std <- "no"
          }

        } else {
          is.std$std <- "no"
        }

      }
    })

    output$presence_std <- renderText({
      input$occurrenceType
      if (is.null(is.std$std)) {
        ''
      } else {
        "Absence/presence values not standard."
      }

    })



    # ----------------------------------------------------------------------------
    #   Convert a column to obtain occurrenceStatus / scientificName
    # ----------------------------------------------------------------------------

    #Initialize the object 'biology_values' as a reactive object
    biology_values <- reactiveValues(occ_status_df = NULL,
                                     sci_name_df = NULL)


    # ------------------- #
    # 1. occurrenceStatus #
    # --------------------#

    #
    # Selection of data.frame column names
    output$fill_occ <- renderUI({
      # If missing input, return to avoid error later in function
      df_mapped <- df_biology$dtf_biology
      os <-
        df_mapped$fieldname[which(df_mapped$standard == "occurrenceStatus")]

      if (!is.null(os) & length(os) > 0) {
        fill_list_occ <- unique(dtf_import$results_table[[os]])
      } else {
        fill_list_occ <- NULL
      }

      if (!is.null(biology_values$occ_status_df)) {
        fill_list_occ <-
          fill_list_occ[!fill_list_occ %in% biology_values$occ_status_df$Initial_value]
      }


      # Selection list of old colnames
      selectInput(
        ns("old_oc_col"),
        "Non-standard values",
        choices = fill_list_occ,
        multiple = TRUE,
        size = 4,
        selectize = FALSE
      )


    })


    # Create or updtade the datafram each time
    # the user click on 'Ajouter l'analyse'.
    observeEvent(input$add_occ_biology, {
      if (is.null(biology_values$occ_status_df)) {
        biology_values$occ_status_df <-
          data.frame(
            Initial_value = input$old_oc_col,
            occurrenceStatus = input$convert_os_val
          )
      } else{
        biology_values$occ_status_df <- rbind(
          biology_values$occ_status_df ,
          data.frame(
            Initial_value = input$old_oc_col,
            occurrenceStatus = input$convert_os_val
          )
        )
      }

    })

    # Display data.frame
    output$tbl_occ_biology = renderDT({
      biology_values$occ_status_df
    }, rownames = FALSE,  options = list(dom = ''), filter = "none")

    # ------------------#
    # 2. scientificName #
    # ------------------#


    # Selection of data.frame column names
    output$fill_sn <- renderUI({
      # If missing input, return to avoid error later in function
      df_mapped <- df_biology$dtf_biology
      sn <-
        df_mapped$fieldname[which(df_mapped$standard == "specificEpithet")]

      if (!is.null(sn) && length(sn) > 0) {
        fill_list_sn <- unique(dtf_import$results_table[[sn]])
      } else {
        fill_list_sn <- NULL
      }

      if (!is.null(biology_values$sci_name_df)) {
        fill_list_sn <-
          fill_list_sn[!fill_list_sn %in% biology_values$sci_name_df$Initial_value]

      }


      # Selection list of old colnames
      selectInput(
        ns("old_sn_col"),
        "Standard names",
        choices = fill_list_sn,
        multiple = TRUE,
        size = 3,
        selectize = FALSE
      )


    })


    # Create or updtade the datafram each time
    # the user click on 'Ajouter l'analyse'.
    observeEvent(input$add_sn_biology, {
      if (is.null(biology_values$sci_name_df)) {
        biology_values$sci_name_df <-
          data.frame(
            Initial_value = input$old_sn_col,
            scientificName = input$convert_sn_val
          )

      } else{
        biology_values$sci_name_df <- rbind(
          biology_values$sci_name_df ,
          data.frame(
            Initial_value = input$old_sn_col,
            scientificName = input$convert_sn_val
          )
        )
      }

    })


    # Display data.frame
    output$tbl_sn_biology = renderDT({
      biology_values$sci_name_df
    }, rownames = FALSE,  options = list(dom = ''), filter = "none")


    # ------------------------------------
    #   Manually fill the occurrenceStatus
    # -------------------------------------

    observeEvent(input$manual_os, {
      df_conv_tbl_tmp <- biology_out$biology_out_df
      df_conv_tbl_tmp[['occurrenceStatus']] <- input$manual_os
      biology_out$biology_out_df <- df_conv_tbl_tmp
    })



    output$df_biology_out <- DT::renderDataTable(DT::datatable({
      l <-
        intersect(
          names(biology_out$biology_out_df),
          c("occurrenceStatus", "scientificName", "specificEpithet")
        )
      if (!is.null(biology_out$biology_out_df)) {
        select(biology_out$biology_out_df, all_of(l)) %>% distinct()
      }
    }
    ,
    options = list(
      paging = FALSE,
      autoWidth = TRUE,
      scrollY = TRUE,
      dom = 't'
    ),
    rownames = FALSE,
    escape = F))


    # ----------------------------------------------------------------------------
    #                     Partial occurrences
    # ----------------------------------------------------------------------------


    # Selection of data.frame column names
    output$select_sum_col <- renderUI({
      # If missing input, return to avoid error later in function
      if (is.null(dtf_occurrence)) {
        fill_list <- NULL
      } else {
        fill_list <- dtf_occurrence$fieldname

      }

      # Selection list of old colnames
      selectInput(
        ns("sum_col"),
        "Select column containing counts to sum",
        choices = fill_list,
        multiple = FALSE,
        size = 2,
        selectize = FALSE
      )


    })


    output$occ_partial_num <- renderText({
      input$sum_count
      input$sum_col
      if (is.null(input$sum_col)) {
        ''
      } else {
        dtf <- as.data.frame(dtf_import$results_table)
        is.num <- pull(
          d_test <- dtf  %>% replace(is.na(.), 0) %>%
            mutate_at(vars(input$sum_col), list(is_num = suppressWarnings(as.numeric))) %>% summarise_all( ~ sum(is.na(.))),
          is_num
        )

        is.num <-  ifelse(is.num == 0, TRUE, FALSE)

        if (isFALSE(is.num)) {
          "Invalid column (cannot be converted in integers)."
        } else {
          "Valid column."
        }
      }

    })


    list_out_biol <- reactiveValues(
      new_cols = list(),
      to_convert = list(),
      sumCount = list("sum_count" = FALSE,
                      "sum_col" = FALSE)
    )

    observeEvent(input$valid_biology, {
      # Controlled values
      list_out_biol$new_cols <-
        list("basisRecord" = input$basisRecord,
             "recordType" = input$recordType)


      list_out_biol_tmp <- list()
      df_mapped <- df_biology$dtf_biology
      sn <-
        df_mapped$fieldname[which(df_mapped$standard == "specificEpithet")]
      os <-
        df_mapped$fieldname[which(df_mapped$standard == "occurrenceStatus")]

      if (!is.null(input$sum_count) & !is.null(input$sum_col)) {
        dtf <- as.data.frame(dtf_import$results_table)
        is.num <- pull(
          d_test <- dtf %>% replace(is.na(.), 0) %>%
            mutate_at(vars(input$sum_col), list(is_num = suppressWarnings(as.numeric))) %>% summarise_all( ~ sum(is.na(.))),
          is_num
        )

        is.num <-  ifelse(is.num == 0, TRUE, FALSE)

        if (isTRUE(is.num)) {
          list_out_biol[["sumCount"]] <- list("sum_count" = input$sum_count,
                                              "sum_col" = input$sum_col)
        }
      }

      if (!is.null(biology_values$sci_name_df) & !is.null(sn)) {
        list_out_biol_tmp[[sn]] <- biology_values$sci_name_df

      } else {
        list_out_biol$new_cols <- c(list_out_biol$new_cols,

                                    "scientificName" = input$manual_sn)
      }

      if (!is.null(biology_values$occ_status_df) & !is.null(os)) {
        list_out_biol_tmp[[os]] <- biology_values$occ_status_df
      } else {
        list_out_biol$new_cols <- c(list_out_biol$new_cols,
                                    "occurrenceStatus" = input$manual_os)
      }

      if (nrow(tab_occurrence_2$dtf_selected > 0)) {
        to_exclude <- pull(tab_occurrence_2$dtf_selected, standard)
        list_out_biol$new_cols <-
          exclude(list_out_biol$new_cols, to_exclude)
      }

      list_out_biol$to_convert <- list_out_biol_tmp

    })


    # ----------------------------------------------------------------------------
    #                     Construction dataframe "Terrain"
    # ----------------------------------------------------------------------------

    # Step 1 - Initialization: before any user input, the dataframes are all NULL.

    OccurrenceFields <-
      reactiveValues(
        Field_list = NULL,
        Tempo = NULL,
        Spatial = NULL,
        Biology = NULL,
        Mof = NULL
      ) # Will store all text and selected values


    # Step 2 - The user add a new value on the interface:
    # replace the corresponding value in the corresponding dataframe.
    observe({
      OccurrenceFields$Field_list <- occurrence_fields$fields
      OccurrenceFields$Tempo <- list_out$parameters
      OccurrenceFields$Spatial <- list_out_sp$parameters
      OccurrenceFields$Biology <- list_out_biol
      OccurrenceFields$Mof <- Mof$mof_df

    })
    #

    return(OccurrenceFields)
    #


  }

