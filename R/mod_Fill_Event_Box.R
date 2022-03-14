#' Fill_Event_Box UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd

mod_Fill_Event_Box_ui <- function(id, PreFilled.box) {
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
                      id = "event_tabs",
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
                               shiny::dataTableOutput(ns("tbl_event_in"))),
                        column(
                          1,
                          actionButton(
                            ns("insert_event_field"),
                            "Select fieldname",
                            icon("arrow-circle-right"),
                            class = "readyButton shadow"
                          )
                        ),
                        column(width = 6,
                               shiny::dataTableOutput(ns("tbl_event_out"))),

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
                                   shiny::dataTableOutput(outputId = ns("temporal_out")))
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
                            title = "Extraction of the date",
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
                                "From a single column" = "1_col_auto",
                                #"From a single column (manually)" = "1_col_manu",
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
                              shiny::dataTableOutput(ns('ex_df'))
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
                          ),

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

                      tabPanel(
                        "SPATIAL FIELDS",
                        tagList(
                          tags$br(),
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

                          fluidRow(
                            tags$br(),
                            # Add vertical spacing
                            tags$br(),
                            box(
                              width = 12,
                              status = "primary",
                              title = "Pre-filled standards",
                              solidHeader = TRUE,
                              column(width = 5,
                                     shiny::dataTableOutput(outputId = ns("spatial_out")))
                            ),

                            box(
                              width = 12,
                              status = "primary",
                              title = "Standards with free values",
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
                              column(width = 12,
                                     column(
                                       width = 4,
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
                                           "VA" )

                                       )),
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


                              )
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


                      )
                      ,
                      tabPanel("MEASUREMENT/FACTS",

                               mod_Mof_ui(ns("Mof_ui")))





                    )


                  ) #End box) #End fluidRow

                ) #End mainPanel

      )
    )
  )
}


mod_Fill_Event_Box_server <-
  function(input,
           output,
           session,
           dtf_event,
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


    event_fields <- reactiveValues(fields = NULL)

    id_rows <- reactiveValues(rows = NULL)

    df_temporal <- reactiveValues(dtf_temporal = data.frame())
    df_mof <- reactiveValues(dtf_mof = data.frame())
    df_spatial <- reactiveValues(
      dtf_spatial = data.frame(),
      remF = c(),
      remC = c(),
      dataFree = data.frame()
    )

    tab_event_2 <- reactiveValues(dtf_selected = data.frame())


    #Show the 'delete row' button when there is at least one row in in the table xtable
    observe({
      toggle("insert_event_field", condition = nrow(dtf_event) > 0)
    })

    observeEvent(input$insert_event_field,
                 {
                   id_rows$rows <- input$tbl_event_in_rows_selected

                   if (is.null(id_rows$rows)) {
                     id_rows$rows <- c()
                   }

                   if (nrow(dtf_event) > 0) {
                     tab_event_2$dtf_selected <- dtf_event[id_rows$rows, ]
                     dtf_2 <- dtf_event[id_rows$rows, ]

                   }
                 })


    #Show the 'delete row' button when there is at least one row in in the table xtable
    observe({
      toggle("all_valid", condition = nrow(tab_event_2$dtf_selected) > 0)
    })

    observeEvent(input$all_valid,

                 {
                   dtf_2 <- tab_event_2$dtf_selected
                   if (nrow(dtf_2) > 0) {
                     event_fields$fields <-
                       select(filter(dtf_2, type == "Event"), all_of(c('fieldname', 'standard')))
                     df_temporal$dtf_temporal <-
                       as.data.frame(filter(dtf_2, (concept == "Time" &
                                                      type ==
                                                      "Event")))

                     df_spatial$dtf_spatial <-
                       as.data.frame(filter(dtf_2, (
                         concept == "Location" &
                           type ==
                           "Event"
                       )))

                     df_mof$dtf_mof <-
                       as.data.frame(filter(dtf_2, type == "Event Mof"))
                     df_spatial$remF <-
                       fieldsF[!fieldsF %in% pull(filter(dtf_2, (
                         concept == "Location" &
                           type ==
                           "Event"
                       )), standard)]

                     df_spatial$remC <-
                       fieldsC[!fieldsC %in% pull(filter(dtf_2, (
                         concept == "Location" &
                           type ==
                           "Event"
                       )), standard)]


                   }
                 })


    # DataFrame before fieldnames selection
    output$tbl_event_in <- renderDT({
      if (nrow(dtf_event) > 0) {
        select(dtf_event,
               all_of(c(
                 "fieldname", "standard", "concept", "type"
               )))
      } else {
        dtf_event
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
    output$tbl_event_out <- DT::renderDataTable(DT::datatable({
      tab_event_2$dtf_selected
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
      input$date_min_manual
      input$date_max_manual
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
            format_date = format_date,
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

        list_out_tmp[["type_extract"]] = type_extract
        list_out_tmp[["format_date"]] = format_date
        list_out_tmp[["sep_interval"]] = sep_interval
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

      } else if (type_extract == "1_col_manu") {
        # data_time_field <- df_mapped$fieldname[which(df_mapped$standard == "dataTime")]
        # vals <- unique(df_temporal_table_tmp[data_time_field])
        # if (length(vals) == 0) vals <- NA
        # to_convert$df_to_convert <- data.frame("values" = vals,
        #                                        "beginDate" = NA,
        #                                        'endDate' = NA)

      }

      l_col_tempo <-
        c("timeLevel",
          "dataTime",
          "beginDate",
          "endDate",
          "eventDate")
      l_col <- intersect(colnames(df_out), l_col_tempo)

      df_out <- select(df_out, all_of(l_col))
      df_out <- df_out[1:min(nrow(df_out), 5),]

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

    # output$ex_df <- renderDT({
    #   if (!is.null(to_convert$df_to_convert)) {
    #     colnames(to_convert$df_to_convert) <- c('dataTime', 'beginDate', 'endDate')
    #   }
    #   to_convert$df_to_convert},
    #   selection = 'none', editable = list(target = "column", disable = list(columns = 1),
    #                                       server = TRUE, rownames = FALSE)
    # )
    #  observeEvent(input$all_good,



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
          fill_list <-
            fill_list[!fill_list %in% free_spatial$free_df$Standard]
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


      new_cols <- c(
        list(
          "locationType" = input$locationType,
          "xyType" = input$xyType,
          "xyUncertainty" = input$xyUncertainty,
          "countryCode" = input$countryCode
        ),
        l
      )

      if (nrow(tab_event_2$dtf_selected > 0)) {
        to_exclude <-
          fieldsC[fieldsC %in% pull(tab_event_2$dtf_selected, standard)]
        new_cols <- exclude(new_cols, to_exclude)
      }

      list_out_sp_tmp[["new_cols"]] = new_cols
      list_out_sp_tmp[["new_df"]] = free_spatial$free_df
      list_out_sp$parameters <- list_out_sp_tmp

    })





    # output$tbl_spatial <- renderDT({
    #   tmp <- data.frame()
    #   if (!is.null(dtf_import$results_table)) {
    #
    #     tmp <- bind_cols(dtf_import$results_table, editable_dt())
    #     tmp[tmp==""] <-NA
    #     tmp <- tmp[,colSums(is.na(tmp))<nrow(tmp)]
    #     tmp[1:5,]%>%distinct()
    #
    #   }
    #   tmp[1:5,]%>%distinct()
    #   # dtf_import$results_table, all_of(c(spatialFieldsFree, spatialFieldsControlled))
    # })

    # Mof <- reactiveValues()
    # observe({
    #   Mof[["mof_df"]]<- callModule(mod_Mof_server, "Mof_ui" , df_mof,
    #                                dtf_import)
    #
    # })

    # ----------------------------------------------------------------------------
    #                     Construction dataframe "Terrain"
    # ----------------------------------------------------------------------------

    # Step 1 - Initialization: before any user input, the dataframes are all NULL.

    EventFields <- reactiveValues(
      Field_list = NULL,
      Tempo = NULL,
      Spatial = NULL,
      Mof = NULL
    ) # Will store all text and selected values


    #  EventFields <- reactiveValues(Field_list = NULL) # Will store all text and selected values

    # Step 2 - The user add a new value on the interface:
    # replace the corresponding value in the corresponding dataframe.
    observe({
      EventFields$Field_list <- event_fields$fields
      EventFields$Tempo <- list_out$parameters
      EventFields$Spatial <- list_out_sp$parameters
      EventFields$Mof <-
        callModule(mod_Mof_server, "Mof_ui" , df_mof,
                   dtf_import)
    })
    #

    return(EventFields)
    #


  }
