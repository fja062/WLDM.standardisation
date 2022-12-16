#' Export UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Export_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h1("Structure of the dataset (events/occurrence)"),

    tags$br(),

    fluidRow(
      # Event box
      box(
        width = 16,
        title = "Events",
        solidHeader = TRUE,
        collapsible = TRUE,
        collapsed = TRUE,

        DT::dataTableOutput(ns("to_return_event"))

      ),
      box(
        width = 16,
        title = "Occurrence",
        solidHeader = TRUE,
        collapsible = TRUE,
        collapsed = TRUE,

        DT::dataTableOutput(ns("to_return_occ"))


      ),

      box(
        width = 16,
        title = "Events Measurements/facts",
        solidHeader = TRUE,
        collapsible = TRUE,
        collapsed = TRUE,

        DT::dataTableOutput(ns("to_return_mof_event"))


      ),

      box(
        width = 16,
        title = "Occurence Measurements/facts",
        solidHeader = TRUE,
        collapsible = TRUE,
        collapsed = TRUE,


        DT::dataTableOutput(ns("to_return_mof_occ"))


      ),

      box(
        width = 16,
        title = "Visualisation",
        solidHeader = TRUE,
        collapsible = TRUE,
        collapsed = TRUE,
        style = "overflow-x: scroll;",
        fluidPage(
          checkboxInput(ns("visu"), "Generate visualisation", FALSE),
          htmlOutput(ns("inc"))
          #   includeHTML("C:/Users/sarah.valentin/Documents/R/WLDM_shiny/WDLM_shiny_en_cours/docs/simple_grv.html")




        )
      ),
      box(
        width = 16,
        title = "Download dataset",
        solidHeader = TRUE,
        collapsible = FALSE,
        collapsed = FALSE,
        column(width = 8,
               selectInput(
                 ns("data_type"),
                 "Data type",
                 choices = c(
                   "hunting",
                   "occurrence",
                   "density"
                 )
               )
               ),
        column(
          width = 4,

        ui <- fluidPage(
          checkboxInput(ns("identifier"), "Generate a dataset identifier", FALSE),
          downloadButton(ns("dl"), "Download")
        )
      ))

    )
  )
}

#' Export Server Function
#'
#' @description  A shiny Module.
#' @noRd
mod_Export_server <-
  function(input,
           output,
           session,
           dtf_import,
           dtf.event,
           dtf.occurrence) {
    event_file <- reactiveValues(event_file_df = NULL,
                                 event_mof = data.frame())
    occ_file <- reactiveValues(occ_file_df = NULL,
                               occ_mof = data.frame())

    observe({
      if (!is.null(dtf.event$fields)) {
        mapping <- data.frame(dtf.event$fields)
        if (!is.null(dtf.occurrence$fields)) {
          mapping <- bind_rows(mapping, data.frame(dtf.occurrence$fields))
        } else {
          dtf.occurrence <-
            list("fields" = data.frame(), "parameters" = list())
        }


        mapping <- mapping[, c("fieldname", "standard")]
        df  <- as.data.frame(dtf_import$results_table)

        df_agg <- createAggregatedDF(df, dtf.event, dtf.occurrence)

        event_file$event_file_df <-
          createLongFileEvent(df_agg, dtf.event, mapping)

        occ_file$occ_file_df <-
          createLongFileOcc(df_agg, dtf.event, dtf.occurrence, mapping)
        MoF_Event <- createMoF(dtf.event, df_agg, typeLevel = "Event")
        event_file$event_mof <-
          createMoFMoF(dtf.event, MoF_Event, df_agg, typeLevel = "Event")
        MoF_Occ <-
          createMoF(dtf.occurrence, df_agg, typeLevel = "Occurrence")
        occ_file$occ_mof <-
          createMoFMoF(dtf.occurrence, MoF_Occ, df_agg, typeLevel = "Occurrence")

      }
    })


    diagram <- reactiveValues(diagram_plot_exp = NULL,
                              diagram_plot_imp = NULL)
    getPage <- function() {
      return(includeHTML(paste0(getwd(), "/docs/simple_grv.html")))
    }
    observeEvent(input$visu, {
      if (!is.null(dtf.event$fields) & !is.null(dtf.occurrence$fields)) {
        mapping <- data.frame(dtf.event$fields)
        mapping <-
          bind_rows(mapping, data.frame(dtf.occurrence$fields))
        mapping <<- mapping[, c("fieldname", "standard")]
        df <- df_for_diagram(dtf.event, dtf.occurrence, mapping)

        d <- generate_diagram(df, mapping)
        nodeLabel <<- d["nodeLabel"]
        subgraphEvent <<- d["subgraphEvent"]
        subgraphOccu <<- d["subgraphOccu"]
        subgraphMeasurement <<- d["subgraphMeasurement"]
        subgraphMeasurementO <<- d["subgraphMeasurementO"]
        allGraph <<- d["allGraph"]



        diagram$diagram_plot_exp <- grViz(
          "
      digraph boxes_and_circles {

      # a 'graph' statement
      graph [ fontsize = 13,rankdir=LR]

      # several 'node' statements
      node [shape = rectangle,
      fontname = Helvetica,
      fontsize = 8,
      style = filled]
      @@1


      # several 'edge' statements
      edge [color = grey, arrowtail = none]

      # subgraph event

      @@2

      # subgraph occurrence

      @@3

      # subgraph mof

      @@4

# subgraph mof

      @@5

      @@6
      }

[1]:nodeLabel
[2]:subgraphEvent
[3]:subgraphOccu
[4]:subgraphMeasurement
[5]:subgraphMeasurementO
[6]:allGraph


"
        )


        save_html(HTML(export_svg(diagram$diagram_plot_exp)),
                  paste0(getwd(),
                         "/docs/simple_grv.html"))


        diagram$diagram_plot_imp <- getPage()
      }
    })


    output$inc <- renderUI({
      diagram$diagram_plot_imp
    })


    observeEvent(input$identifier, {
      dataset_id <- UUIDgenerate(FALSE)
      event_file$event_file_df <-
        bind_cols(data.frame("datasetID" = dataset_id),
                  event_file$event_file_df)
    })

        observeEvent(input$dataType, {
      dataset_id <- UUIDgenerate(FALSE)
      event_file$event_file_df <-
        bind_cols(data.frame("datasetID" = dataset_id),
                  event_file$event_file_df)
    })



    output$to_return_event <-  DT::renderDataTable(DT::datatable({
      event_file$event_file_df
    },
    options = list(
      paging = FALSE,
      autoWidth = TRUE,
      scrollY = TRUE,
      dom = 't'
    ),
    rownames = FALSE,
    escape = F))

    output$to_return_occ <-  DT::renderDataTable(DT::datatable({
      occ_file$occ_file_df
    },
    options = list(
      paging = FALSE,
      autoWidth = TRUE,
      scrollY = TRUE,
      dom = 't'
    ),
    rownames = FALSE,
    escape = F))

    output$to_return_mof_event <-  DT::renderDataTable(DT::datatable({
      event_file$event_mof
    },
    options = list(
      paging = FALSE,
      autoWidth = TRUE,
      scrollY = TRUE,
      dom = 't'
    ),
    rownames = FALSE,
    escape = F))


    output$to_return_mof_occ <-  DT::renderDataTable(DT::datatable({
      occ_file$occ_mof
    },
    options = list(
      paging = FALSE,
      autoWidth = TRUE,
      scrollY = TRUE,
      dom = 't'
    ),
    rownames = FALSE,
    escape = F))



    output$dl <- downloadHandler(
      filename = function() {
       paste0("WLDM_",
              input$data_type, "_",
              event_file$event_file_df$countryCode, "_",
              str_extract(string = event_file$event_file_df$eventDate, pattern = r"(^.*(?=/))"),
              #min(year(dmy(str_extract(string = event_file$event_file_df$eventDate, pattern = r"(^.*(?=/))")))),
              #min(year(dmy(event_file$event_file_df$beginDate))),
              "-",
              str_extract(string = event_file$event_file_df$eventDate, pattern = r"((?<=/).*$)"),
              "_",
              #max(year(dmy(str_extract(string = event_file$event_file_df$eventDate, pattern = r"((?<=/).*$)")))), "_",
              #min(year(dmy(event_file$event_file_df$beginDate))),
              str_replace(occ_file$occ_file_df$scientificName, " ", "-"), "_",
              event_file$event_file_df$datasetID,
             ".xlsx")
      },
      content = function(file) {
        write.xlsx(
          x = tribble(~`FIELD CODE`, ~FILLED,
                      "title","",
                      "citation","",
                      "datasetID","",
                      "url","",
                      "extractionDate","",
                      "article","",
                      "collectionFrameworkName","",
                      "collectionFrameworkID","",
                      "collectionFrameworkType","",
                      "originalLanguage","",
                      "provName","",
                      "provSurname","",
                      "provAffiliation","",
                      "provAddress","",
                      "provEmail","",
                      "provPhone","",
                      "provPostCode","",
                      "provCity","",
                      "provCountry","",
                      "role","",
                      "dsaSigner","",
                      "dsaDate","",
                      "accessibility","",
                      "gbifAuthorization","",
                      "descriptionVerbatim","",
                      "datasetType","",
                      "updateFrequency","",
                      "countryCode","",
                      "geographicScale","",
                      "beginDate","",
                      "endDate","",
                      "temporalResolution","",
                      "Taxon","",
                      "samplingProcess","",
                      "samplingFrame","",
                      "samplingFrameAvailability","",
                      "samplingFrameUrl","",
                      "samplingFrameSize","",
                      "plannedSampleSize","",
                      "sampleSize","",
                      "sampleSizeUnit","",
                      "fieldProtocolType","",
                      "totalEffortDefinition","",
                      "totalEffort","",
                      "totalEffortUnit","",
                      "samplingVerbatim","",
                      "outputVariables","",
                      "stratificationVariables","",
                      "stratumVariables","",
                      "clusterVariables","",
                      "sizeVariables","",
                      "covariates","",
                      "analysisFamily","",
                      "analysisName","",
                      "inference","",
                      "incertitudeInformationType","",
                      "validationMethod","",
                      "dataSourceID","",
                      "scriptID","",
                      "sofware","",
                      "analysisVerbatim",""
                      ),
          file = file,
          sheetName = "Metadata",
          append = FALSE,
          showNA = FALSE
        )

        write.xlsx(
          x = event_file$event_file_df,
          file = file,
          sheetName = "Event",
          append = TRUE,
          row.names = FALSE,
          showNA = FALSE
        )


        if (nrow(occ_file$occ_file_df) > 0) {
          write.xlsx(
            x = occ_file$occ_file_df,
            file = file,
            sheetName = "Occurrence",
            append = TRUE,
            row.names = FALSE,
            showNA = FALSE
          )

        }

        if (nrow(event_file$event_mof) > 0) {
          write.xlsx(
            x = event_file$event_mof,
            file = file,
            sheetName = "MoF_Event",
            append = TRUE,
            row.names = FALSE,
            showNA = FALSE
          )
        }

        if (nrow(occ_file$occ_mof) > 0) {
          write.xlsx(
            x = occ_file$occ_mof,
            file = file,
            sheetName = "MoF_Occurrence",
            append = TRUE,
            row.names = FALSE,
            showNA = FALSE
          )
        }




      }
    )



  }
