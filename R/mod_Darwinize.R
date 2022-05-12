#' Darwinize UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Darwinize_ui <- function(id) {
  ns <- NS(id)
  tagList(
    #tagList(
    h1("Mapping user names with Darwin Core standards."),
    box(
      width = 12,
      status = "primary",
      collapsible = T,
      tags$div(
        HTML('<i class="fas fa-info-circle"></i>'),
        HTML(
          '<b> Create the mapping between the user column names (fieldnames) and Darwin Core standards. </b>
             <br> First click on <b>Darwinize</b> to find automatic matches.
             Validate the correct matches by selecting one or more matches, then click on'
        ),
        HTML('<i class="fas fa-arrow-circle-right">.</i>'),
        HTML(
          '<br> For the manual mapping, select one or more fieldnames and their corresponding concept. Chose the relevant standard names, then click on'
        ),
        HTML('<i class="fas fa-arrow-circle-right"></i>.'),
        HTML(
          '<br> If you don\'t know the concept, select \"All\" to display all the standard names. The list of standard names with their definition is available in the box <b> Darwin Core definitions</b>.
                      <br> When the mapping is finished, click on <b> Validate the mapping </b>.'
        )
      )
    ),

    tags$br(),

    fluidRow(
      box(
        width = 12,
        title = "Automatic mapping",
        status = "info",
        solidHeader = TRUE,

        column(12,

               #------- INPUT COLUMNS --------------

               div(
                 column(1, div(id = "controls",

                               fluidRow(
                                 actionButton(ns("darwinize"), "Darwinize", icon("exchange-alt"), class = "activeButton shadow")
                               ))),

                 column(4,  id = "origin2", div(
                   h4("", class = "control-header"),
                   div(class = "control-header stats-red"),

                   DT::dataTableOutput(ns("auto"))
                 )),

                 column(1, div(
                   fluidRow(actionButton(
                     ns("select_auto"), "", icon("arrow-circle-right"), class = "readyButton shadow"
                   )),
                   fluidRow(actionButton(
                     ns("remove_auto"), "", icon("backspace"), class = "readyButton shadow"
                   ))
                 )),

                 column(4,  id = "origin2", div(
                   h4("", class = "control-header"),
                   div(class = "control-header stats-red"),

                   DT::dataTableOutput(ns("auto_output"))
                 ))

               )),

        #------- CONTROLS --------------

      ),

      box(
        width = 12,
        title = "Manual mapping",
        status = "primary",
        solidHeader = TRUE,

        column(2,  id = "origin", div(
          div(class = "control-header stats-red", textOutput(ns("origin_count"))),

          DT::dataTableOutput(ns("original"))
        )),
        column(2, div(DT::dataTableOutput(ns(
          "conceptdf"
        )))),
        column(3, div(DT::dataTableOutput(ns(
          "standard"
        )))),
        column(1, div(
          div(
            id = "controls",
            hr(),
            fluidRow(actionButton(
              ns("manual"), "", icon("arrow-circle-right"), class = "readyButton shadow"
            )),
            fluidRow(actionButton(
              ns("remove"), "", icon("backspace"), class = "readyButton shadow"
            )),
            hr()
          )
        )),
        column(4, div(
          #  h4("Manual Renames", class = "control-header"),
          div(class = "control-header stats-green", textOutput(ns("manual_count"))),
          DT::dataTableOutput(ns("manualized"))
        ))
      ),

      fluidRow(style = "display:inline-block;width:100%;text-align: center;",
               actionButton(
                 ns("valid_dw"),
                 "Validate the mapping",
                 icon = icon("readyButton shadow")
               )),
      tags$br(),
      tags$br(),

      box(
        width = 12,
        title = "Darwin Core definitions",
        status = "primary",
        collapsible = TRUE,
        collapsed = TRUE,
        DT::dataTableOutput(ns("term_versions"))
      )


    )
  )
}


#' Darwinize Server Function
#'
#' @noRd
mod_Darwinize_server <- function(input,
                                 output,
                                 session,
                                 dtf_import,
                                 PreFilled.dtf) {
  ns <- session$ns
  manuals <- data.frame()

  term_versions <-
    select(read.csv2("docs/term_versions.csv", sep = ","),
           all_of(c("term_localName", "definition")))
  colnames(term_versions)[1] <- c("standard")

  darwin_dictionary <- read_excel("docs/darwin_cloud.xlsx",
                                  sheet = "model_final")

  darwin_dictionary_all <- bind_rows(darwin_dictionary,
                                     term_versions) %>%
    distinct(standard, .keep_all = TRUE)

  names_left <- reactive(names(dtf_import$results_table))

  list_concept <-
    c("Location",
      "Time",
      "Identification",
      "Biology",
      "All")

  user_dataset_names <- reactive(names(dtf_import$results_table))

  #----------------------------------------#
  #------- AUTOMATIC MATCHES --------------#
  #----------------------------------------#

  # Not validated automatic and manual matches
  auto_analyse <- reactiveValues(auto_table  = data.frame())
  manual_analyse <- reactiveValues(manual_table = data.frame())

  # Validated automatic matches
  auto_output_analyse <-
    reactiveValues(auto_output_table = data.frame())

  observeEvent(input$darwinize, {
    input$darwinize
    input$select_auto
    input$remove_auto
    input$remove

    if (is.null(dtf_import$results_table)) {
      showNotification("Load a file before darwinize.",
                       duration = 6)
    } else {
      pre_names <- names(dtf_import$results_table)
      names_left <<- pre_names
      mapped_dic <- create_mapping(darwin_dictionary, names_left)

      #Reset mapping
      auto_output_analyse$auto_output_table <- data.frame()
      manual_analyse$manual_table <- data.frame()

      if (nrow(mapped_dic) == 0) {
        showNotification("None automatic match",
                         duration = 6)
        auto_analyse$auto_table <- data.frame()
      } else {
        auto_analyse$auto_table <- mapped_dic[, c("fieldname", "standard")]
      }

    }
  })




  #Show the 'delete row' button when there is at least one row in in the table xtable
  observe({
    toggle("select_auto", condition = nrow(auto_analyse$auto_table) > 0)
  })

  observeEvent(input$select_auto, {
    input$darwinize
    input$manual
    input$remove_auto
    input$remove
    from <- input$auto_rows_selected

    if (is.null(from)) {
      showNotification("Select a row from the first table on the left to manually rename.",
                       duration = 6)
    } else {
      df <- bind_rows(auto_output_analyse$auto_output_table,
                      auto_analyse$auto_table[from,]) %>%
        distinct(fieldname, .keep_all = TRUE)


      # Remove the row from previous
      auto_analyse$auto_table <- auto_analyse$auto_table[-from,]

      # Remove old names if new dataframe has been downloaded
      df <- filter(df, fieldname %in% user_dataset_names())

      auto_output_analyse$auto_output_table <- df
      pre_names <- names_left
      names_left <<-
        pre_names[!(pre_names %in% auto_output_analyse$auto_output_table[['fieldname']])]

    }

  })

  # If a row is removed
  #Show the 'delete row' button when there is at least one row in in the table xtable
  observe({
    toggle("remove_auto",
           condition = nrow(auto_output_analyse$auto_output_table) > 0)
  })

  observeEvent(input$remove_auto, {
    input$darwinize
    input$manual
    input$remove_auto

    row_rem <- input$auto_output_rows_selected
    vec_rem <- auto_output_analyse$auto_output_table[row_rem, ]
    auto_output_analyse$auto_output_table <-
      filter(auto_output_analyse$auto_output_table,!fieldname %in% vec_rem[['fieldname']])

    auto_analyse$auto_table <- bind_rows(auto_analyse$auto_table,
                                         vec_rem)
    auto_analyse$auto_table <- filter(auto_analyse$auto_table,
                                      fieldname %in% user_dataset_names())

    names_left <<- unique(c(names_left, vec_rem[['fieldname']]))


    if (!any(class(names_left) == 'reactive') &
        nrow(auto_output_analyse$auto_output_table) > 0) {
      auto_output_analyse$auto_output_table <-
        filter(auto_output_analyse$auto_output_table,
               !fieldname %in% names_left)
    }

  })

  # OUTPUT TABLES
  output$auto <- DT::renderDataTable(DT::datatable({
    input$select_auto
    input$remove_auto
    input$darwinize
    if (any(class(names_left) == 'reactive') |
        nrow(auto_analyse$auto_table) == 0) {
      data.frame()
    } else {
      filter(auto_analyse$auto_table, fieldname %in% names_left)
    }
  },
  options = list(
    paging = FALSE,
    autoWidth = TRUE,
    scrollY = TRUE,
    dom = 't'
  ),
  rownames = FALSE,
  escape = F))

  output$auto_output <- DT::renderDataTable(DT::datatable({
    input$select_auto
    input$remove_auto
    input$darwinize
    auto_output_analyse$auto_output_table
  },
  options = list(
    paging = FALSE,
    autoWidth = TRUE,
    scrollY = TRUE,
    dom = 't'
  ),
  rownames = FALSE,
  escape = F))

  #----------------------------------------#
  #------- MANUAL MATCHES -----------------#
  #----------------------------------------#

  concept_analyse <- reactiveValues(concept_table = NULL)
  observeEvent(input$original_rows_selected,
               {
                 concept_analyse$concept_table <-
                   data.frame(concept = list_concept)
               })

  output$conceptdf <- DT::renderDataTable(DT::datatable({
    concept_analyse$concept_table

  },
  options = list(
    paging = FALSE,
    autoWidth = TRUE,
    scrollY = TRUE,
    dom = 't'
  ),
  rownames = FALSE,
  selection = 'single',
  escape = F))

  concept_name <- reactive({
    list_concept <-
      c("Location",
        "Time",
        "Identification",
        "Biology",
        "All")
    row_concept <- input$conceptdf_rows_selected
    listc <- NULL
    if (is.null(row_concept)) {
      showNotification("Select a row each from two tables on the left to manually rename.",
                       duration = 6)
    } else if (list_concept[row_concept] == "All") {
      listc <- "all"
    } else {
      listc <- list_concept[row_concept]

    }
    listc
  })




  # When a fieldname is selected
  standard_analyse <- reactiveValues(standard_table = NULL)

  observeEvent(input$conceptdf_rows_selected,
               {
                 if (concept_name() == "all") {
                   standard_analyse$standard_table <-
                     data.frame("standard" = darwin_dictionary_all[["standard"]])
                 } else {
                   standard_analyse$standard_table <-
                     data.frame("standard" = unique(pull(
                       filter(darwin_dictionary, concept == concept_name()),
                       standard
                     )))
                 }
               })

  output$standard <- DT::renderDataTable(DT::datatable({
    standard_analyse$standard_table
  },
  rownames = FALSE,
  selection = 'single',
  escape = F))

  #Initialize the object 'results_analyse' as a reactive object



  #Show the 'delete row' button when there is at least one row in in the table xtable
  observe({
    toggle("manual",
           condition = !is.null(input$standard_rows_selected))
  })

  observeEvent(input$manual, {
    input$manual
    input$select_auto
    input$remove
    from <- input$original_rows_selected
    to <- input$standard_rows_selected

    if (is.null(from) || is.null(to)) {
      showNotification("Select a row each from two tables on the left to manually rename.",
                       duration = 6)
    } else {
      from_name <- names_left[from]

      to_name <- standard_analyse$standard_table[to, c("standard")]

      manual_analyse$manual_table <-
        rbind(
          manual_analyse$manual_table,
          data.frame(fieldname = from_name, standard = to_name)
        )

      pre_names <- names_left
      names_left <<- pre_names[!(pre_names %in% from_name)]

      # ADD
      if (nrow(auto_analyse$auto_table) > 0) {
        auto_analyse$auto_table <-
          filter(auto_analyse$auto_table, fieldname != from_name)
      }

      if (!any(class(names_left) == 'reactive') &
          nrow(manual_analyse$manual_table) > 0) {
        manual_analyse$manual_table <-
          filter(manual_analyse$manual_table,
                 !fieldname %in% names_left) %>%
          distinct(fieldname, .keep_all = TRUE)
      }


    }

  })

  #Show the 'delete row' button when there is at least one row in in the table xtable
  observe({
    toggle("remove", condition = nrow(manual_analyse$manual_table) > 0)
  })

  # If a row is removed
  observeEvent(input$remove, {
    input$manual
    input$select_auto
    input$remove

    row_rem <- input$manualized_rows_selected
    names_rem <-
      manual_analyse$manual_table[row_rem, c("fieldname")]

    manual_analyse$manual_table <-
      manual_analyse$manual_table[-row_rem, ]


    names_left <<- unique(c(names_left, names_rem))
  })


  output$manualized <- DT::renderDataTable(DT::datatable({
    input$darwinize
    input$manual
    input$select_auto
    input$remove

    manual_analyse$manual_table


  },
  options = list(
    paging = FALSE,
    autoWidth = TRUE,
    scrollY = TRUE,
    dom = 't'
  ),
  rownames = FALSE,
  escape = F))

  output$original <- DT::renderDataTable(DT::datatable({
    input$manual
    input$darwinize
    input$select_auto
    input$remove_auto
    input$remove
    if (any(class(names_left) == 'reactive')) {
      # as.data.frame(names_left())
      data.frame()
    } else {
      setNames(as.data.frame(names_left), "fieldname")
    }
  },
  options = list(
    paging = FALSE,
    autoWidth = TRUE,
    scrollY = TRUE,
    dom = 't'
  ),
  rownames = FALSE,
  selection = 'multiple'))


  #----------------------------------------#
  #------- MANUAL MATCHES WITH DICO -------#
  #----------------------------------------#

  output$term_versions <- DT::renderDataTable(DT::datatable({
    term_versions
  },
  # options = list(
  #   paging = FALSE,
  #   autoWidth = TRUE,
  #   scrollY = TRUE,
  #   dom = 't'
  #  ),
  rownames = TRUE,
  selection = 'single'))


  #Show the 'delete row' button when there is at least one row in in the table xtable
  observe({
    toggle("valid_dw", condition = input$darwinize)
  })


  #----------------------------------------#
  #-------FINAL EXPORT  -----------------#
  #----------------------------------------#

  Darwin <- reactiveValues(darwin_table = NULL, move = NULL)
  observeEvent(input$valid_dw, {
    df <-  bind_rows(auto_output_analyse$auto_output_table,
                     manual_analyse$manual_table)
    names_left <- names_left[!names_left %in% df[["fieldname"]]]

    if (length(names_left) > 0) {
      df <-  bind_rows(df,
                       data.frame("fieldname" = names_left, "standard" = NA))
    }

    df <- merge(
      df,
      darwin_dictionary[, c("standard", "concept")] %>%
        distinct(standard, .keep_all = TRUE),
      all.x = TRUE,
      all.y = FALSE
    )

    df <- select(df, fieldname, standard, concept)
    f1 <-
      function(x)
        ifelse(!is.na(x), ifelse(x %in% c("occurrenceID", "eventID"), "originalID", x), x)

    df <- df %>% rowwise() %>%
      mutate(standard  = f1(standard))


    Darwin$darwin_table <- df
    Darwin$move <- input$valid_dw


  })

  return(Darwin)
}
