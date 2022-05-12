mod_Import_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h1("File import and generic pre-processing"),

    tags$br(),
    box(
      width = 12,
      status = "primary",
      tagList(icon("info-circle")),
      HTML(
        "<i> <b> Import, complete and tidy your dataset. </b>
             This step corresponds to the generic standardisation of your dataset. All the fields (columns) needed for further processing have to be identified and created. </i>"
      )
    ),

    fluidRow(
      box(
        width = 6,
        title = "File import",
        status = "info",
        solidHeader = TRUE,
        fluidRow(column(
          width = 12,
          tagList(icon("info-circle")),
          HTML(
            "<i> You can specify the sheet name (only for XLS/XLSX files), a separator (for CSV and TXT files), and the first/last row indices corresponding to your data frame.
            <br> To load a <b> shapefile </b>, ensure that all the needed files are in the folder <b> user_files </b> with consistent names. </i>"
          )
        )),
        tags$br(),
        fileInput(ns("user_file"), "Choose CSV, XLS/XLSX or a TXT File"),
        numericInput(ns("skip"),
                     "First row",
                     1,
                     min = 1),
        numericInput(ns("n_max"),
                     "Last row",
                     NA,
                     min = 1),
        textInput(ns("sep"),
                  "Separator (for CSV and TXT files)",
                  ","),
        textInput(ns("sheet_name"),
                  "Sheet name (for XLS/XLSX files)",
                  NULL),

        textInput(ns("shapefile_name"),
                  "Shapefile name, without the file extension (e.g. \"wild_boar_2019\")"),

        actionButton(
          ns("load_shapefile"),
          "Load the shapefile",
          icon = icon("plus-circle")
        )


      ),

      box(
        width = 12,
        title = "Output preview",
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        collaspsed = TRUE,
        style = "overflow-x: scroll;",
        DTOutput(ns("tbl_analyse"))
        #    tableOutput(outputId = "content")
      ),


      box(
        width = 12,
        title = "Add a new column",
        status = "primary",
        solidHeader = TRUE,
        fluidRow(column(
          width = 12,
          tagList(icon("info-circle")),
          HTML(
            "<i> Fill the new column's name and its value, then click on <b>Add the new column</b>  </i>"
          )
        )),
        tags$br(),
        fluidRow(column(
          width = 10,
          splitLayout(
            textInput(ns("new_col_add"), "Column name", ""),
            textInput(ns("new_val_add"), "Value", ""),
            div(tags$br(),
                actionButton(
                  ns("add_new_col"), "Add the new column", icon = icon("plus-circle")
                ))
          )
        ))
      ),


      box(
        width = 12,
        title = "Duplicate a column",
        status = "primary",
        solidHeader = TRUE,
        fluidRow(column(
          width = 12,
          tagList(icon("info-circle")),
          HTML(
            "<i> Select the column(s) to duplicate from the list, then click on <b>Duplicate column(s)</b>  </i>"
          )
        )),
        tags$br(),
        fluidRow(
          column(width = 8,
                 uiOutput(ns("col_dupli"))),
          column(
            width = 2,
            align = "center",
            offset = 1,
            mod_Duplicate_ui(ns("Duplicate_ui"))
          )
        )
      ),

      box(
        width = 12,
        title = "Rename a column",
        status = "primary",
        solidHeader = TRUE,
        fluidRow(column(
          width = 12,
          tagList(icon("info-circle")),
          HTML(
            "<i> Select the column(s) to rename from the list, then click on <b>Rename column(s)</b>  </i>"
          )
        )),
        tags$br(),
        fluidRow(column(
          width = 10,
          splitLayout(
            uiOutput(ns("col_rename")),
            textInput(ns("col_newname"), "New column name", ""),
            div(tags$br(),
                actionButton(
                  ns("rename"),
                  "Rename column",
                  icon = icon("plus-circle")
                )))
        )
        )
      ),
      box(
        width = 12,
        title = "Convert a column",
        status = "primary",
        solidHeader = TRUE,
        fluidRow(column(
          width = 12,
          tagList(icon("info-circle")),
          HTML(
            "<i> Select the column whose values you want to convert. Select one or more initial values, fill their new value, and click on <b>Add the new value</b>.
                              <br> Once all the initial values have been assigned a new one, convert the column by clicking on <b>Convert the column</b>.
                               <br> You can convert several columns at the same time.</i>"
          )
        )),
        tags$br(),

        fluidRow(column(
          width = 10,
          splitLayout(
            uiOutput(ns("col_convert")),
            uiOutput(ns("col_convert_2")),
            textInput(ns("val_converted"),
                      "New value"),
            div(tags$br(),
                actionButton(
                  ns("add_new_val"), "Add the new value", icon = icon("plus-circle")
                ))
          )
        )),

        fluidRow(
          column(
            width = 7,
            offset = 1,
            DTOutput(ns("tbl_all_conv")),
            tags$br(),
            tags$br()

          ),
          column(
            width = 2,
            align = "center",
            offset = 1,
            mod_Convert_ui(ns("Convert_ui"))
          )

          # Print the red box 'delete' option
        )
      ),
      box(
        width = 12,
        title = "Tidy a column",
        status = "primary",
        solidHeader = TRUE,
        fluidRow(column(
          width = 12,
          tagList(icon("info-circle")),
          HTML(
            "<i> Select the column(s) to tidy. Fill the name of key column (e.g. if you selected the columns \"2017-2018\" and \"2018-2019\",
                           the key column name can be \"time period\" for instance).
                           Fill the name of the value column (e.g. if the columns \"2017-2018\" and \"2018-2019\" contain a count of individuals,
                           the row name can be \"individual count\"). Click on <b>Add</b>. Once all the initial values have been assigned a new one, tidy the columns by clicking on <b>Tidy</b>.</i>"
          )
        )),
        tags$br(),

        fluidRow(column(
          width = 10,
          splitLayout(
            uiOutput(ns("selectcomp")),
            textInput(ns("new_col"), "Key", ""),
            textInput(ns("new_val"), "Value", ""),
            div(tags$br(),
                actionButton(
                  ns("add_untidy_col"),
                  "Add",
                  icon = icon("plus-circle")
                ))
          )
        )),
        tags$br(),


        fluidRow(
          column(
            width = 7,
            offset = 1,
            DTOutput(ns("tbl_tidy_info")),
            tags$br(),
            tags$br(),

          ),

          column(
            width = 2,
            align = "center",
            offset = 1,
            # Print the red box 'delete' option
            actionButton(ns("delete_untidy_col"), "Delete", icon = icon("minus-circle"))
          ),

          mod_Tidy_Df_ui(ns("Tidy_Df_ui_import"))

        )

      ),
      box(
        width = 12,
        title = "Apply a function to one or several columns",
        status = "primary",
        solidHeader = TRUE,
        fluidRow(column(
          width = 12,
          tagList(icon("info-circle")),
          HTML(
            "<i> Select the column(s) to rename from the list, then click on <b>Rename column(s)</b>  </i>"
          )
        )),
        tags$br(),
        fluidRow(column(
          width = 12,
          splitLayout(
            selectInput(
              ns("func"),
              "Function to apply",
              choices = c("", "subset", "split", "concatenate"),
              selected = "",
              size = 3,
              selectize = FALSE),
            uiOutput(ns("col_1")),
            conditionalPanel(
              condition = "input.func == 'concatenate'",
              ns = NS(id),
              uiOutput(ns("col_2")),
              textInput(ns("sep_concat"),
                        "Character string to concatenate the columns:",
                        "")
            ),
            conditionalPanel(
              condition = "input.func == 'split'",
              ns = NS(id),
              textInput(ns("sep_split"),
                        "Character string to separate the column:",
                        "/"),
              textInput(ns("col2"),
                        "Name of the created column:",
                        "")
            ),
            conditionalPanel(
              condition = "input.func == 'subset'",
              ns = NS(id),
              numericInput(ns("start"),
                           "Index of the first element",
                           1,
                           min = 1),
              numericInput(ns("end"),
                           "Index of the last element",
                           NA,
                           min=1)
            ),

            div(tags$br(),
                actionButton(
                  ns("apply_func"),
                  "Apply function",
                  icon = icon("plus-circle")
                )))
        )
        )
      )

    )

  )


  #End fluidRow
  # End tag box

}

#' Acteur Server Function
#'
#' @description To use of the 'Sigle' selected in 'Organismes' in the page "Personnes", code adapted from: https://stackoverflow.com/questions/46555355/passing-data-within-shiny-modules-from-module-1-to-module-2
#'
#' @noRd

mod_Import_server <-
  function(input, output, session, PreFilled.dtf) {
    ns <- session$ns

    # ----------------------------------------------------------------------------
    #   Import the user data.frame (with correct sep, sheet name and first row)
    # ----------------------------------------------------------------------------

    # Initialize the object 'user_analyse' as a reactive object
    user_analyse <- reactiveValues(user_table = NULL)

    observe({
      file <- input$user_file
      ext <- tools::file_ext(file$datapath)
      req(file)
      validate(need(ext %in% c("csv", "xlsx", "xls", "shp", "txt"), "Please upload a csv file"))

      testPosInteger <- function(x) {
        test <- all.equal(x, as.integer(x), check.attributes = FALSE)
        if (test == TRUE) {
          if (x >= 1) {
            return(TRUE)
          }
          else {
            return(FALSE)
          }
        }
        else {
          return(FALSE)
        }
      }

      if (!(testPosInteger(input$skip))) {
        skip <- 1
      } else {
        skip <- input$skip
      }

      if (ext == "csv") {
        headers <-
          read.csv(
            file$datapath,
            skip = skip - 1,
            sep = input$sep,
            header = F,
            nrows = 1,
            as.is = T
          ) %>%
          janitor::remove_empty("cols")
        df <-
          read.csv(
            file$datapath,
            skip = skip,
            sep = input$sep,
            header = F
          ) %>%
          janitor::remove_empty("cols")

        colnames(df) = headers

      } else if (ext %in% c("txt")){
        if (input$sep == "\\t") {sep <- "\t"} else {sep <- input$sep}

        headers <-
          read.delim(
            file$datapath,
            skip = skip - 1,
            sep = sep,
            nrows = 1,
            as.is = T, header = F
          ) %>%
          janitor::remove_empty("cols")

        df <-
          read.delim(
            file$datapath,
            skip = skip,
            sep = sep,
            header = F
          ) %>%
          janitor::remove_empty("cols")

        if (ncol(data.frame(headers[1,])) == ncol(df)) {

          colnames(df) <- data.frame(headers[1,])}


      } else if (ext %in% c("xlsx", "xls")){

        skip <- skip - 1
        if (input$sheet_name == '') {
          sheet_name <- NULL
        } else {
          sheet_name <- input$sheet_name
        }
        l_sheet <- excel_sheets(path = file$datapath)

        if (is.na(input$n_max)) {
          n_max <- Inf
        } else {
          n_max <- input$n_max
        }

        if (is.null(sheet_name) ||  sheet_name%in% l_sheet) {
          df <-
            read_excel(
              file$datapath,
              skip = skip,
              sheet = sheet_name,
              n_max = n_max
            ) %>%
            janitor::remove_empty("cols")

        }
      } else {
        df <- NULL
      }
      user_analyse$user_table <- df
    })

    observeEvent(input$load_shapefile, {

      req(input$shapefile_name)
      shape_path <- paste0("./user_files/", input$shapefile_name, ".shp")

      if (file.exists(shape_path)) {

        df <-read_sf(shape_path) %>%
          st_simplify(., dTolerance = 100) %>%
          janitor::remove_empty()

        df <-df%>%

          mutate(footprintWKT=st_as_text(st_geometry(df)))%>%

          st_drop_geometry() %>%
          as.data.frame()

        user_analyse$user_table <- df


      }
    })


    observe({
      if (!is.null(user_analyse$user_table)) {

        colnames(user_analyse$user_table) <-
          sapply(colnames(user_analyse$user_table),
                 replaceFieldname,
                 data.frame("fieldname" = "eventID",
                            "standard" = "originalID"),
                 USE.NAMES = FALSE)
      }
    })

    # Display data.frame
    # output$tbl_analyse <- renderDT({
    #     user_analyse$user_table
    #   })

    # columnDefs allows controlling the maximal number of characters displayed in the cells (usefull with WKT columns)
    output$tbl_analyse <- renderDT({user_analyse$user_table}, options = list(
      columnDefs = list(list(
        targets = "_all",
        render = JS(
          "function(data, type, row, meta) {",
          "return type === 'display' && data != null && data.length > 30 ?",
          "'<span title=\"' + data + '\">' + data.substr(0, 30) + '...</span>' : data;",
          "}")
      ))),
      class = "display")

    # -------------------------
    #   Add a new column
    # -------------------------
    #Show the 'delete row' button when there is at least one row in in the table xtable
    observe({
      toggle("add_new_col", condition = length(input$new_col_add > 0))
    })

    # Create or updtade the datafram each time
    # the user click on 'Ajouter l'analyse'.
    observeEvent(input$add_new_col, {


      if (input$new_col_add %in% names(user_analyse$user_table)) {
        showNotification("Choose a column name that does not exist in your data frame.",
                         duration = 6)
      } else {


        user_analyse$user_table <- bind_cols(user_analyse$user_table,
                                             setNames(data.frame(input$new_val_add),
                                                      input$new_col_add)) }
    })

    # ----------------------------------------------------------------------------
    #   Create a data.frame with untidy columns info
    # ----------------------------------------------------------------------------


    # Selection of data.frame column names
    output$selectcomp <- renderUI({
      # If missing input, return to avoid error later in function
      if (is.null(user_analyse$user_table)) {
        fill_list <- NULL
      } else {
        fill_list <- names({
          user_analyse$user_table
        })

      }

      # Selection list of old colnames
      selectInput(
        ns("untidy_col"),
        "Untidy column",
        choices = unique(fill_list),
        multiple = TRUE,
        size = 4,
        selectize = FALSE
      )


    })

    #Initialize the object 'results_analyse' as a reactive object
    untidy_analyse <- reactiveValues(untidy_table = NULL)

    #Show the 'delete row' button when there is at least one row in in the table xtable
    observe({
      toggle("add_untidy_col",
             condition = !is.null(user_analyse$user_table))
    })

    # Create or updtade the datafram each time
    # the user click on 'Ajouter l'analyse'.
    observeEvent(input$add_untidy_col, {
      if (is.null(input$untidy_col) |
          is.null(input$new_col) | is.null(input$new_val)) {
        showNotification("Fill each field to add untidy column information.",
                         duration = 6)
      } else {
        if (is.null(untidy_analyse$untidy_table)) {
          untidy_analyse$untidy_table <-
            data.frame(
              Untidy_column = input$untidy_col,
              New_column = input$new_col,
              New_value = input$new_val
            )
        } else{
          untidy_analyse$untidy_table <- rbind(
            untidy_analyse$untidy_table,
            data.frame(
              Untidy_column = input$untidy_col,
              New_column = input$new_col,
              New_value = input$new_val
            )
          )
        }
      }
    })


    #Show the 'delete row' button when there is at least one row in in the table untidy_table
    observe({
      toggle("delete_untidy_col",
             condition = !is.null(untidy_analyse$untidy_table))
    })

    observeEvent(input$delete_untidy_col, {
      if (is.null(input$tbl_tidy_info_rows_selected)) {
        showNotification("Select a row.",
                         duration = 6)
      } else{
        untidy_analyse$untidy_table <-
          untidy_analyse$untidy_table[-input$tbl_tidy_info_rows_selected, ]

      }


    })


    # Display data.frame
    output$tbl_tidy_info = renderDT({
      untidy_analyse$untidy_table
    }, rownames = FALSE,  options = list(dom = ''), filter = "none")

    # ---------------------------
    #   Tidy the user data.frame
    # ---------------------------

    tidy_df_analyse <- reactiveValues(tidy_df_table = NULL)

    # Initialize
    res_tidy <- reactiveValues(lets_tidy = reactiveValues())

    # Create the red box showing the 'Tidy' option
    observe({
      res_tidy$lets_tidy <-
        callModule(mod_Tidy_Df_server,
                   "Tidy_Df_ui_import",
                   untidy_analyse,
                   res_tidy$lets_tidy)

    })

    observeEvent(res_tidy$lets_tidy$btn, {
      user_analyse$user_table <-
        tidy_data(user_analyse$user_table, untidy_analyse$untidy_table)
      untidy_analyse$untidy_table <- data.frame()

    })


    # ---------------------------
    #  Duplicate columns
    # ---------------------------


    # Selection of data.frame column names
    output$col_dupli <- renderUI({
      # If missing input, return to avoid error later in function
      if (is.null(user_analyse$user_table)) {
        fill_list <- NULL
      } else {
        fill_list <- names({
          user_analyse$user_table
        })

      }

      # Selection list of old colnames
      selectInput(
        ns("col_to_dupli"),
        "Column",
        choices = fill_list,
        multiple = TRUE,
        size = 3,
        selectize = FALSE
      )


    })

    # Initialize
    res_dupli <- reactiveValues(lets_dupli = reactiveValues())

    # Create the red box showing the 'Tidy' option
    observe({
      res_dupli$lets_dupli <-
        callModule(mod_Duplicate_server,
                   "Duplicate_ui",
                   user_analyse,
                   res_dupli$lets_dupli)
    })

    observeEvent(res_dupli$lets_dupli$btn, {
      user_analyse$user_table <-
        duplicate_columns(user_analyse$user_table,
                          input$col_to_dupli)

    })


    # ---------------------------
    #  Rename columns
    # ---------------------------


    # Selection of data.frame column names
    output$col_rename <- renderUI({
      # If missing input, return to avoid error later in function
      if (is.null(user_analyse$user_table)) {
        fill_list <- NULL
      } else {
        fill_list <- names({
          user_analyse$user_table
        })

      }

      # Selection list of old colnames
      selectInput(
        ns("col_to_rename"),
        "Column",
        choices = fill_list,
        multiple = TRUE,
        size = 3,
        selectize = FALSE
      )


    })


    observeEvent(input$rename, {
      req(input$col_to_rename, input$col_newname)

      if (input$col_newname %in% names(user_analyse$user_table)) {
        showNotification("Choose a column name that does not exist in your data frame.",
                         duration = 6)
      } else {
        user_analyse$user_table <-
          rename_base(user_analyse$user_table,
                      input$col_to_rename,
                      input$col_newname) }

    })


    # ---------------------------
    #  Apply a function to columns
    # ---------------------------


    # Selection of data.frame column names
    output$col_1 <- renderUI({
      # If missing input, return to avoid error later in function
      if (is.null(user_analyse$user_table)) {
        fill_list <- NULL
      } else {
        fill_list <- names({
          user_analyse$user_table
        })

      }

      # Selection list of old colnames
      selectInput(
        ns("col_1_selec"),
        "Column 1",
        choices = fill_list,
        multiple = TRUE,
        size = 3,
        selectize = FALSE
      )


    })

    # Selection of data.frame column names
    output$col_2 <- renderUI({
      # If missing input, return to avoid error later in function
      if (is.null(user_analyse$user_table)) {
        fill_list <- NULL
      } else {
        fill_list <- names({
          user_analyse$user_table
        })

      }

      # Selection list of old colnames
      selectInput(
        ns("col_2_selec"),
        "Column 2",
        choices = unique(fill_list),
        multiple = TRUE,
        size = 4,
        selectize = FALSE
      )
    })

    observeEvent(input$apply_func, {
      if (!input$col_1_selec %in% names(user_analyse$user_table)) {
        showNotification("Select a column to apply a function.",
                         duration = 6)

      } else {


        if (input$func == "concatenate") {
          user_analyse$user_table <- concat(user_analyse$user_table,
                                            input$col_1_selec,
                                            input$col_2_selec,
                                            input$sep_concat)

        } else if (input$func == "subset") {

          user_analyse$user_table <- substr_col(user_analyse$user_table,
                                                input$col_1_selec,
                                                input$start,
                                                input$end)

        } else if (input$func == "split") {
          if (input$col2 %in% names(user_analyse$user_table)) {
            showNotification("Choose a column name that does not exist in your data frame.",
                             duration = 6)

          } else if (is.null(input$col2) || input$col2 == "") {

            showNotification("Enter a column name.",
                             duration = 6)

          } else {
            user_analyse$user_table <- split_col(user_analyse$user_table,
                                                 input$col_1_selec,
                                                 input$col2,
                                                 input$sep_split)

          }
        }}
    })

    # ----------------------------------------------------------------------------
    #   Convert the values of a column
    # ----------------------------------------------------------------------------

    #Initialize the object 'biology_values' as a reactive object
    convert_values <- reactiveValues(convert_df = NULL)

    #
    # Selection of data.frame column names
    output$col_convert <- renderUI({
      # If missing input, return to avoid error later in function

      if (is.null(user_analyse$user_table)) {
        fill_list <- NULL
      } else {
        fill_list <- names({
          user_analyse$user_table
        })

      }

      # Selection list of old colnames
      selectInput(
        ns("col_to_convert"),
        "Column",
        choices = fill_list,
        multiple = FALSE,
        size = 4,
        selectize = FALSE
      )

    })

    output$col_convert_2 <- renderUI({
      if (!is.null(input$col_to_convert)) {
        fill_list_2 <-
          unique(user_analyse$user_table[[input$col_to_convert]])
      } else {
        fill_list_2 <- NULL
      }

      # Selection list of old colnames
      selectInput(
        ns("val_to_convert"),
        "Initial value",
        choices = unique(fill_list_2),
        multiple = TRUE,
        size = 4,
        selectize = FALSE
      )

    })



    observe({
      toggle(
        "add_new_val",
        condition = !is.null(user_analyse$user_table) &
          !is.null(input$val_to_convert)
      )
    })

    observeEvent(input$add_new_val, {
      if (is.null(convert_values$convert_df)) {
        convert_values$convert_df <-
          data.frame(
            Column = input$col_to_convert,
            Initial_value = input$val_to_convert,
            New_value = input$val_converted
          )

      } else {
        convert_values$convert_df <- bind_rows(
          convert_values$convert_df,
          data.frame(
            Column = input$col_to_convert,
            Initial_value = input$val_to_convert,
            New_value = input$val_converted
          )
        )
      }

    })


    # Display data.frame
    output$tbl_all_conv = renderDT({
      convert_values$convert_df
    }, rownames = FALSE,  options = list(dom = ''), filter = "none")

    # Initialize
    res_conv <- reactiveValues(lets_conv = reactiveValues())

    # Create the red box showing the 'Tidy' option
    observe({
      res_conv$lets_conv <-
        callModule(mod_Convert_server,
                   "Convert_ui",
                   convert_values,
                   res_conv$lets_conv)
    })

    observeEvent(res_conv$lets_conv$btn, {
      user_analyse$user_table <- convert_columns(user_analyse$user_table,
                                                 convert_values$convert_df)

    })

    Import <- reactiveValues(results_table = NULL)

    observe({
      Import$results_table <- user_analyse$user_table
    })

    return(Import)
  }

