#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboard
#' @importFrom shinyjs useShinyjs
#' @importFrom DT DTOutput
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here

    dashboardPage(

      skin = "black",

      dashboardHeader(title = "- WLDM Standardization -"),

      dashboardSidebar(

        sidebarMenu(
          id = "main",
          # Choose logo among: https://fontawesome.com/icons?d=gallery&m=free
          # I like 'seedling' or 'crow'

           menuItem("Import", tabName = "import", icon = icon("file-import"),
                        badgeColor = "green"),
          menuItem("Darwinize", tabName = "Darwinize", icon = icon("exchange-alt"),
                     badgeColor = "green"),

        menuItem("Structure", tabName = "Structure", icon = icon("border-all"),
                 badgeColor = "green"),
        menuItem("Events", tabName = "Event", icon = icon("info-circle"),
                 badgeColor = "green"),
        menuItem("Occurrence", tabName = "Occurrence", icon = icon("paw"),
                 badgeColor = "green"),
        menuItem("Export", tabName = "Export", icon = icon("file-download"),
                 badgeColor = "green")

        )
      ),

      dashboardBody(

        tags$style(HTML(".box.box-solid.box-primary>.box-header {
        color:#fff;
        background:#DD985C
        }
        .box.box-solid.box-primary{
        border-bottom-color:#DD985C;
        border-left-color:#DD985C;
        border-right-color:#DD985C;
        border-top-color:#DD985C;
        }"),
                   HTML(".box.box-solid.box-secondary>.box-header {
        color:#fff;
        background:#f3969a
        }
        .box.box-solid.box-secondary{
        border-bottom-color:#f3969a;
        border-left-color:#f3969a;
        border-right-color:#f3969a;
        border-top-color:#f3969a;
        }"
                   ),
                   HTML(".box.box-solid.box-success>.box-header {
        color:#fff;
        background:#56cc9d
        }
        .box.box-solid.box-success{
        border-bottom-color:#56cc9d;
        border-left-color:#56cc9d;
        border-right-color:#56cc9d;
        border-top-color:#56cc9d;
        }"
                   ) ,
                   HTML(".box.box-solid.box-success>.box-header {
        color:#fff;
        background:#56cc9d
        }
        .box.box-solid.box-success{
        border-bottom-color:#56cc9d;
        border-left-color:#56cc9d;
        border-right-color:#56cc9d;
        border-top-color:#56cc9d;
        }"
                   ),
                   HTML(".box.box-solid.box-success>.box-header {
        color:#fff;
        background:#56cc9d
        }
        .box.box-solid.box-success{
        border-bottom-color:#56cc9d;
        border-left-color:#56cc9d;
        border-right-color:#56cc9d;
        border-top-color:#56cc9d;
        }"
                   ),
                   HTML(".box.box-solid.box-success>.box-header {
        color:#fff;
        background:#56cc9d
        }
        .box.box-solid.box-success{
        border-bottom-color:#56cc9d;
        border-left-color:#56cc9d;
        border-right-color:#56cc9d;
        border-top-color:#56cc9d;
        }"
                   )),


        # Change specific color: https://stackoverflow.com/questions/45016826/change-color-in-shinydashboard
        tags$style(
          type = 'text/css',
          '.bg-light-blue {background-color: #DD985C!important; }',
          '.bg-aqua {background-color: #6cc3d5!important; }'
        ),

        tags$head(tags$style(HTML("
        @import url('//fonts.googleapis.com/css?family=Fira Sans Condensed|Cabin:400,700');
      .main-header .logo {
        font-family: 'Fira Sans Condensed', serif;
        font-weight: bold;
        font-size: 24px;
      }
    "))),

        #To choose a nice font: https://fonts.google.com/?sort=popularity
        tags$style(HTML("
      @import url('//fonts.googleapis.com/css?family=Fira Sans Condensed|Cabin:400,700');

      * {
        font-family: 'Fira Sans Condensed', serif;
        font-weight: 500;
        line-height: 1;
      }
    ")),
        tags$style(HTML("
      @import url('//fonts.googleapis.com/css?family=Fira Sans Condensed|Cabin:400,700');

      * {
        font-family: 'Fira Sans Condensed', serif;
        font-weight: 500;
        line-height: 1;
      }
    ")),

        tags$style(HTML("
      @import url('//fonts.googleapis.com/css?family=Fira Sans Condensed|Cabin:400,700');

      * {
        font-family: 'Fira Sans Condensed', serif;
        font-weight: 500;
        line-height: 1;
      }
    ")),

        tabItems(


          tabItem(tabName = "import",
                  useShinyjs(),
                  fluidPage(
                    mod_Import_ui("Import_ui_1")
                  )
          ),
          tabItem(tabName = "Darwinize",
                  useShinyjs(),
                  fluidPage(
                    mod_Darwinize_ui("Darwinize_ui_1")
                  )
          ),

          tabItem(tabName = "Structure",
                  useShinyjs(),
                  fluidPage(
                    mod_Structure_ui("Structure_ui_1")
                  )
          ),
          tabItem(tabName = "Event",
                  useShinyjs(),
                  fluidPage(
                    mod_Fill_Event_ui("Fill_Event_ui_1")
                  )
          ),
          tabItem(tabName = "Occurrence",
                  useShinyjs(),
                  fluidPage(
                    mod_Fill_Occurrence_ui("Fill_Occurrence_ui_1")
                  )
          ),
          tabItem(tabName = "Export",
                  useShinyjs(),
                  fluidPage(
                    mod_Export_ui("Export_ui_1")
                  )
          )

          )

        )

      ) # Close dashboardBody
    ) # Close dashboardPage


}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  #
  # add_resource_path(
  #   'www', app_sys('app/www')
  # )
  #
  # tags$head(
  #   favicon(),
  #   bundle_resources(
  #     path = app_sys('app/www'),
  #     app_title = 'surveillance.dashboard'
  #   )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
#  )
}

