#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom shinydashboard updateTabItems
#' @importFrom golem get_golem_options
#' @importFrom shinyjs onclick
#' @import DT 
#' @noRd
app_server <- function( input, output, session ) {
  # List the first level callModules here
 
  
  Import.dtf <- callModule(mod_Import_server, "Import_ui_1", PreFilled.dtf)
 Darwinize.dtf <- callModule(mod_Darwinize_server, "Darwinize_ui_1", Import.dtf, PreFilled.dtf)
 
 observeEvent(Darwinize.dtf$move, {   
   updateTabItems(session, inputId = "main", selected = "Structure")
})
 
 Structure.dtf <- callModule(mod_Structure_server, "Structure_ui_1", Darwinize.dtf, PreFilled.dtf)
 
 observeEvent(Structure.dtf$move_structure, {
   updateTabItems(session, inputId = "main", selected = "Event")
 })
 
  Fill_Event.dtf <- callModule(mod_Fill_Event_server, "Fill_Event_ui_1", Structure.dtf, Import.dtf)
   Fill_Occurrence.dtf <- callModule(mod_Fill_Occurrence_server, "Fill_Occurrence_ui_1", Structure.dtf, Import.dtf)
  
  Export.dtf <- callModule(mod_Export_server, "Export_ui_1", Import.dtf, Fill_Event.dtf,  Fill_Occurrence.dtf)
 
}
