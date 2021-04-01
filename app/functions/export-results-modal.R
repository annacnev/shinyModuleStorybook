export_results_modal <- function(){
  # library(shiny)
  # ns <- NS(id)
  
  modalDialog(
    style = "background-color: #ecf0f5",
    title = "Download Modules",
    size = "s",
    
    checkboxGroupInput(
      inputId = "modulesToExport",
      label = "",
      choices = c("Event Table 1" = "eventTable1",
                  "Event Table 2" = "eventTable2"),
      selected = c("eventTable1", "eventTable2")
    ),
    footer = tagList(
      actionButton(
        "cancelExport",
        "Cancel"
      ),
      downloadLink(class = 'btn btn-default',
                   "exportModules",
                   "Export",
                   icon = icon("download"))
    )
  )
}