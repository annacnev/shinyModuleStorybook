
rename_list <- c("Time (Day)"="time", "EA Diluent A Suspension Dose (mg)"="diluentA_dose",
                 "EA Castor Oil Solution Dose (mg)"="castorOil_dose", "Additional Doses"="addl",
                 "Interval Between Doses"="ii")

# Module UI Function
eventTable_UI <- function(id) {
  ns <- NS(id)
  tagList(
    useSweetAlert(), # Set up shinyalert
    useShinyFeedback(),
    tags$head(tags$style(type = 'text/css','.modal-dialog { width: fit-content !important; }')),
    shinydashboardPlus::boxPlus(
      width = 12,
      title = "Event Table", 
      closable = FALSE, 
      status = "primary", 
      solidHeader = FALSE, 
      collapsible = TRUE,
      style = "min-height: 500px;",
      uiOutput(ns("eventTable_body")),
      actionButton(inputId = ns("saveTable"),label = "Save",type = "success"),
      downloadButton(ns("table_csv"), "Download in CSV"),br(),br()
    )
  )
}

eventTable_Server <- function(id) {
  moduleServer(
    id, 
    function(input, output, session){
      
      ns <- session$ns
      
      # Reactive table 
      rv <- reactiveValues()
      rv$Data <- data.table(readRDS("data/base_table.RDS")) # needs to be data.table for row indexing
      
      observeEvent(input$eventUpload,{
        file <- input$eventUpload
        ext <- tools::file_ext(file$datapath)
        shiny::req(file)
        validate(need(ext == "csv" | ext == "RDS" , "Please upload a csv or RDS file"))
        if(ext=="csv"){
          data <- read.csv(file$datapath)
        }else if(ext=="RDS"|ext=="rds"){
          data <- readRDS(file$datapath)
        }
        rv$Data <- data
      })
      
      # Contains table and buttons
      output$eventTable_body <- renderUI({
        fluidPage(
          column(6,
                 fileInput(ns("eventUpload"), "Upload your own event table?", accept = c(".csv",".RDS"),multiple = F),
                 helpText("Add or Remove Dosing Events")),
          column(6,
                 HTML('<div class="btn-group" role="group" aria-label="Basic example" style = "padding:10px">'),
                 div(style="display:inline-block;width:30%;text-align: center;",
                     actionButton(inputId = ns("Add_row_head"),label = "Add")),
                 div(style="display:inline-block;width:30%;text-align: center;",
                     actionButton(inputId = ns("mod_row_head"),label = "Edit")),
                 div(style="display:inline-block;width:30%;text-align: center;",
                     actionButton(inputId = ns("Del_row_head"),label = "Delete")),
                 HTML('</div>')),
          
          column(12, dataTableOutput(ns("eventTable"))),
          tags$script(sprintf("
                              $(document).on('click', '#%s button', function () {
                              Shiny.onInputChange('%s',this.id);
                              Shiny.onInputChange('%s', Math.random()) });", 
                              ns("eventTable"), ns("lastClickId"), ns("lastClick")))
        )
      })
      outputOptions(output, "eventTable_body", suspendWhenHidden = FALSE)
      
      # Render event Table 
      output$eventTable <- renderDataTable({
        DT <- rv$Data
        datatable(DT,selection = 'single', escape=F, colnames = names(rename_list)) 
      })
      outputOptions(output, "eventTable", suspendWhenHidden = FALSE)
      
      # Module to add Row
      observeEvent(input$Add_row_head, {
        showModal(modalDialog(title = "Add a new row",
                              tableButtons_UI(ns("rowButtons"),temp_df=rv$Data, 
                                              add_tag=input$Add_row_head, labels=rename_list),
                              actionButton(ns("go"), "Add item"),
                              easyClose = TRUE, footer = NULL))
        # to catch observeEvents
        tableButtons_Server("rowButtons", temp_df=rv$Data, add_tag=input$Add_row_head)
      })
      
      # Add new row to table  
      observeEvent(input$go, {
        datafile <- tableButtons_Server("rowButtons", temp_df=rv$Data, add_tag=input$Add_row_head)
        new_row <- data.frame(datafile())
        rv$Data <- data.table(rbind(rv$Data, new_row))
        removeModal()
      })
      
      # save to RDS  
      observeEvent(input$saveTable,{
        saveRDS(rv$Data, "data/base_table.RDS")
        sendSweetAlert(session=session, title = "Saved!", type = "success")
      })
      
      # Delete selected rows
      observeEvent(input$Del_row_head,{
        showModal(
          if(length(input$eventTable_rows_selected) >= 1 ){
            modalDialog(
              title = "Warning",
              paste("Are you sure delete",length(input$eventTable_rows_selected),"rows?" ),
              footer = tagList(
                modalButton("Cancel"),
                actionButton(ns("ok"), "Yes")
              ), easyClose = TRUE)
          }else{
            modalDialog(
              title = "Warning",
              paste("Please select row(s) that you want to delete!" ),easyClose = TRUE)
          })
      })
      
      observeEvent(input$ok, {
        rv$Data <- rv$Data[-input$eventTable_rows_selected]
        removeModal()
      })
      
      # Edit Row
      observeEvent(input$mod_row_head,{
        showModal(
          if(length(input$eventTable_rows_selected) >= 1){
            modalDialog(
              fluidPage(
                h3(strong("Modification"),align="center"),
                hr(),
                dataTableOutput(ns('row_modif')), br(),
                actionButton(ns("save_changes"),"Save changes"),
                tags$script(HTML(sprintf("
                             $(document).on('click', '#%s', function () {
                             var list_value=[]
                             for (i = 0; i < $( '.new_input' ).length; i++){
                             list_value.push($( '.new_input' )[i].value)}
                             Shiny.onInputChange('%s', list_value) });", ns("save_changes"),ns("newValue"))))
              ), size="l")
          }else{
            modalDialog(
              title = "Warning",
              paste("Please select the row that you want to edit!" ), easyClose = TRUE)
          }
        )
      })
      
      observeEvent(input$save_changes, {removeModal()})
      
      # Modified Table
      output$row_modif <- renderDataTable({
        selected_row <- input$eventTable_rows_selected
        old_row <- rv$Data[selected_row]
        row_change <- list()
        for (i in colnames(old_row)){
          if(is.numeric(rv$Data[[i]])){
            row_change[[i]] <- paste0('<input class="new_input" value= ','"',old_row[[i]],'"','  type="number" id=new_',i,' ><br>')
          }else{ 
            row_change[[i]] <- paste0('<input class="new_input" value= ','"',old_row[[i]],'"',' type="textarea"  id=new_',i,'><br>')
          }
        }
        row_change <- as.data.table(row_change)
        setnames(row_change,colnames(old_row))
        DT <- row_change
        DT 
        #datatable(DT, escape=F, colnames = names(rename_list)) 
      },escape=F, options=list(dom='t', ordering=F, scrollX = TRUE), selection="none",colnames = names(rename_list))
      
      # Modify Row
      observeEvent(input$newValue,{
        newValue <- lapply(input$newValue, function(col) {
          if (suppressWarnings(all(!is.na(as.numeric(as.character(col)))))) {
            as.numeric(as.character(col))
          }else{
            col
          }
        })
        DF <- data.frame(lapply(newValue, function(x) t(data.frame(x))))
        colnames(DF) <- colnames(rv$Data)
        rv$Data[input$eventTable_rows_selected] <- DF
      })
      
      # Download as CSV
      output$table_csv <- downloadHandler(
        filename = function() {
          paste("event_table", Sys.Date(), ".csv", sep="")
        },
        content = function(file) {
          write.csv(data.frame(rv$Data), file, row.names = F)
        }
      )
      
    })
}

