
# Module UI ---------------------------------------------------------------

event_table_ui <- function(id){
  ns <- shiny::NS(id)
  
  tagList(
    shinydashboardPlus::boxPlus(
      width = 12,
      title = "Event Table", 
      closable = FALSE, 
      status = "primary", 
      solidHeader = FALSE, 
      collapsible = TRUE,
      style = "min-height: 500px;",
      fluidRow(
        column(width = 3,
               boxPad(color = "gray",
                      tagList(
                        h4('Add Dosing Events'),
                        numericInput(inputId = ns("time_input"),
                                     label = 'Time (Day):',
                                     min = 0, value = 0, step = 1),
                        numericInput(inputId = ns("dose_input"),
                                     label = "Dose (mg):",
                                     min = 0, value = 0, step = 1),
                        numericInput(inputId = ns("cmt_input"),
                                     label = "Compartment:",
                                     min = 0, value = 0, step = 1),
                        numericInput(inputId = ns("addl_input"),
                                     label = "Additional Doses:",
                                     min = 0, value = 0, step = 1),
                        numericInput(inputId = ns("ii_input"),
                                     label = "Interval Between Doses (Days):",
                                     min = 0, value = 0, step = 1),
                        fluidRow(
                          column(4,style= "text-align:left",
                                 actionButton(inputId = ns("add_row"), label = "Add Item")),
                          column(8,style= "text-align:right",
                                 dropdown(label = "Plot Settings", icon = icon("gear"),
                                          status = "primary", width = "340px",
                                          sliderInput(inputId = ns("plot_time"), "Max Time to plot (Days)",
                                                      value = 308, min = 280, max = 730,step = 28)))
                        )
                      ))),
        column(9,
               fluidRow(
                 column(width = 12, style="padding-left:25px; padding-right:25px;",
                        DT::DTOutput(ns('dosing_table'), height="400px"))),
               fluidRow(
                 column(width = 6, style = 'text-align:right;',
                        downloadButton(outputId = ns('save_input_table'), label = 'Save Table')))
        ))
    )
  )
}


# Module Server -----------------------------------------------------------

event_table_server <- function(id){
  moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns
      
      input_ready <- reactiveVal(FALSE)
      
      
      # Event Table -------------------------------------------------------------
      
      .rv <- reactiveValues(
        data = data.frame(
          remove = character(),
          time = numeric(),
          dose = numeric(),
          cmt = numeric(),
          addl = numeric(),
          ii = numeric()),
        data_cache = data.frame(
          remove = character(),
          time = numeric(),
          dose = numeric(),
          cmt = numeric(),
          addl = numeric(),
          ii = numeric())
      )
      
      data <- reactive({
        DT::datatable(data = .rv$data,
                      colnames = c(
                        "Remove" = "remove", "Time (Day)" = "time",
                        "Dose (mg)" = "dose",
                        "Compartment" = "cmt",
                        "Additional Doses" = "addl",
                        "Interval Between Doses (Days)" = "ii"),
                      class = "compact cell-border",
                      rownames = FALSE, fillContainer = TRUE, escape = FALSE,
                      options = list(dom = 'ltipr', columnDefs = list(
                        list(targets = 1, sortable = FALSE)))
        )
      })
      
      output$dosing_table <- DT::renderDT({
        if(nrow(.rv$data) >=1) input_ready(TRUE)
        data()
      })
      
      # Add/Remove Rows ---------------------------------------------------------
      
      observeEvent(input$add_row, {
        input_ready(FALSE)
        current_data <- .rv$data
        current_cache <- .rv$data_cache
        new_row_idx <- nrow(.rv$data_cache) + 1
        
        delete_button <- as.character(actionButton(
          paste('delete_button', new_row_idx, sep = "_"),
          label = NULL,
          icon = icon('trash'),
          onclick = paste0(
            'Shiny.setInputValue(\"', ns('deletePressed'),
            '\",  this.id, {priority: "event"})')
        ))
        
        added_rows <- data.frame(
          remove = delete_button,
          time = input$time_input,
          dose = input$dose_input,
          cmt = input$cmt_input,
          addl = input$addl_input,
          ii = input$ii_input
        )
        
        .rv$data <- dplyr::bind_rows(current_data, added_rows)
        .rv$data_cache <- dplyr::bind_rows(current_cache,added_rows)
      })
      
      
      observeEvent(input$deletePressed, {
        input_ready(FALSE)
        #row_n <- as.integer(str_extract(input$deletePressed, '\\d+'))
        row_n <- grep(input$deletePressed,.rv$data$remove)
        .rv$data <- dplyr::filter(.rv$data, !row_number() %in% row_n)
      })
      
      # shinyFeedback -----------------------------------------------------------
      
      observeEvent(input$time_input, {
        shinyFeedback::feedbackDanger(
          inputId = "time_input",
          show = is.na(input$time_input),
          text = sprintf("Numeric value between %s-%s required", 0, 730))
        
        if (!is.na(input$time_input)) {
          in_range <- (input$time_input <= 730 && input$time_input >= 0) 
          shinyFeedback::feedbackDanger(
            inputId = "time_input",
            show = !in_range,
            text = sprintf("Numeric value between %s-%s required", 0, 730))
        }
      })
      
      observeEvent(input$dose_input, {
        shinyFeedback::feedbackDanger(
          inputId = "dose_input",
          show = is.na(input$dose_input),
          text = sprintf("Numeric value between %s-%s required", 0, 1500))
        
        if (!is.na(input$dose_input)) {
          in_range <- (input$dose_input <= 1500 && input$dose_input >= 0) 
          shinyFeedback::feedbackDanger(
            inputId = "dose_input",
            show = !in_range,
            text = sprintf("Numeric value between %s-%s required", 0, 1500))
        }
      })
      
      observeEvent(input$cmt_input, {
        shinyFeedback::feedbackDanger(
          inputId = "cmt_input",
          show = is.na(input$cmt_input),
          text = sprintf("Numeric value between %s-%s required", 0, 5))
        
        if (!is.na(input$cmt_input)) {
          in_range <- (input$cmt_input <= 5 && input$cmt_input >= 0) 
          shinyFeedback::feedbackDanger(
            inputId = "cmt_input",
            show = !in_range,
            text = sprintf("Numeric value between %s-%s required", 0, 5))
        }
      })
      
      observeEvent(input$addl_input, {
        shinyFeedback::feedbackDanger(
          inputId = "addl_input",
          show = is.na(input$addl_input),
          text = sprintf("Numeric value between %s-%s required", 0, 279))
        
        if (!is.na(input$addl_input)) {
          in_range <- (input$addl_input <= 279 && input$addl_input >= 0) 
          shinyFeedback::feedbackDanger(
            inputId = "addl_input",
            show = !in_range,
            text = sprintf("Numeric value between %s-%s required", 0, 279))
        }
      })
      
      observeEvent(input$ii_input, {
        shinyFeedback::feedbackDanger(
          inputId = "ii_input",
          show = is.na(input$ii_input),
          text = sprintf("Numeric value between %s-%s required", 0, 279))
        
        if (!is.na(input$ii_input)) {
          in_range <- (input$ii_input <= 279 && input$ii_input >= 0) 
          shinyFeedback::feedbackDanger(
            inputId = "ii_input",
            show = !in_range,
            text = sprintf("Numeric value between %s-%s required", 0, 279))
        }
      })
      
      
      
      
      # Save table --------------------------------------------------------------
      
      
      output$save_input_table <- downloadHandler(
        filename = function() {
          paste("data-", Sys.Date(), ".csv", sep="")
        },
        content = function(file) {
          output_data <- select(.rv$data, -remove)
          write.csv(output_data, file)
        }
      )
      
      
      return(
        list(
          data = reactive({.rv$data}),
          input_ready = reactive({input_ready()}),
          plot_time = reactive(input$plot_time)
        )
      )
      
    }
  )}
