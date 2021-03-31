

source("global.R")

shinyApp(
  
  # UI ----------------------------------------------------------------------
  
  ui = dashboardPagePlus(skin = "green",
                         header = dashboardHeader(title = "Shiny Module Database",
                                                  titleWidth = "300px"),
                         sidebar = dashboardSidebar(width = "300px",
                                                    sidebarMenu(
                                                      menuItem("Dashboard", tabName = "dashboard",icon = icon("gears")),
                                                      menuItem("Shiny Module Example",tabName = "mod-example"),
                                                      menuItem("Simulation Modules", icon = icon("gears"),
                                                               menuSubItem("Event Table 1",tabName = "eventTable1"),
                                                               menuSubItem("Event Table 2",tabName = "eventTable2")
                                                      )
                                                    ),
                                                    hr(),
                                                    fluidRow(
                                                      column(8,offset=2,style="text-align:center",
                                                             actionBttn("exportResultsModal", "Export Module(s)", 
                                                                        icon = icon("download"), size = 'sm',color="success"))
                                                    ),
                                                    hr()
                         ),
                         body = dashboardBody(
                           tags$head(
                             tags$script(resizeScript), # Resizes apps to fit screen width
                             tags$script(HTML(linkToTabScript)),
                             tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")),
                           tabItems(
                             tabItem("dashboard", uiOutput("dashboard")),
                             tabItem("mod-example", htmlOutput("module_frame")),
                             tabItem("eventTable1", eventTable_UI("event_table1")),
                             tabItem("eventTable2", event_table_ui("event_table2"))
                           )
                         ),
                         title = "shinyDashboardPlus",
                         footer = dashboardFooter(
                           left_text = tagList(
                             h1("Shiny Module Database"),
                             h3("")
                           ),
                           right_text = tagList(img(src="MetrumLogo.png", height='68px', width='287px'))
                         )
  ),
  
  # Server ------------------------------------------------------------------
  
  server = function(input, output, session) {
    
    ### Dashboard Rmd ###
    output$dashboard <- renderUI({
      tagList(
        inclRmd("script/dashboard.Rmd"))
    })
    
    # Modules -----------------------------------------------------------------
    
    # Shiny Module Example from Rsconnect
    output$module_frame <- renderUI({
      shiny::req(input$dimension[1])
      app_link <- "https://i-003fe8b9cd192d9a1.metworx.com/rsconnect/shiny-module-example/"
      repo_link <- a("Shiny Module Example", href="https://ghe.metrumrg.com/tech-solutions/shiny-example")
      frame_width <- (input$dimension[1]-300)*0.95
      module_app <- tags$iframe(src=app_link, height=850, width=frame_width)
      tagList(
        h3("R Shiny Module Example"),
        tagList("Repository:",repo_link),
        HTML("<br><b>Note:</b> You must be logged into Rsconnect for the app to show below."),
        tags$br(),tags$br(),
        module_app)
    })
    
    # Event Table 1 -----------------------------------------------------------
    
    eventTable1 <- eventTable_Server("event_table1")
    
    # Event Table 2 -----------------------------------------------------------
    
    eventTable2 <- event_table_server('event_table2')
    
    
    # Download Module ---------------------------------------------------------
    
    observeEvent(input$exportResultsModal, {
      showModal(export_results_modal())
    })
    
    output$exportModules <- downloadHandler(
      filename = function() {
        paste("Modules-", Sys.Date(), ".zip", sep="")
      },
      content = function(file) {
        temp_dir <- tempdir()
        message(temp_dir)
        data_to_export <- lapply(1:length(input$modulesToExport),function(i){
          files <- list.files(file.path("modules",input$modulesToExport[i]),recursive = T)
          sub_folders <- dirname(files[grep("/",files)])
          fs::dir_create(file.path(temp_dir,"modules",input$modulesToExport[i]), recurse = TRUE)
          fs::dir_create(file.path(temp_dir,"modules",input$modulesToExport[i],sub_folders), recurse = TRUE)
          file.path(input$modulesToExport[i],files)
        }) %>% unlist()
        filesToZip <- c()
        for(file.i in data_to_export){
          this_file.i <- file.path(temp_dir,"modules" , file.i)
          if(!fs::file_exists(this_file.i)){
            fs::file_copy(file.path("modules",file.i), this_file.i)
          }
          filesToZip <- c(filesToZip, this_file.i)
          rm(this_file.i)
        }
        zip(zipfile = file, files = filesToZip, flags = "-r9Xj")
        
        for(file.i in filesToZip){
          file.remove(file.i)
        }
        removeModal()
      }
    )
    observeEvent(input$cancelExport, {
      removeModal()
    })
    
  } # End Server
)