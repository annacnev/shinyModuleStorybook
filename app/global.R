suppressPackageStartupMessages({
  library(collapsibleTree)
  library(shiny)
  library(shinyWidgets)
  library(shinydashboard)
  library(shinydashboardPlus)
  library(shinyFeedback)
  library(tools)
  library(logrrr)
  library(fs)
  library(rlang)
  library(tidyverse)
  #library(shinytest)
})

# Run shinytest::installDependencies() for shinytest to work

# source global functions
functions <- list.files("functions")
for(function.i in functions){
  source(file.path("functions", function.i))
}

# source modules and module functions (to keep modules organized)
modules <- list.files("modules", recursive = T)
for(module.i in modules){
  if(file_ext(module.i)=="R"){
    source(file.path("modules", module.i))
  }else{
    next
  }
}

# source scripts
scripts <- list.files("script")
for(script.i in scripts){
  if(file_ext(script.i)=="Rmd"){
    next
  }else{
    source(file.path("script", script.i))
  }
}

# Setup - logrrr ----------------------------------------------------------

cfg <- config::get(file = "app-config.yml", use_parent = FALSE)

whoami <- function(session) {
  if (!is.null(session$user)) {
    return(session$user)
  } 
  return("UNKNOWN")
}

if (cfg$is_production) {
  lgr <- Logrrr$new(log_level = "DEBUG",
                    output =  LogOutput$new())
} else {
  if (file_exists("debug-log.txt")) {
    file_delete("debug-log.txt") 
  }
  lgr <- Logrrr$new(log_level = "TRACE",
                    output = list(text = LogOutput$new(),
                                  file = LogOutput$new(format_func = JSONFormatter(), output = "debug-log.txt")))
}

lgr$with_fields(config = attributes(cfg)$config)$info("configuration")



