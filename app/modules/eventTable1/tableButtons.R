
# Module UI Function
tableButtons_UI <- function(id, temp_df, add_tag, labels) {
  ns <- NS(id)
  buttons <- tagList()
  # return a list of tags
  for (i in 1:length(colnames(temp_df))){
    if (is.numeric(temp_df[[i]])){
      buttons[[i]] <- numericInput(ns(paste0(colnames(temp_df)[i],"_add",add_tag)), paste0(names(labels)[i],":"),
                                   min = 0, value = 0, step=1)
    }else{ 
      buttons[[i]] <- textInput(ns(paste0(colnames(temp_df)[i],"_add",add_tag)), paste0(names(labels)[i],":") )
    }
  }
  return(buttons)
}

# Module Server Function
tableButtons_Server <- function(id, temp_df, add_tag) {
  moduleServer(
    id, 
    function(input, output, session){
      
      ns <- session$ns
      
      newValues <- reactive({
        inputlist <- list()
        for (j in 1:length(colnames(temp_df))){
          val <- input[[paste0(colnames(temp_df)[j],"_add",add_tag)]]
          inputlist[j] <- val
        }
        inputlist
      })
      
      observeEvent(newValues(), {
        for (i in 1:length(colnames(temp_df))){
          id <- paste0(colnames(temp_df)[i],"_add",add_tag)
          if(is.na(input[[id]])){
            feedbackDanger(id,is.na(input[[id]]),
                           paste("Please Enter a value")
            )
          }else{
            if(grepl("time",id)){
              in_range <- if(input[[id]] <= 280 && input[[id]] >= 0) {
                TRUE
              } else {
                FALSE
              }
              feedbackDanger(id, !in_range,
                             sprintf("Numeric value between %s-%s required", 0, 280))
            }else if(grepl("diluentA",id)){
              in_range <- if(input[[id]] <= 1500 && input[[id]] >= 0) {
                TRUE
              } else {
                FALSE
              }
              feedbackDanger(id, !in_range,
                             sprintf("Numeric value between %s-%s required", 0, 1500))
            }else if(grepl("castorOil",id)){
              in_range <- if(input[[id]] <= 1500 && input[[id]] >= 0) {
                TRUE
              } else {
                FALSE
              }
              feedbackDanger(id, !in_range,
                             sprintf("Numeric value between %s-%s required", 0, 1500))
            }else if(grepl("addl",id)){
              in_range <- if(input[[id]] <= 279 && input[[id]] >= 0) {
                TRUE
              } else {
                FALSE
              }
              feedbackDanger(id, !in_range,
                             sprintf("Numeric value between %s-%s required", 0, 279))
            }else if(grepl("ii",id)){
              in_range <- if(input[[id]] <= 280 && input[[id]] >= 0) {
                TRUE
              } else {
                FALSE
              }
              feedbackDanger(id, !in_range,
                             sprintf("Numeric value between %s-%s required", 0, 280))
            }
          }
        }
      },ignoreInit=T)
      
      
      newRow <- reactive({
        shiny::req(newValues())
        df_matrix <- do.call(cbind.data.frame,newValues())
        newRow <- data.frame(df_matrix)
        colnames(newRow) <- colnames(temp_df)
        newRow
      })
      
      return(newRow)
      
    })
}

