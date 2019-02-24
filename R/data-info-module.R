# data info module
data_infoUI <- function(id){
  ns <- NS(id)
  tagList(
    textInput(ns("variable_name"),"variable name", "RR"),
    checkboxInput(ns("using_excel"), "using Excel", value = FALSE),
    fileInput(ns("files"), label="load files in the correct format - see the information on the right", multiple=TRUE),
    selectInput(ns("separator"), "select separator",
                list("tabulator", ",", ";", "space")),
    textInput(ns("data_columns"), "enter the column for RR intervals and flags - see explanations", "1 2"),
    textInput(ns("minmax"),"enter minimum and maximum acceptable RR length", "0 3000"),
    selectInput(ns("color"), "select color from the list below", glob_color_list)
  )
}
#' module creating the data-info UI and returning its values - handy because it
#' can be put anywhere (i.e. in modal)
#'
#' @param input input shiny variable
#' @param output output shiny variable
#' @param session input shiny variable
#'
#' @return input reactive values
data_info <- function(input, output, session) {

  return(
    list(
      variable_name = reactive(input$variable_name),
      using_excel = reactive(input$using_excel),
      files = reactive(input$files),
      separator = reactive(input$separator),
      data_columns = reactive(input$data_columns),
      minmax = reactive(input$minmax),
      color = reactive(input$color)
    )
  )
}
