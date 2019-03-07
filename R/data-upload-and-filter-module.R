#' module with data upload and filtering
#'
data_upload_and_filterUI <- function(id) {
  ns <- NS(id)
  actionButton(ns("data_button"), "Get/filter data")
}


#' module with data upload and filtering
#'
#' no input
#' @return inputs corresponding to upload and filtering

data_upload_and_filter <- function(input, output, session) {
  filtering_modal <- function() {
  showModal(
    modalDialog(
      #upload_filterUI(ns("upload_filterUI"))
      tagList(textInput("variable_name","variable name", "RR"),
              checkboxInput("using_excel", "using Excel", value = FALSE),
              fileInput('data_addresses', label="load files in the correct format - see the information on the right", multiple=TRUE),
              selectInput("separator", "select separator",
                          list("tabulator", ",", ";", "space")),
              textInput("data_columns", "enter the column for RR intervals and flags - see explanations", "1 2"),
              textInput("minmax","enter minimum and maximum acceptable RR length", "0 3000"),
              selectInput("color", "select color from the list below",
                          glob_color_list))
    )
  )
  }
  observeEvent(input$data_button, {
    filtering_modal()
  })
  return(
    list(
      variable_name = reactive(input$variable_name),
      using_excel = reactive(input$using_excel),
      data_addresses = reactive(input$data_addresses),
      separator = reactive(input$separator),
      data_columns = reactive(input$data_columns),
      minmax = reactive(input$minmax),
      color = reactive(input$color)
    )
  )
}
