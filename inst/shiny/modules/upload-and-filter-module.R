data_upload_and_filterUI <- function(id) {
  ns <- NS(id)
    fluidRow(
      column(4,
             textInput(ns("variable_name"),"variable name",
                       value = glob_init_var_name),
             checkboxInput(ns("using_excel"), "using Excel",
                           value = glob_init_excel),
             fileInput(ns("files"),
                       label="load files in the correct format - see the information on the right",
                       multiple=TRUE)
      ),
      column(4,
             selectInput(ns("separator"), "select separator",
                         list(glob_init_separator, ",", ";", "space", "\t")),
             textInput(ns("data_columns"), "enter the column for RR intervals and flags - see explanations",
                       glob_init_columns),
             textInput(ns("minmax"),"enter minimum and maximum acceptable RR length",
                       glob_init_min_max_sinus)
      ),
      column(4,
             selectInput(ns("color"), "select color from the list below", glob_color_list,
                         selected = glob_init_color)
      ),
      tagList(
        modalButton("Cancel"),
        actionButton(ns("go"),
                     "Go",
                     icon("refresh"),
                     onclick = "document.querySelectorAll('[data-dismiss]')[0].click();",
                     class = 'btn-primary')
      ))
  }

#' module with data upload and filtering
#'
#' no input
#' @return inputs corresponding to upload and filtering
#' @export
data_upload_and_filter <- function(input, output, session) {
  reactive({ # save current filter values here
    store_in_storage(session, "var_name", input$variable_name)
    store_in_storage(session, "excel",  input$using_excel)
    store_in_storage(session, "data_addresses", input$files %||% get_from_storage(session, "data_addresses"))
    store_in_storage(session, "separator", input$separator)
    store_in_storage(session, "data_columns", isolate(input$data_columns))
    store_in_storage(session, "min_max_sinus", input$minmax)
    store_in_storage(session, "figure_color", input$color)
    input$go
  })
}
