data_upload_and_filterUI <- function(id) {
  ns <- NS(id)
  fluidRow(
    column(4,
           h3("Files to upload"),
           fileInput(ns("files"),
                     label="Select files to upload",
                     multiple=TRUE),
           checkboxInput(ns("using_excel"), "using Excel",
                         value = glob_init_excel),
           selectInput(ns("separator"), "select separator",
                       list(glob_init_separator, ",", ";", "space", "\t")),
           textInput(ns("data_columns"), "enter the column for RR intervals and flags - see explanations",
                     glob_init_columns),
    ),
    column(4,
           h3("Filters"),
           textInput(ns("minmax"),"minimum and maximum RR length",
                     glob_init_min_max_sinus)
    ),
    column(4,
           h3("Output format"),
           textInput(ns("variable_name"),"variable name",
                     value = glob_init_var_name),
           selectInput(ns("color"), "select color from the list below", glob_color_list,
                       selected = glob_init_color)
    )#,

#    actionButton(ns("go"),
#                 "Go",
#                 icon("refresh"),
#                 onclick = "document.querySelectorAll('[data-dismiss]')[0].click();",
#                 class = 'btn-primary')
  )
}

#' module with data upload and filtering
#'
#' no input
#' @return inputs corresponding to upload and filtering
#' @export
data_upload_and_filter <- function(input, output, session) {
  list(
    variable_name = reactive(input$variable_name),
    using_excel = reactive(input$using_excel),
    files = reactive(input$files %||% calculate_data_addresses()),
    separator = reactive(get_sep(input$separator)),
    data_columns = reactive(input$data_columns),
    minmax = reactive(input$minmax),
    color = reactive(input$color),
    go = reactive(input$go)
  )
}
