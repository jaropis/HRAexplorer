data_upload_and_filterUI <- function(id) {
  ns <- NS(id)
  fluidRow(id = "first_row",
           fluidRow(
             box(width = 4,
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
                 actionButton(inputId = ns('preview'), label = "Preview")
             ),
             box(width = 4,
                    h3("Filters"),
                    textInput(ns("minmax"),"minimum and maximum RR length",
                              glob_init_min_max_sinus)
             ),
             box(width = 4,
                    h3("Output format"),
                    textInput(ns("variable_name"),"variable name",
                              value = glob_init_var_name),
                    selectInput(ns("color"), "select color from the list below", glob_color_list,
                                selected = glob_init_color)
             )),
           fluidRow(
             box(width = 4,
                    h3("Spectral Analysis"),
                    radioButtons(ns("use_ULF"),
                                 "Calculate ULF?",
                                 choices = c("Yes", "No"),
                                 selected = "No",
                                 inline = TRUE)),
             box(width = 4,
                    h3("Windowing options"),
                    selectInput(inputId = ns("window_type"),
                                label = "Window movement type",
                                choices = list(jumping = "jump",
                                               sliding = "slide"),
                                selected = "jump"),
                    selectInput(inputId = ns("move_type"),
                                label = "Window time basis",
                                choices = list("time based" = "time",
                                               "index based" = "index"),
                                selected = c("time based")),
                    numericInput(inputId = ns("window_length"),
                                 label = "Window length in minutes",
                                 value = 5,
                                 min = 0)),
             box(width = 4,
                    h3("Dynamic asymmetry"),
                    selectizeInput(inputId = ns('dynamic_asym'),
                                   label = "Select comparisons\nfor dynamic asymmetry",
                                   choices = glob_dynamic_asymmetry_vars,
                                   selected = "",
                                   multiple = TRUE))
           ))
}

#' module with data upload and filtering
#'
#' no input
#' @return inputs corresponding to upload and filtering
#' @export
data_upload_and_filter <- function(input, output, session) {
  ns <- session$ns
  current_sample_data <- reactiveVal(NULL)
  dataModal <- function() {
    if (!input$using_excel) {
      current_sample_data(read.csv(calculate_data_addresses()[1, c("datapath")], sep = glob_separators[[input$separator]])[1:3, ])
    }
    modalDialog(size = "l",
      DT::dataTableOutput(ns('sample_data')),
      footer = tagList(
        modalButton("Done")
      )
    )
  }

  observeEvent(input$preview, {
   showModal(dataModal())
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

  output$sample_data <- DT::renderDT({
    req(current_sample_data())
    current_sample_data()
  })

  observeEvent(input$move_type, {
    if (input$move_type == 'time') {
      updateNumericInput(session,
                        "window_length",
                        label = "Window length in minutes",
                        value = 5)
    } else {
      updateNumericInput(session,
                        "window_length",
                        label = "Window length in heart beats",
                        value = 300)
    }
  }, ignoreInit = TRUE)
  list(
    variable_name = reactive(input$variable_name),
    using_excel = reactive(input$using_excel),
    files = reactive(input$files %||% calculate_data_addresses()),
    separator = reactive(get_sep(input$separator)),
    data_columns = reactive(input$data_columns),
    minmax = reactive(input$minmax),
    color = reactive(input$color),
    use_ULF = reactive(input$use_ULF),
    window_type = reactive(input$window_type),
    move_type = reactive(input$move_type),
    window_length = reactive(input$window_length),
    dynamic_asym = reactive(input$dynamic_asym)
  )
}
