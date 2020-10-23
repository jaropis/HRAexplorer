data_upload_and_filterUI <- function(id) {
  ns <- NS(id)
  fluidRow(id = "first_row",
           fluidRow(
             box(width = 4,
                    h3("Files to upload"),
                    fileInput(ns("files"),
                              label="Select files to upload",
                              multiple=TRUE),
                    selectInput(ns("separator"), "select separator",
                                list(glob_init_separator, ",", ";", "space", "\t")),
                    textInput(ns("data_columns"), "enter the column for RR intervals and flags - see explanations",
                              glob_init_columns),
                 actionButton(inputId = ns('preview'), label = "Preview")
             ),
             box(width = 4,
                    h3("Filters"),
                    textInput(ns("minmax"),"minimum and maximum RR length",
                              glob_init_min_max_sinus),
                 selectizeInput(inputId = ns("sinus"),
                             label = "Sinus beat flag",
                             choices = c(0),
                             selected = c(0),
                             multiple = TRUE),
                 selectizeInput(inputId = ns("ventricular"),
                             label = "Ventricular beat flag",
                             choices = c(1),
                             selected = c(1),
                             multiple = TRUE),
                 selectizeInput(inputId = ns("supraventricular"),
                             label = "Supraventricular beat flag",
                             choices = c(2),
                             selected = c(2),
                             multiple = TRUE),
                 selectizeInput(inputId = ns("artefact"),
                             label = "Artefact flag",
                             choices = c(3),
                             selected = c(3),
                             multiple = TRUE)
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
  beat_choices <- reactiveVal(NULL)
  flags_coding <- reactiveVal(NULL)

  dataModal <- function() {
    raw_read_one_file(input$files %||% calculate_data_addresses(), file_no = 1, glob_separators[[input$separator]]) %>%
      sample_table() %>%
      current_sample_data()

    modalDialog(size = "l",
      tags$div(id = "table_div",
        DT::dataTableOutput(ns('sample_data'))
      ),
      footer = tagList(
        modalButton("Done")
      )
    )
  }

  observeEvent(input$preview, {
   showModal(dataModal())
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

  observeEvent(input$files, {
    req(input$data_columns)
    if (length(strsplit(input$data_columns, " ")[[1]]) == 1) {
      c(0)
    } else {
      collect_unique_flags(input$files, input$data_columns, input$separator)
    } %>%
      beat_choices()
    updateSelectizeInput(session, "sinus", choices = beat_choices())
    updateSelectizeInput(session, "ventricular", choices = beat_choices())
    updateSelectizeInput(session, "supraventricular", choices = beat_choices())
    updateSelectizeInput(session, "artefact", choices = beat_choices())
  })

  observeEvent(c(input$sinus, input$ventricular, input$supraventricular, input$artefact), {
    if (!is.null(beat_choices()) && all(beat_choices() %in% c(input$sinus, input$ventricular, input$supraventricular, input$artefact)) ||
        is.null(beat_choices()) && identical(c(input$sinus, input$ventricular, input$supraventricular, input$artefact), c("0", "1", "2", "3"))) { # the latter happens at the beginning
      flags_coding(list(sinus = as.numeric(input$sinus),
                        ventricular = as.numeric(input$ventricular),
                        supraventricular = as.numeric(input$supraventricular),
                        artefact = as.numeric(input$artefact)))
    }
  })

  output$sample_data <- DT::renderDT({
    req(current_sample_data())
    current_sample_data()
  }, options = list(dom = 'tr'))

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
    using_excel = reactive(check_for_excel(input$files %||% calculate_data_addresses())),
    files = reactive(input$files %||% calculate_data_addresses()),
    separator = reactive(get_sep(input$separator)),
    data_columns = reactive(input$data_columns),
    minmax = reactive(input$minmax),
    color = reactive(input$color),
    use_ULF = reactive(input$use_ULF),
    window_type = reactive(input$window_type),
    move_type = reactive(input$move_type),
    window_length = reactive(input$window_length),
    dynamic_asym = reactive(input$dynamic_asym),
    flags_coding = flags_coding # this is reactive itself
  )
}
