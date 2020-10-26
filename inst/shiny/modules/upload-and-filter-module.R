data_upload_and_filterUI <- function(id) {
  ns <- NS(id)
  fluidRow(id = "first_row",
           fluidRow(
             box(width = 12, id = "choices_box",
                 htmlOutput(ns("confirm_choices"))),
             box(width = 4,
                    h3("Files to upload"),
                    fileInput(ns("files"),
                              label="Select files to upload",
                              multiple=TRUE),
                    selectInput(ns("separator"), "select separator",
                                list(glob_init_separator, ",", ";", "space", "\t")),
                    textInput(ns("data_columns"), "enter the column for RR intervals and flags - see explanations",
                              glob_init_columns),
                 div(id = "preview_confirm",
                 actionButton(inputId = ns('preview'), label = "Preview"),
                 div(class = 'div_between'),
                 actionButton(inputId = ns('confirm'), label = "Confirm", class = 'btn-primary')
                 )
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
  rval_current_sample_data <- reactiveVal(NULL)
  rval_beat_choices <- reactiveVal(NULL)
  rval_flags_coding <- reactiveVal(NULL)
  rval_data_cols_reset <- reactiveVal(FALSE)
  rval_data_ready <- reactiveVal(FALSE)

  dataModal <- function() {
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
    raw_read_one_file(input$files %||% calculate_data_addresses(), file_no = 1, glob_separators[[input$separator]]) %>%
      sample_table() %>%
      rval_current_sample_data()
  }, ignoreInit = FALSE, ignoreNULL = FALSE)

  observeEvent(input$files, {
    updateTextInput(session,inputId = "data_columns", value = "")
    updateSelectizeInput(session, "sinus", selected = "")
    updateSelectizeInput(session, "ventricular", selected = "")
    updateSelectizeInput(session, "supraventricular", selected = "")
    updateSelectizeInput(session, "artefact", selected = "")
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

  observeEvent(input$data_columns, {
    req(isTruthy(input$data_columns))
    rval_data_cols_reset(TRUE)
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

  observeEvent(c(input$files, input$data_columns), {
    req(rval_data_cols_reset())
    req(input$data_columns)
    data_columns <- as.numeric(strsplit(input$data_columns, "[ ]+")[[1]])
    req(all(data_columns %in% seq(ncol(rval_current_sample_data()))))
    if (length(data_columns) == 1) {
      rval_beat_choices("no choices")
    } else {
      rval_beat_choices(collect_unique_flags(input$files, input$data_columns, input$separator))
    }
    rval_flags_coding(NULL)
    updateSelectizeInput(session, "sinus", choices = 'if' (length(data_columns) == 1, "no choices", rval_beat_choices()), selected = "no choices")
    updateSelectizeInput(session, "ventricular", choices ='if' (length(data_columns) == 1, "no choices", rval_beat_choices()), selected = "no choices")
    updateSelectizeInput(session, "supraventricular", choices = 'if' (length(data_columns) == 1, "no choices", rval_beat_choices()), selected = "no choices")
    updateSelectizeInput(session, "artefact", choices = 'if' (length(data_columns) == 1, "no choices", rval_beat_choices()), selected = "no choices")
    updateTextInput(session, "data_columns", value = NULL)
  })

  observeEvent(c(input$sinus, input$ventricular, input$supraventricular, input$artefact), {
    req(input$data_columns)
    if (!is.null(rval_beat_choices()) &&
        (all(rval_beat_choices() %in% c(input$sinus, input$ventricular, input$supraventricular, input$artefact)) || rval_beat_choices() == "no choices")  ||
        is.null(rval_beat_choices()) && identical(c(input$sinus, input$ventricular, input$supraventricular, input$artefact), c("0", "1", "2", "3"))) { # the latter happens at the beginning
      rval_flags_coding(list(sinus = as.numeric(input$sinus),
                        ventricular = as.numeric(input$ventricular),
                        supraventricular = as.numeric(input$supraventricular),
                        artefact = as.numeric(input$artefact)))
    }
  })

  output$sample_data <- DT::renderDT({
    req(rval_current_sample_data())
    rval_current_sample_data()
  }, options = list(dom = 'tr'))

  output$confirm_choices <- renderText({
    if (isTruthy(rval_data_ready())) {
      "<h3><font color=\"#228B22\"><b>Choices confirmed</b></font></h3>"
    } else {
      "<h3><font color=\"#cc0000\"><b>Confirm choices</b></font></h3>"
    }
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

  # checking if what is enetered makes sense
  observeEvent(input$confirm, {
    columns <- strsplit(input$data_columns, "[ ]+")[[1]]
    minmax <- strsplit(input$minmax, "[ ]+")[[1]]
    if (isTruthy(rval_flags_coding()) &
        length(columns) >= 1 &
        all(columns %in% seq(ncol(rval_current_sample_data()))) &
        length(minmax) == 2 &
        as.numeric(minmax[1]) < as.numeric(minmax[2])) {
      rval_data_ready(TRUE)
    }
  })

  observeEvent(c(rval_flags_coding(), input$minmax, input$data_columns, input$files), {
    rval_data_ready(FALSE)
  })
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
    flags_coding = rval_flags_coding, # this is reactive itself
    data_ready = rval_data_ready
  )
}
