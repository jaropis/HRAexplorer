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
                    selectInput(inputId = ns("time_unit"),
                                label = "Select time unit",
                                choices = c("minute", "second")
                    ),
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
           ),
           fluidRow(
             box(width = 4,
                        h3("pnnX"),
                        shiny::textInput(ns("pnnX_th"), "Thresholds for pnnX", glob_pnnX)),
             box(width = 4,
                 h3("Advanced topics"),
                 selectInput(ns("shuffle"),
                             "Shuffle the recordings?",
                             choices = list("Globally" = "Yes", "In Windows" = "window", "No" = "No"),
                             selected = "No"),
                 shiny::numericInput(ns("tolerance"), "Window length tolerance", glob_tolerance))
                    )
      )
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
  rval_pnnX_th <- reactiveVal(c(30, 50))
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

  observeEvent(c(input$files, input$separator), {
    raw_read_one_file(input$files %||% calculate_data_addresses(), file_no = 1, glob_separators[[input$separator]]) %>%
      sample_table() %>%
      rval_current_sample_data()
  }, ignoreInit = FALSE, ignoreNULL = FALSE)


  observeEvent(c(input$files, input$separator), {
    # disabling spectral analysis for long recordings
    req(input$files)
    lengths <- c()
    for (file_idx in seq_along(input$files[[1]])) {
      lengths <- c(lengths,
                   raw_read_one_file(input$files %||% calculate_data_addresses(), file_no = file_idx, glob_separators[[input$separator]]) %>%
                     nrow()
      )}
    if(any(lengths > 6000)) {
      shinyjs::runjs("document.querySelectorAll(\"a[href='#shiny-tab-spectral']\")[0].style.pointerEvents = 'none'")
    } else {
      shinyjs::runjs("document.querySelectorAll(\"a[href='#shiny-tab-spectral']\")[0].style.pointerEvents = 'auto'")
    }
  }, ignoreInit = FALSE, ignoreNULL = FALSE)

  observeEvent(input$files, {
    updateTextInput(session,inputId = "data_columns", value = "")
    updateSelectizeInput(session, "sinus", selected = "")
    updateSelectizeInput(session, "ventricular", selected = "")
    updateSelectizeInput(session, "supraventricular", selected = "")
    updateSelectizeInput(session, "artefact", selected = "")
    updateSelectizeInput(session, "separator", selected = "")
    updateSelectizeInput(session, "dynamic_asym", selected = "")
    updateSelectizeInput(session, "window_type", selected = "jump")
    updateSelectizeInput(session, "move_type", selected = "time")
    updateSelectizeInput(session, "time_unit", selected = "minute")
    updateNumericInput(session, "window_length", value = 5)
    updateRadioButtons(session, "use_ULF", selected = "No")
    updateSelectInput(session, "shuffle", selected = "No")
    updateNumericInput(session, "tolerance", value = glob_tolerance)
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

  observeEvent(input$data_columns, {
    req(isTruthy(input$data_columns))
    rval_data_cols_reset(TRUE)
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

  observeEvent(input$separator, {
    req(!isTruthy(input$data_columns))
    rval_data_cols_reset(FALSE)
  })

  observeEvent(c(input$files, input$data_columns), {
    req(rval_data_cols_reset())
    req(input$data_columns)
    data_columns <- as.numeric(strsplit(input$data_columns, "[ ]+")[[1]])
    req(all(data_columns %in% seq(ncol(rval_current_sample_data()))))
    if (length(data_columns) == 1) {
      rval_beat_choices("no choices")
    } else {
      rval_beat_choices(collect_unique_flags(input$files, input$data_columns, glob_separators[[input$separator]]))
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
      rval_flags_coding(list(sinus = input$sinus,
                        ventricular = input$ventricular,
                        supraventricular = input$supraventricular,
                        artefact = input$artefact))
    }
  })

  output$sample_data <- DT::renderDT({
    req(rval_current_sample_data())
    rval_current_sample_data()
  }, options = list(dom = 'tr'))

  output$confirm_choices <- renderText({
    if (isTruthy(rval_data_ready())) {
      "<div style=\"display: flex;\"><h3><font color=\"#228B22\"><b>Choices confirmed</b></font></h3><div style=\"width:100px\"></div><a href=\"https://www.youtube.com/watch?v=P8r2nqKeq2I/\" style=\"height: 100%;\" target=\"_blank\"><img src = \"img/course_icon2_small.png\" style=\"height: 100%;margin-top: 17px;\"></a></div>"
    } else {
      "<div style=\"display: flex;\"><h3><font color=\"#cc0000\"><b>Confirm choices</b></font></h3><div style=\"width:100px\"></div><a href=\"https://www.youtube.com/watch?v=P8r2nqKeq2I/\" style=\"height: 100%;\" target=\"_blank\"><img src = \"img/course_icon2_small.png\" style=\"height: 100%;margin-top: 17px;\"></a></div>"
    }
  })

  observeEvent(c(input$move_type, input$time_unit), {
    if (input$move_type == 'time') {
      updateNumericInput(session,
                        "window_length",
                        label = 'if'(input$time_unit == "minute", "Window length in minutes", "Window length in seconds"),
                        value = 'if'(input$time_unit == "minute", 5, 300))
      shinyjs::enable('time_unit')
    } else {
      updateNumericInput(session,
                        "window_length",
                        label = "Window length in heart beats",
                        value = 300)
      shinyjs::disable('time_unit')
    }
  }, ignoreInit = TRUE)

  # checking if what is entered makes sense
  observeEvent(input$confirm, {
    columns <- strsplit(input$data_columns, "[ ]+")[[1]]
    minmax <- strsplit(input$minmax, "[ ]+")[[1]]
    if (isTruthy(rval_flags_coding()) &
        length(columns) >= 1 &
        all(columns %in% seq(ncol(rval_current_sample_data()))) &
        length(minmax) == 2 &
        as.numeric(minmax[1]) < as.numeric(minmax[2]) &
        !is.na(as.numeric(input$window_length)) &
        !is.na(as.numeric(input$tolerance))) {
      rval_data_ready(TRUE)
    }
  })


  observeEvent(input$pnnX_th, {
    input_string <- input$pnnX_th %>%
      sub(",", "", .)
    sapply(strsplit(input_string, split = "\\s+")[[1]], function(x) as.numeric(x)) %>%
      unname %>%
      rval_pnnX_th()
  })
  observeEvent(c(rval_flags_coding(),
                 input$minmax,
                 input$data_columns,
                 input$files,
                 input$window_length,
                 input$dynamic_asym,
                 input$window_type,
                 input$move_type,
                 input$window_length,
                 input$use_ULF,
                 input$shuffle,
                 input$tolerance,
                 input$color,
                 input$variable_name,
                 input$pnnX_th), {
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
    time_unit = reactive(input$time_unit),
    move_type = reactive(input$move_type),
    window_length = reactive(input$window_length),
    dynamic_asym = reactive(input$dynamic_asym),
    flags_coding = rval_flags_coding, # this is reactive itself
    data_ready = rval_data_ready,
    shuffle = reactive(input$shuffle),
    tolerance = reactive(input$tolerance),
    pnnX_th = rval_pnnX_th
  )
}
