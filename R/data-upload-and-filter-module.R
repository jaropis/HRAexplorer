#' module with data upload and filtering
#'
#' no input
#' @return inputs corresponding to upload and filtering

data_upload_and_filter <- function(input, output, session) {
  ns <- session$ns
  filtering_modal <- function() {
  showModal(
    modalDialog(
      #upload_filterUI(ns("upload_filterUI"))
      tagList(textInput(ns("variable_name"),"variable name", glob_init_var_name),
              checkboxInput(ns("using_excel"), "using Excel", glob_init_excel),
              fileInput(ns('data_addresses'), label="load files in the correct format - see the information on the right", multiple=TRUE),
              selectInput(ns("separator"), "select separator",
                          list(glob_init_separator, ",", ";", "space")),
              textInput(ns("data_columns"), "enter the column for RR intervals and flags - see explanations", glob_init_columns),
              textInput(ns("minmax"),"enter minimum and maximum acceptable RR length", glob_init_min_max_sinus),
              selectInput(ns("color"), "select color from the list below",
                          glob_color_list)),
      footer = tagList(
        modalButton("Cancel"),
        actionButton(ns("ok"), "OK")
      )
    )
  )
  }
  observeEvent(input$data_button, {
    filtering_modal()
  })

  observeEvent(input$ok, {
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
  })
}
