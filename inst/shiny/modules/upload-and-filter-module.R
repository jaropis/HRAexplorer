data_upload_and_filterUI <- function(id) {
  ns <- NS(id)
  actionButton(ns("get_filters"),
               "Get/filter data",
               class = "btn-success",
               style="margin:0 auto;margin-top:90px;display:block;")
}

#' module with data upload and filtering
#'
#' no input
#' @return inputs corresponding to upload and filtering
#' @export
data_upload_and_filter <- function(input, output, session) {
  ns <- session$ns

  observeEvent(input$get_filters, {
    filtering_modal <- function() {
      showModal(
        modalDialog(
          size = 'l',
          fade = TRUE,
          #upload_filterUI(ns("upload_filterUI"))
          tagList(
            fluidRow(
              column(4,
                     textInput(ns("variable_name"),"variable name",
                               get_from_storage(session, "var_name") %||% glob_init_var_name),
                     checkboxInput(ns("using_excel"), "using Excel",
                                   value = get_from_storage(session, "excel") %||% glob_init_excel),
                     fileInput(ns("files"),
                               label="load files in the correct format - see the information on the right",
                               multiple=TRUE)
              ),
              column(4,
                     selectInput(ns("separator"), "select separator",
                                 list(get_from_storage(session, "separator") %||% glob_init_separator, ",", ";", "space", "\t")),
                     textInput(ns("data_columns"), "enter the column for RR intervals and flags - see explanations",
                               get_from_storage(session, "data_columns") %||% glob_init_columns),
                     textInput(ns("minmax"),"enter minimum and maximum acceptable RR length",
                               get_from_storage(session, "min_max_sinus") %||% glob_init_min_max_sinus)
              ),
              column(4,
                     selectInput(ns("color"), "select color from the list below", glob_color_list,
                                 selected = get_from_storage(session, "color") %||% glob_init_color)
              )
            )
          ),
          footer = tagList(
            modalButton("Cancel"),
            actionButton(ns("go"),
                         "Go",
                         icon("refresh"),
                         onclick = "document.querySelectorAll('[data-dismiss]')[0].click();",
                         class = 'btn-primary')
          )
        )

      )
    }
    filtering_modal()
  })
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
