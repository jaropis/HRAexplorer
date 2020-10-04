#' module to show results for a single recording on the flip side
#' @export
single_resultsUI <- function(id) {
  ns <- NS(id)
  uiOutput(ns("single_table"))
}


single_results <- function(input, output, session,
                           type_of_plot,
                           rct_line_number,
                           rct_current_pp_values) {
  observe({
    req(rct_line_number())
    data_line <- rct_current_pp_values()[rct_line_number(), ]
    html_description <- description_string(data_line, type_of_plot)
    output$single_table <- renderUI(HTML(html_description))
  })
}
