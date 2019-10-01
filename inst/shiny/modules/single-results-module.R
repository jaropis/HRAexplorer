#' module to show results for a single recording on the flip side
#' @export
single_resultsUI <- function(id) {
  ns <- NS(id)
  uiOutput(ns("single_table"))
}


single_results <- function(input, output, session,
                           type_of_plot,
                           line_number,
                           rct_current_pp_values) {
  data_line <- rct_current_pp_values()[line_number, ]
  html_description <- description_string(data_line, type_of_plot)
  output$single_table <- renderUI(HTML(html_description))
}
