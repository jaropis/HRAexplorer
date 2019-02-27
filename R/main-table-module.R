#' main results table depending on the analysis type
#'
main_tableUI <- function(id) {
  ns <- NS(id)
  tagList(
    DT::dataTableOutput(ns("main_table")),
    downloadButton(ns("downloadResults"), 'Download results as Excel file')
  )
}

#' module for displaying main table
#'
#' @param rct_current_pp_values the current values related to poincare plot (to be changed for other types of output)
#'
#' @return DT
#' @export
main_table <- function(input, output, session,
                       rct_current_pp_values) {

    output$main_table <- DT::renderDataTable({
    rct_current_pp_values()
  })

  output$downloadResults <- downloadHandler(
    filename = "PPResults.xlsx",
    content = function(file) {
      XLConnect::writeWorksheetToFile( file = file, data=rct_current_pp_values(), sheet="Poincare plot")
    })

}
