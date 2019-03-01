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
  main_DTable <- reactive({
    results_matrix <- as.data.frame(rct_current_pp_values())

      buttons_row <- paste0('
               <button type="button" class="btn btn-primary" "id=view_',1:nrow(results_matrix),'>View PP</button>
             ')
      results_matrix <- data.frame(file = results_matrix[, 1, drop = FALSE],
                              "view" = buttons_row,
                              results_matrix[, 2:ncol(results_matrix)])
    main_table <- DT::datatable(results_matrix,
                                options = list(scrollX = '150px'),
                                escape = FALSE,
                                rownames = FALSE,
                                selection = 'none')
    main_table
  })

  output$main_table <- DT::renderDataTable({
    main_DTable()
  })

  output$downloadResults <- downloadHandler(
    filename = "PPResults.xlsx",
    content = function(file) {
      XLConnect::writeWorksheetToFile( file = file, data=rct_current_pp_values(), sheet="Poincare plot")
    })

}
