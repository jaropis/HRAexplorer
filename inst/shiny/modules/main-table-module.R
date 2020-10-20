#' main results table depending on the analysis type
main_tableUI <- function(id) {
  ns <- NS(id)
  tagList(
    DT::dataTableOutput(ns("main_table")),
    downloadButton(ns("downloadResults"), 'Download results as Excel file')
  )
}

#' module for displaying main table
#' @param rct_current_values the current values related to poincare plot (to be changed for other types of output)
#'
#' @return DT
main_table <- function(input, output, session,
                       rct_current_values, button_label = "View", button_id = NULL) {
  main_DTable <- reactive({
    results_matrix <- HRAexplorer::get_results_matrix(rct_current_values(), button_label, button_id)
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
      XLConnect::writeWorksheetToFile( file = file, data=rct_current_values(), sheet="Poincare plot")
    })
}
