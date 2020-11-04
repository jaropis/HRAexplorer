#' main results table depending on the analysis type
main_tableUI <- function(id) {
  ns <- NS(id)
  fluidRow(
    box(width = 12,
        tagList(
          textOutput(ns("title")),
          div(id = ns('flip-container'),
              DT::dataTableOutput(ns("main_table")),
              downloadButton(ns("downloadResults"), 'Download results as Excel file'),
              shinyjs::hidden(downloadButton(ns("downloadAllDynamic"), "Download results for each file"))
          )
        )
    )
  )
}

#' module for displaying main table
#' @param rct_current_values the current values related to poincare plot (to be changed for other types of output)
#'
#' @return DT
main_table <- function(input, output, session,
                       rct_current_values,
                       button_label = "View",
                       button_id = NULL,
                       file_name,
                       dynamic = FALSE) {
  output$title <- renderText(file_name())
  if (dynamic) {
    shinyjs::show("downloadAllDynamic")
  }
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
    filename = function() {
      file_name()
    },
    content = function(file) {
      openxlsx::write.xlsx(x = rct_current_values(), file = file)
    })
}
