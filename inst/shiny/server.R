shinyServer(function(input, output, session){

  data_info <- callModule(data_upload_and_filter,
                          "get-filter-data")
  # listen for clicks on the main table (View buttons)
  observeEvent(c(input$foo), {
    callModule(plots,
               "plots",
               type_of_plot = "poincare",
               data_address = data_info$files(),
               line_number = as.integer(input$foo),
               separator = data_info$separator(),
               data_columns = data_info$data_columns(),
               minmax = data_info$minmax(),
               using_excel = data_info$using_excel(),
               variable_name = data_info$variable_name(),
               color = data_info$color()
    )

    # call results for a single file on the plot page
    callModule(single_results,
               "single-results",
               type_of_plot = "poincare",
               line_number = as.integer(input$foo), # triggering here
               rct_current_pp_values = rct_current_pp_values
    )
  }, ignoreInit = TRUE)
  # now reactive conductor holding the results of Poincare plot calculations

  rct_current_pp_values <- reactive({
    # TODO add runs and spectral here!
    returnTable <- get_numerical_results(analysis_type = "poincare",
                                          data_info$files(),
                                          separator = data_info$separator(),
                                          column_data = data_info$data_columns(),
                                          minmax = data_info$minmax(),
                                          using_excel = data_info$using_excel()
    )
  })

  rct_current_runs_values <- reactive({
    # TODO add runs and spectral here!
    returnTable <- get_numerical_results(analysis_type = "runs",
                                         data_info$files(),
                                         separator = data_info$separator(),
                                         column_data = data_info$data_columns(),
                                         minmax = data_info$minmax(),
                                         using_excel = data_info$using_excel()
    )
  })

  callModule(main_table,
             "main-table",
             rct_current_values = rct_current_pp_values
  )
  callModule(main_table,
             "main-table-runs",
             rct_current_values = rct_current_runs_values
  )
})
