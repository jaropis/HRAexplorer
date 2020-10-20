shinyServer(function(input, output, session){

  # now reactive conductor holding the results of Poincare plot calculations

  rct_current_pp_values <- reactive({
    get_numerical_results(analysis_type = "poincare",
                          data_info$files(),
                          separator = data_info$separator(),
                          column_data = data_info$data_columns(),
                          minmax = data_info$minmax(),
                          using_excel = data_info$using_excel()
    )
  })

  rct_current_runs_values <- reactive({
    get_numerical_results(analysis_type = "runs",
                          data_info$files(),
                          separator = data_info$separator(),
                          column_data = data_info$data_columns(),
                          minmax = data_info$minmax(),
                          using_excel = data_info$using_excel()
    )
  })

  rct_current_spectral_values <- reactive({
    get_numerical_results(analysis_type = "spectral",
                          data_info$files(),
                          separator = data_info$separator(),
                          column_data = data_info$data_columns(),
                          minmax = data_info$minmax(),
                          using_excel = data_info$using_excel(),
                          use_ULF = data_info$use_ULF()
    )
  })
  rct_current_quality_values <- reactive({
    get_numerical_results(analysis_type = "quality",
                          data_info$files(),
                          separator = data_info$separator(),
                          column_data = data_info$data_columns(),
                          minmax = data_info$minmax(),
                          using_excel = data_info$using_excel()
    )
  })

  rct_current_dynamic_pp_results <- reactive({
    get_dynamic_numerical_results(analysis_type = "poincare_dynamic",
                                  data_info$files(),
                                  separator = data_info$separator(),
                                  column_data = data_info$data_columns(),
                                  minmax = data_info$minmax(),
                                  using_excel = data_info$using_excel(),
                                  window_type = data_info$window_type(),
                                  move_type = data_info$move_type(),
                                  window_length = data_info$window_length())
  })

  rct_current_dynamic_runs_results <- reactive({
    get_dynamic_numerical_results(analysis_type = "runs_dynamic",
                                  data_info$files(),
                                  separator = data_info$separator(),
                                  column_data = data_info$data_columns(),
                                  minmax = data_info$minmax(),
                                  using_excel = data_info$using_excel(),
                                  window_type = data_info$window_type(),
                                  move_type = data_info$move_type(),
                                  window_length = data_info$window_length())
  })

  rct_current_dynamic_spectral_results <- reactive({
    get_dynamic_numerical_results(analysis_type = "spectral_dynamic",
                                  data_info$files(),
                                  separator = data_info$separator(),
                                  column_data = data_info$data_columns(),
                                  minmax = data_info$minmax(),
                                  using_excel = data_info$using_excel(),
                                  window_type = data_info$window_type(),
                                  move_type = data_info$move_type(),
                                  window_length = data_info$window_length())
  })

  rct_current_dynamic_quality_results <- reactive({
    get_dynamic_numerical_results(analysis_type = "quality_dynamic",
                                  data_info$files(),
                                  separator = data_info$separator(),
                                  column_data = data_info$data_columns(),
                                  minmax = data_info$minmax(),
                                  using_excel = data_info$using_excel(),
                                  window_type = data_info$window_type(),
                                  move_type = data_info$move_type(),
                                  window_length = data_info$window_length())
  })

  data_info <- callModule(data_upload_and_filter,
                          "get-filter-data")

  callModule(plots,
             "plots",
             type_of_plot = "poincare",
             data_address = data_info$files(),
             rct_line_number = reactive(input$foo), # listen for clicks on the main table (View buttons)
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
             rct_line_number = reactive(input$foo), # triggering here - as above
             rct_current_pp_values = rct_current_pp_values
  )

  callModule(main_table,
             "main-table",
             rct_current_values = rct_current_pp_values,
             button_label = "View",
             button_id = "btn_view_staticpp_"
  )
  callModule(main_table,
             "main-table-runs",
             rct_current_values = rct_current_runs_values
  )
  callModule(main_table,
             "main-table-quality",
             rct_current_values = rct_current_quality_values
  )
  callModule(main_table,
             "main-table-spectral",
             rct_current_values = rct_current_spectral_values
  )
  callModule(main_table,
             "main-table-dynamic",
             rct_current_values = rct_current_dynamic_pp_results,
             button_label = "Detail",
             button_id = "btn_view_dynamicpp_"
  )
  callModule(main_table,
             "main-table-runs-dynamic",
             rct_current_values = rct_current_dynamic_runs_results,
             button_label = "Detail",
             button_id = "btn_view_dynamicruns_"
  )
  callModule(main_table,
             "main-table-spectral-dynamic",
             rct_current_values = rct_current_dynamic_spectral_results,
             button_label = "Detail",
             button_id = "btn_view_dynamicspectral_"
  )
  callModule(main_table,
             "main-table-quality-dynamic",
             rct_current_values = rct_current_dynamic_quality_results,
             button_label = "Detail",
             button_id = "btn_view_dynamicquality_"
  )
})
