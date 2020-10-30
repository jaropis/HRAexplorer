options(shiny.maxRequestSize=300*1024^2)
shinyServer(function(input, output, session){

  # now reactive conductor holding the results of Poincare plot calculations
  rval_current_file_pp <- reactiveVal(NULL)
  rval_current_file_runs <- reactiveVal(NULL)
  rval_current_file_spectral <- reactiveVal(NULL)
  rval_current_file_quality <- reactiveVal(NULL)

  rct_current_pp_values <- reactive({
    req(data_info$data_ready())
    get_numerical_results(analysis_type = "poincare",
                          data_info$files(),
                          separator = data_info$separator(),
                          column_data = data_info$data_columns(),
                          minmax = data_info$minmax(),
                          using_excel = data_info$using_excel(),
                          flags_coding = data_info$flags_coding()
    )
  })

  rct_current_runs_values <- reactive({
    req(data_info$data_ready())
    get_numerical_results(analysis_type = "runs",
                          data_info$files(),
                          separator = data_info$separator(),
                          column_data = data_info$data_columns(),
                          minmax = data_info$minmax(),
                          using_excel = data_info$using_excel(),
                          flags_coding = data_info$flags_coding()
    )
  })

  rct_current_spectral_values <- reactive({
    req(data_info$data_ready())
    get_numerical_results(analysis_type = "spectral",
                          data_info$files(),
                          separator = data_info$separator(),
                          column_data = data_info$data_columns(),
                          minmax = data_info$minmax(),
                          using_excel = data_info$using_excel(),
                          use_ULF = data_info$use_ULF(),
                          flags_coding = data_info$flags_coding()
    )
  })
  rct_current_quality_values <- reactive({
    req(data_info$data_ready())
    get_numerical_results(analysis_type = "quality",
                          data_info$files(),
                          separator = data_info$separator(),
                          column_data = data_info$data_columns(),
                          minmax = data_info$minmax(),
                          using_excel = data_info$using_excel(),
                          flags_coding = data_info$flags_coding()
    )
  })

  rct_current_dynamic_pp_results <- reactive({
    req(data_info$data_ready())
    get_dynamic_numerical_results(analysis_type = "poincare_dynamic",
                                  data_info$files(),
                                  separator = data_info$separator(),
                                  column_data = data_info$data_columns(),
                                  minmax = data_info$minmax(),
                                  using_excel = data_info$using_excel(),
                                  window_type = data_info$window_type(),
                                  move_type = data_info$move_type(),
                                  window_length = data_info$window_length(),
                                  asym_comparisons = data_info$dynamic_asym(),
                                  flags_coding = data_info$flags_coding())
  })
  rct_current_single_dynamic_pp <- reactive({
    req(data_info$data_ready())
    req(isTruthy(input$dynamicpp))
    get_dynamic_numerical_results(analysis_type = "poincare_dynamic",
                                  data_info$files(),
                                  separator = data_info$separator(),
                                  column_data = data_info$data_columns(),
                                  minmax = data_info$minmax(),
                                  using_excel = data_info$using_excel(),
                                  window_type = data_info$window_type(),
                                  move_type = data_info$move_type(),
                                  window_length = data_info$window_length(),
                                  clicked_file = as.numeric(input$dynamicpp),
                                  flags_coding = data_info$flags_coding())
    })

  rct_current_dynamic_runs_results <- reactive({
    req(data_info$data_ready())
    get_dynamic_numerical_results(analysis_type = "runs_dynamic",
                                  data_info$files(),
                                  separator = data_info$separator(),
                                  column_data = data_info$data_columns(),
                                  minmax = data_info$minmax(),
                                  using_excel = data_info$using_excel(),
                                  window_type = data_info$window_type(),
                                  move_type = data_info$move_type(),
                                  window_length = data_info$window_length(),
                                  asym_comparisons = data_info$dynamic_asym(),
                                  flags_coding = data_info$flags_coding())
  })

  rct_current_single_dynamic_runs_results <- reactive({
    req(data_info$data_ready())
    req(isTruthy(input$dynamicruns))
    get_dynamic_numerical_results(analysis_type = "runs_dynamic",
                                  data_info$files(),
                                  separator = data_info$separator(),
                                  column_data = data_info$data_columns(),
                                  minmax = data_info$minmax(),
                                  using_excel = data_info$using_excel(),
                                  window_type = data_info$window_type(),
                                  move_type = data_info$move_type(),
                                  window_length = data_info$window_length(),
                                  clicked_file = as.numeric(input$dynamicruns),
                                  flags_coding = data_info$flags_coding())
  })

  rct_current_dynamic_spectral_results <- reactive({
    req(data_info$data_ready())
    get_dynamic_numerical_results(analysis_type = "spectral_dynamic",
                                  data_info$files(),
                                  separator = data_info$separator(),
                                  column_data = data_info$data_columns(),
                                  minmax = data_info$minmax(),
                                  using_excel = data_info$using_excel(),
                                  window_type = data_info$window_type(),
                                  move_type = data_info$move_type(),
                                  window_length = data_info$window_length(),
                                  flags_coding = data_info$flags_coding(),
                                  use_ULF = data_info$use_ULF())

  })

  rct_current_single_dynamic_spectral_results <- reactive({
    req(data_info$data_ready())
    req(input$dynamicspectral)
    get_dynamic_numerical_results(analysis_type = "spectral_dynamic",
                                  data_info$files(),
                                  separator = data_info$separator(),
                                  column_data = data_info$data_columns(),
                                  minmax = data_info$minmax(),
                                  using_excel = data_info$using_excel(),
                                  window_type = data_info$window_type(),
                                  move_type = data_info$move_type(),
                                  window_length = data_info$window_length(),
                                  clicked_file = as.numeric(input$dynamicspectral),
                                  flags_coding = data_info$flags_coding(),
                                  use_ULF = data_info$use_ULF())
  })

  rct_current_dynamic_quality_results <- reactive({
    req(data_info$data_ready())
    get_dynamic_numerical_results(analysis_type = "quality_dynamic",
                                  data_info$files(),
                                  separator = data_info$separator(),
                                  column_data = data_info$data_columns(),
                                  minmax = data_info$minmax(),
                                  using_excel = data_info$using_excel(),
                                  window_type = data_info$window_type(),
                                  move_type = data_info$move_type(),
                                  window_length = data_info$window_length(),
                                  flags_coding = data_info$flags_coding())
  })

  rct_current_single_dynamic_quality_results <- reactive({
    req(input$dynamicquality)
    req(data_info$data_ready())
    get_dynamic_numerical_results(analysis_type = "quality_dynamic",
                                  data_info$files(),
                                  separator = data_info$separator(),
                                  column_data = data_info$data_columns(),
                                  minmax = data_info$minmax(),
                                  using_excel = data_info$using_excel(),
                                  window_type = data_info$window_type(),
                                  move_type = data_info$move_type(),
                                  window_length = data_info$window_length(),
                                  clicked_file = as.numeric(input$dynamicquality),
                                  flags_coding = data_info$flags_coding())
  })

  data_info <- callModule(data_upload_and_filter,
                          "get-filter-data")

  # Handling download files names for single file analysis and making the overleaf visible
  observeEvent(input$dynamicpp, {
    rval_current_file_pp({
      if(is.null(input$dynamicpp)) {
        "nic.xlsx"
      } else {
        paste0("SingleFileDynamicPP",
               data_info$files()$name[[as.numeric(input$dynamicpp)]],
               ".xlsx")
      }
    })
  })

  observeEvent(input$dynamicruns, {
    rval_current_file_runs({
      if(is.null(input$dynamicruns)) {
        "nic.xlsx"
      } else {
        paste0("SingleFiledynamicruns",
               data_info$files()$name[[as.numeric(input$dynamicruns)]],
               ".xlsx")
      }
    })
  })

  observeEvent(input$dynamicspectral, {
    rval_current_file_spectral({
      if(is.null(input$dynamicspectral)) {
        "nic.xlsx"
      } else {
        paste0("SingleFiledynamicspectral",
               data_info$files()$name[[as.numeric(input$dynamicspectral)]],
               ".xlsx")
      }
    })
  })

  observeEvent(input$dynamicquality, {
    rval_current_file_quality({
      if(is.null(input$dynamicquality)) {
        "nic.xlsx"
      } else {
        paste0("SingleFiledynamicquality",
               data_info$files()$name[[as.numeric(input$dynamicquality)]],
               ".xlsx")
      }
    })
  })
  # adding listeners to the front and back-buttons (related to removing mirror tables from flipbox)
  shinyjs::runjs("waitForEl('#btn-5-front', add_show_to_button, ['details-table-pp-flip-container'])")
  shinyjs::runjs("waitForEl('#btn-6-front', add_show_to_button, ['details-table-runs-flip-container'])")
  shinyjs::runjs("waitForEl('#btn-7-front', add_show_to_button, ['details-table-spectral-flip-container'])")
  shinyjs::runjs("waitForEl('#btn-8-front', add_show_to_button, ['details-table-quality-flip-container'])")
  shinyjs::runjs("waitForEl('#btn-5-back', add_hide_to_button, ['details-table-pp-flip-container'])")
  shinyjs::runjs("waitForEl('#btn-6-back', add_hide_to_button, ['details-table-runs-flip-container'])")
  shinyjs::runjs("waitForEl('#btn-7-back', add_hide_to_button, ['details-table-spectral-flip-container'])")
  shinyjs::runjs("waitForEl('#btn-8-back', add_hide_to_button, ['details-table-quality-flip-container'])")

  callModule(plots,
             "plots",
             type_of_plot = "poincare",
             data_address = data_info$files(),
             rct_line_number = reactive(input$staticpp), # listen for clicks on the main table (View buttons)
             separator = data_info$separator(),
             data_columns = data_info$data_columns(),
             minmax = data_info$minmax(),
             using_excel = data_info$using_excel(),
             variable_name = data_info$variable_name(),
             color = data_info$color(),
             flags_coding = data_info$flags_coding()
  )

  # call results for a single file on the plot page
  callModule(single_results,
             "single-results",
             type_of_plot = "poincare",
             rct_line_number = reactive(input$staticpp), # triggering here - as above
             rct_current_pp_values = rct_current_pp_values
  )

  callModule(main_table,
             "main-table",
             rct_current_values = rct_current_pp_values,
             button_label = "View",
             button_id = "btn_view_staticpp_",
             file_name =reactiveVal("PoincrePlot.xlsx")

  )
  callModule(main_table,
             "main-table-runs",
             rct_current_values = rct_current_runs_values,
             file_name =reactiveVal("Runs.xlsx")
  )
  callModule(main_table,
             "main-table-quality",
             rct_current_values = rct_current_quality_values,
             file_name =reactiveVal("Quality.xlsx")
  )
  callModule(main_table,
             "main-table-spectral",
             rct_current_values = rct_current_spectral_values,
             file_name =reactiveVal("Spectral.xlsx")
  )
  callModule(main_table,
             "main-table-dynamic",
             rct_current_values = rct_current_dynamic_pp_results,
             button_label = "Detail",
             button_id = "btn_view_dynamicpp_",
             file_name =reactiveVal("DynamicPP.xlsx")
  )
  callModule(main_table,
             "details-table-pp",
             rct_current_values = rct_current_single_dynamic_pp,
             file_name =rval_current_file_pp
  )
  callModule(main_table,
             "main-table-runs-dynamic",
             rct_current_values = rct_current_dynamic_runs_results,
             button_label = "Detail",
             button_id = "btn_view_dynamicruns_",
             file_name =reactiveVal("RunsDynamic.xlsx")
  )
  callModule(main_table,
             "details-table-runs",
             rct_current_values = rct_current_single_dynamic_runs_results,
             file_name =rval_current_file_runs
  )
  callModule(main_table,
             "main-table-spectral-dynamic",
             rct_current_values = rct_current_dynamic_spectral_results,
             button_label = "Detail",
             button_id = "btn_view_dynamicspectral_",
             file_name =reactiveVal("SpectralDynamic.xlsx")
  )
  callModule(main_table,
             'details-table-spectral',
             rct_current_values = rct_current_single_dynamic_spectral_results,
             file_name =rval_current_file_spectral
  )
  callModule(main_table,
             "main-table-quality-dynamic",
             rct_current_values = rct_current_dynamic_quality_results,
             button_label = "Detail",
             button_id = "btn_view_dynamicquality_",
             file_name =reactive("QualityDynamic.xlsx")
  )
  callModule(main_table,
             'details-table-quality',
             rct_current_values = rct_current_single_dynamic_quality_results,
             file_name =rval_current_file_quality
  )
})
