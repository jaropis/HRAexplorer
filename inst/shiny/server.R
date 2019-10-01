shinyServer(function(input, output){

  data_info <- callModule(data_upload_and_filter,
                          "get-filter-data")
  # listen for clicks on the main table (View buttons)
  observeEvent(c(input$foo, data_info()),{
    # call plotting module
    req(is.null(data_info()) || data_info()) # draw at the beginning, then require specific value (GO clicked)
    callModule(plots,
               "plots",
               type_of_plot = "poincare",
               data_address = state_RR_settings$data_addresses,
               line_number = as.numeric(input$foo %||% glob_init_line_number), # triggering here
               separator = getSep(state_RR_settings$separator %||% glob_init_separator),
               data_columns = state_RR_settings$data_columns %||% glob_init_columns,
               minmax = state_RR_settings$min_max_sinus %||% glob_init_min_max_sinus,
               using_excel = state_RR_settings$excel %||% glob_init_excel,
               variable_name = state_RR_settings$var_name %||% glob_init_var_name,
               color = state_figures$color %||% glob_init_color
    )
  }, ignoreInit = TRUE)

  observeEvent(c(input$foo, data_info()),{
    # call results for a single file on the plot page
    req(is.null(data_info()) || data_info()) #
    callModule(single_results,
               "single-results",
               type_of_plot = "poincare",
               line_number = as.numeric(input$foo %||% glob_init_line_number), # triggering here
               rct_current_pp_values = rct_current_pp_values
    )
  }, ignoreInit = TRUE)
  # now reactive conductor holding the results of Poincare plot calculations

  rct_current_pp_values <- reactive({
    # TODO add runs and spectral here!
    #todo - what about errors!
    req(is.null(data_info()) || data_info() != 0) # so that it does not recalculate when something else than pressing Go is done in the modal
    force(data_info()) # react to the go button in the modal
    returnTable <- get_numerical_results(analysis_type = "poincare",
                                          state_RR_settings$data_addresses %||% {state_RR_settings$data_addresses <- calculate_data_addresses()},
                                          separator = getSep(state_RR_settings$separator %||% {state_RR_settings$separator <- glob_init_separator}),
                                          column_data = state_RR_settings$data_columns %||% {state_RR_settings$data_columns <- glob_init_columns},
                                          minmax = state_RR_settings$min_max_sinus %||% {state_RR_settings$min_max_sinus <- glob_init_min_max_sinus},
                                          using_excel = state_RR_settings$excel %||% {state_RR_settings$excel <- glob_init_excel}
    )
  })

  callModule(main_table,
             "main-table",
             rct_current_values = rct_current_pp_values
  )
  ### end of server below
})
