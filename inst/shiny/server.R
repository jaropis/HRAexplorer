shinyServer(function(input, output){

  data_info <- callModule(data_upload_and_filter,
                          "get-filter-data")

  # listen for clicks on the main table (View buttons)
  observeEvent(input$foo,{
    # call plotting module
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

  # now reactive conductor holding the results of Poincare plot calculations

  rct_current_pp_values <- reactive({
    #todo - what about errors!
    force(data_info$go()) # react to the go button in the modal
    returnTable <- getPpResults(state_RR_settings$data_addresses %||% {state_RR_settings$data_addresses <- calculate_data_addresses()},
                                separator = getSep(state_RR_settings$separator %||% {state_RR_settings$separator <- glob_init_separator}),
                                column_data = state_RR_settings$data_columns %||% {state_RR_settings$data_columns <- glob_init_columns},
                                minmax = state_RR_settings$min_max_sinus %||% {state_RR_settings$min_max_sinus <- glob_init_min_max_sinus},
                                using_excel = state_RR_settings$excel %||% {state_RR_settings$excel <- glob_init_excel}
    )
  })

  callModule(main_table,
             "main-table",
             rct_current_pp_values = rct_current_pp_values
  )
  ### end of server below
})
