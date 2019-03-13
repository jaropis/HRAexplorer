shinyServer(function(input, output){

  data_info <- list(
    data_addresses = reactive({NULL}),
    line_number = reactive({1}),
    variable_name = reactive(NULL),
    using_excel = reactive(NULL),
    files = reactive(NULL),
    separator = reactive(NULL),
    data_columns = reactive(NULL),
    minmax = reactive(NULL),
    color = reactive(NULL)
  )

  data_info <- callModule(data_upload_and_filter,
                          "get-filter-data")

  rct_data_address <- reactive({

    if (!is.null(data_info$data_addresses())){
      return(data_info$data_addresses())
    }
    pattern <- paste0("../initial_data/*", glob_init_file_extension)
    initial_files <- Sys.glob(pattern)
    names <- unname(
      vapply(initial_files, function(x) strsplit(x, "/")[[1]][3], FUN.VALUE = c("a"))
    )
    types <- rep("text/plain", length(names))
    dataPaths <- data.frame(name = names, size = 0,
                            type = types,
                            datapath = initial_files,
                            stringsAsFactors = FALSE)
    return(dataPaths)
  })

  # listen for clicks on the main table (View buttons)
  observeEvent(input$foo,{
    # call plotting module
    callModule(plots,
               "plots",
               type_of_plot = "poincare",
               data_address = rct_data_address(),
               line_number = as.numeric(input$foo %||% glob_init_line_number),
               separator = getSep(state_RR_settings$separator %||% glob_init_separator),
               data_columns = state_RR_settings$data_columns %||% glob_init_columns,
               minmax = state_RR_settings$min_max_sinus %||% glob_init_min_max_sinus,
               using_excel = state_RR_settings$excel %||% glob_init_excel,
               variable_name = state_RR_settings$var_name %||% glob_init_var_name,
               color = state_figures$color %||% glob_init_color
    )
  })

  # now reactive conductor holding the results of Poincare plot calculations

  rct_current_pp_values <- reactive({
    #todo - what about errors!
    force(data_info$go()) # react to the go button in the modal
    returnTable <- getPpResults(isolate(rct_data_address()),
                                separator = getSep(isolate(data_info$separator()) %||% glob_init_separator),
                                column_data = isolate(data_info$data_columns()) %||% glob_init_columns,
                                minmax = isolate(data_info$minmax()) %||% glob_init_min_max_sinus,
                                using_excel = isolate(data_info$using_excel()) %||% glob_init_excel)
  })

  callModule(main_table,
             "main-table",
             rct_current_pp_values = rct_current_pp_values
  )
  ### end of server below
})
