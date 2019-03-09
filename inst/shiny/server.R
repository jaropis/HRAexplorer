shinyServer(function(input, output){

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
  line_chosen <- reactive({
    if (!is.null(input$foo))
      as.numeric(input$foo)
    else
      NULL
  })

  # call plotting module
  callModule(plots,
             "plots",
             type_of_plot = "poincare",
             data_address = rct_data_address(),
             line_number = line_chosen() %||% glob_init_line_number,
             separator = getSep(data_info$separator() %||% glob_init_separator),
             data_columns = data_info$data_columns() %||% glob_init_columns,
             minmax = data_info$minmax() %||% glob_init_min_max_sinus,
             using_excel = data_info$using_excel() %||% glob_init_excel,
             variable_name = data_info$variable_name() %||% glob_init_var_name,
             color = data_info$color() %||% glob_init_color
  )

  # now reactive conductor holding the results of Poincare plot calculations

  rct_current_pp_values <- reactive({
    #todo - what about errors!
    returnTable <- getPpResults(rct_data_address(),
                                separator = getSep(data_info$separator() %||% glob_init_separator),
                                column_data = data_info$data_columns() %||% glob_init_columns,
                                minmax = data_info$minmax() %||% glob_init_min_max_sinus,
                                using_excel = data_info$using_excel() %||% glob_init_excel)
  })

  callModule(main_table,
             "main-table",
             rct_current_pp_values = rct_current_pp_values
  )

  ### end of server below
})
