shinyServer(function(input, output){

  data_info <- callModule(data_info, id = "data-info")

  rct_data_address <- reactive({
    dataPaths <- data.frame(name = c("RR.csv"), size = 0,
                            type = c("text/plain"),
                            datapath = c("../initial_data/RR.csv"),
                            stringsAsFactors = FALSE)
    if (!is.null(data_info$files())){
      dataPaths <- data_info$files()
    }
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
             rct_data_address = rct_data_address,
             line_number = line_chosen,
             inp_separator = data_info$separator,
             inp_data_columns = data_info$data_columns,
             inp_minmax = data_info$minmax,
             inp_using_excel = data_info$using_excel,
             inp_variable_name = data_info$variable_name,
             inp_color = data_info$color
  )

  # now reactive conductor holding the results of Poincare plot calculations

  rct_current_pp_values <- reactive({
    #todo - what about errors!
    returnTable <- getPpResults(rct_data_address(),
                                separator = getSep(data_info$separator() %||% 'tabulator'),
                                column_data = data_info$data_columns() %||% "1 2",
                                minmax = data_info$minmax() %||% "0 3000",
                                using_excel = data_info$using_excel() %||% FALSE)
  })

  callModule(main_table,
             "main-table",
             rct_current_pp_values = rct_current_pp_values
  )
  ### end of server below
})
