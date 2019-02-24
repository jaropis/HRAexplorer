shinyServer(function(input, output){

  data_info <- callModule(data_info, id = "data-info")

  dataAddress <- reactive({
    dataPaths <- data.frame(name = c("RR.csv"), size = 0,
                            type = c("text/plain"),
                            datapath = c("../initial_data/RR.csv"),
                            stringsAsFactors = FALSE)
    if (!is.null(data_info$files())){
      dataPaths <- data_info$files()
    }
    return(dataPaths)
  })

  output$plot <- renderPlot({
    errorOnRead <- FALSE
    rr_and_flags <- read_and_filter_one_file(dataAddress(), 1,
                                             separator=getSep(data_info$separator()),
                                             data_info$data_columns(),
                                             data_info$minmax(),
                                             data_info$using_excel())
    # todo - what about errors??
          hrvhra::drawpp(rr_and_flags$RR, rr_and_flags$annotations,
                     vname = ifelse(data_info$variable_name() == "", "RR", data_info$variable_name()),
                     col = "black", bg = data_info$color(), pch = 21)

  })

  # now reactive conductor holding the results of Poincare plot calculations

  currentPPvalues <- reactive({
      #todo - what about errors!
      returnTable <- getPpResults(dataAddress(),
                                  sep = getSep(data_info$separator()),
                                  data_info$data_columns(),
                                  data_info$minmax(),
                                  data_info$using_excel())
      })

  output$filesView <- renderTable({
    return(currentPPvalues())
  }, include.rownames = FALSE)

  output$myDataView <- renderTable({
    X <- data_info$variableName
    myTable <- data.frame(myData()[[1]], transformData()$data)
    colnames(myTable) <- c(data_info$variableName, "transformation")
    myTable
  })

  output$downloadPlot <- downloadHandler(
    filename = "PoincarePlot.png",
    content = function(file) {
      rr_and_flags <- read_and_filter_one_file(dataAddress(), 1,
                                               separator=getSep(data_info$separator),
                                               data_info$data_columns,
                                               data_info$minmax, data_info$usingExcel)
      png(file, width=1800, height = 1900, res=300)
      plotInput(rr_and_flags[[1]], rr_and_flags[[2]], data_info$color, data_info$variableName)
      dev.off()
    })

  output$downloadResults <- downloadHandler(
    filename = "PPResults.xlsx",
    content = function(file) {
      writeWorksheetToFile( file = file, data=currentPPvalues(), sheet="Poincare plot")
    })
  ### end of server below
})
