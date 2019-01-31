shinyServer(function(input, output){
  dataAddress <- reactive({
    dataPaths <- data.frame(name = c("RR.csv"), size = 0, type = c("text/plain"), datapath = c("./RR.csv"), stringsAsFactors = FALSE)
    if (!is.null(input$files)){
      dataPaths <- input$files
    }
    return(dataPaths)
  })

  output$plot <- renderPlot({
    errorOnRead <- FALSE
    rr_and_flags <- read_and_filter_one_file(dataAddress(), 1, separator=getSep(input$separator), input$data_columns, input$minmax, input$usingExcel)
    tryCatch(
      tempPP <- preparePP(rr_and_flags[[1]], rr_and_flags[[2]]),
      error = function(e) errorOnRead <<-  TRUE
    )
    tryCatch(
      drawPP(tempPP, variableName = ifelse(input$variableName=="", "RR", input$variableName), color = input$color),
      error = function(e)   errorOnRead <<- TRUE
    )
    if (errorOnRead){
      plot(1:10, 1:10, col = "white")
      text(6,7, "Fail - incorrect format", cex = 1.5)
      text(6,5, "try another file type,", cex = 1.5)
      text(6,3, "column selection, or separator", cex = 1.5)
    } else {
      drawPP(tempPP, variableName = ifelse(input$variableName=="", "RR", input$variableName), color = input$color)
    }
  })

  # now reactive conductor holding the results of Poincare plot calculations

  currentPPvalues <- reactive({
    tryCatch(
      returnTable <- getPpResults(dataAddress(), sep = getSep(input$separator), input$data_columns, input$minmax, input$usingExcel),
      error = function(e) returnTable <<- NA
    )
    if (is.na(returnTable[1])) return(data.frame(Info = "FAIL - incorrect format - try choosing another file type, column selection or separator"))
    else return(returnTable)})

  output$filesView <- renderTable({
    return(currentPPvalues())
  }, include.rownames = FALSE)

  output$myDataView <- renderTable({
    X <- input$variableName
    myTable <- data.frame(myData()[[1]], transformData()$data)
    colnames(myTable) <- c(input$variableName, "transformation")
    myTable
  })

  output$downloadPlot <- downloadHandler(
    filename = "PoincarePlot.png",
    content = function(file) {
      rr_and_flags <- read_and_filter_one_file(dataAddress(), 1, separator=getSep(input$separator), input$data_columns, input$minmax, input$usingExcel)
      png(file, width=1800, height = 1900, res=300)
      plotInput(rr_and_flags[[1]], rr_and_flags[[2]], input$color, input$variableName)
      dev.off()
    })

  output$downloadResults <- downloadHandler(
    filename = "PPResults.xlsx",
    content = function(file) {
      writeWorksheetToFile( file = file, data=currentPPvalues(), sheet="Poincare plot")
    })

  ### end of server below
})
