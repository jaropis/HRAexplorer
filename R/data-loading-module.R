# data load module
options( java.parameters = c("-Xss2560k", "-Xmx2g"))

readNumbersFromField <- function(listArgument){
  listOfNamesAndNumbers <- paste(strsplit(listArgument, ",")[[1]], collapse=".")
  listOfNamesAndNumbers <- strsplit(listOfNamesAndNumbers," ")[[1]]
  nazwa <- ""
  myLengthosc <- length(listOfNamesAndNumbers)
  for (element in 1:myLengthosc){
    if (is.na(as.numeric(listOfNamesAndNumbers[element]))) nazwa <- paste(nazwa, listOfNamesAndNumbers[element])
    else break
  }
  listOfNumbers <- listOfNamesAndNumbers[element:myLengthosc]
  myData <- as.vector(sapply(listOfNumbers, as.numeric))
  return(myData)
}

read_and_filter_one_file <- function(fileAddresses, lineNumber, separator, column_data, minmax, usingExcel){
  dataFile <- fileAddresses$datapath[lineNumber]
  javaerror <- FALSE; csverror <- FALSE # these show whether the function should return "some_problem" and exit
  if (usingExcel){
    if (dataFile=="../initial_data/RR.csv") dataFile="../initial_data/RR.xlsx" # just making sure that the XLConnect does not crash on text
    tryCatch(
      wb <- loadWorkbook(dataFile),
      error = function(e) javaerror <<- TRUE # if java fails, "some_problem" will be returned and it should be handled in reactive plot
    )
    if (javaerror) return(data.frame("some_problem"))
    data <- readWorksheet(wb, sheet = 1) # this will never happen if java fails
  } else {
    #browser()
    data <- read.csv(dataFile, sep = separator, header = T, row.names=NULL)
  }
  column_idx <- readNumbersFromField(column_data)
  RR_idx <- column_idx[1]
  flag_idx <- ifelse(length(column_idx)>1, column_idx[2], 0)
  tryCatch( # this will go wrong if the wrong type of file is selected
    RR <- data[[RR_idx]],
    error = function(e) csverror <<- TRUE  # i just return some_problem and it should be handled in the reactive plot
  )
  if (csverror) return(data.frame("some_problem"))
  if (flag_idx>0){
    tryCatch( # now, an error can also happen if there is a flags column, but it does not correspond to an actual column flag
      flags <- data[[flag_idx]],
      error = function(e) csverror <<- TRUE
    )
    if (csverror) return(data.frame("some_problem")) # if this happens, return some_problem
  }
  else
    flags <- RR*0
  # time based filtering here
  minmax <- readNumbersFromField(minmax)
  which_min <- RR <= minmax[1]
  which_max <- RR >= minmax[2]
  flags[which_min] <- 2
  flags[which_max] <- 3
  return(list(RR=RR, annotations=flags))
}

getSep <- function(separator){
  sep = separator
  if (separator == "tabulator"){
    sep = "\t"
  }
  if (separator == "space"){
    sep = " "
  }
  return(sep)
}

getSep <- function(separator){
  sep = separator
  if (separator == "tabulator"){
    sep = "\t"
  }
  if (separator == "space"){
    sep = " "
  }
  return(sep)
}
