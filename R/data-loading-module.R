# data load module
#' function to read numbers from input windows
#'
#' @param list_argument string holding the numbers keyed into the input field
#'
#' @return numeric vector holding the values
#'
#' @export
read_numbers_from_field <- function(list_argument){
  list_of_names_and_numbers <- paste(strsplit(list_argument, ",")[[1]], collapse=".")
  list_of_names_and_numbers <- strsplit(list_of_names_and_numbers," ")[[1]]
  name <- ""
  my_length <- length(list_of_names_and_numbers)
  for (element in 1:my_length){
    if (is.na(as.numeric(list_of_names_and_numbers[element])))
      name <- paste(name, list_of_names_and_numbers[element])
    else break
  }
  list_of_numbers <- list_of_names_and_numbers[element:my_length]
  myData <- vapply(list_of_numbers, as.numeric, FUN.VALUE = numeric(1))
  return(myData)
}

#' @export
read_and_filter_one_file <- function(fileAddresses, lineNumber, separator, column_data, minmax, usingExcel){
  dataFile <- fileAddresses$datapath[lineNumber]
  javaerror <- FALSE; csverror <- FALSE # these show whether the function should return "some_problem" and exit
  if (usingExcel){
    if (dataFile=="../initial_data/RR.csv") dataFile="../initial_data/RR.xlsx" # just making sure that the XLConnect does not crash on text
    tryCatch(
      wb <- XLConnect::loadWorkbook(dataFile),
      error = function(e) javaerror <<- TRUE # if java fails, "some_problem" will be returned and it should be handled in reactive plot
    )
    if (javaerror) return(data.frame("some_problem"))
    data <- XLConnect::readWorksheet(wb, sheet = 1) # this will never happen if java fails
  } else {
    data <- read.csv(dataFile, sep = separator, header = T, row.names=NULL)
  }
  column_idx <- read_numbers_from_field(column_data)
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
  minmax <- read_numbers_from_field(minmax)
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
