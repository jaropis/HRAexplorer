# data load module
loadDataUI <- function(input, output, session){
  tagList(
  textInput("variableName","variable name", "RR"),
  checkboxInput("usingExcel", "using Excel", value = FALSE),
  fileInput('files', label="load files in the correct format - see the information on the right", multiple=TRUE),
  selectInput("separator", "select separator",
              list("tabulator", ",", ";", "space")),
  textInput("data_columns", "enter the column for RR intervals and flags - see explanations", "1 2"),
  textInput("minmax","enter minimum and maximum acceptable RR length", "0 3000"),
  selectInput("color", "select color from the list below",
              glob_color_list)
  )
}

#' data loading module
#' @param input input shiny variable
#' @param output output shiny variable
#' @param session input shiny variable
#' @dataAddress reactive containing the addresses of data files
loadData <- function(input, output, session,
                     dataAddress,
                     separator,
                     data_columns,
                     minmax,
                     usingExcel) {

  rr_and_flags <- reactive(read_and_filter_one_file(dataAddress(), 1, separator=getSep(separator),
                                           data_columns, minmax, usingExcel))
  return(rr_and_flags)
}

# --- FUNCTIONS ----
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

#' function to read and filter a single file containing rr intervals and annotations
#' @param file_addresses list with name, size, type and path of files with rr intervals
#' @param line_number the position of the file being loaded on the list
#' @param separator separator for the loaded file (tab, comma etc.)
#' @param column_data text, two numbers, the numbers of the RR column and the annotation column (typically "1 2")
#' @param minmax tex, two numbers, the minimum RR to be counted as RR of sinus origin, likewise for maximum
#' @param using_Excel boolean, whether the files are Excel files
#'
#' @return list with two elements, RR intervals column and annotations column
#' @export
read_and_filter_one_file <- function(file_addresses, line_number, separator, column_data, minmax, using_Excel){
  data_file <- file_addresses$datapath[line_number]
  javaerror <- FALSE; csverror <- FALSE # these show whether the function should return "some_problem" and exit
  if (using_Excel){
    if (data_file=="../initial_data/RR.csv") data_file="../initial_data/RR.xlsx" # just making sure that the XLConnect does not crash on text
    tryCatch(
      wb <- XLConnect::loadWorkbook(data_file),
      error = function(e) javaerror <<- TRUE # if java fails, "some_problem" will be returned and it should be handled in reactive plot
    )
    if (javaerror) return(data.frame("some_problem"))
    data <- XLConnect::readWorksheet(wb, sheet = 1) # this will never happen if java fails
  } else {
    data <- read.csv(data_file, sep = separator, header = T, row.names=NULL)
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

#' function to read the separator from input - it is either what has ben selected
#' or a selection has to be translated into a real separator
#'
#' @param separator separator
#'
#' @return separator
#' @export
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
