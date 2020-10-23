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
#' @param using_excel boolean, whether the files are Excel files
#'
#' @return list with two elements, RR intervals column and annotations column
#' @export
read_and_filter_one_file <- function(file_addresses, line_number, separator, column_data, minmax, using_excel) {
  data <- raw_read_one_file(file_addresses, line_number, sep = separator)
  column_idx <- read_numbers_from_field(column_data)
  RR_idx <- column_idx[1]
  flag_idx <- ifelse(length(column_idx)>1, column_idx[2], 0)
  RR <- as.numeric(data[[RR_idx]])
  if (flag_idx > 0) {
    flags <- as.numeric(data[[flag_idx]])
  } else {
    flags <- RR*0
  }

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
get_sep <- function(separator){
  sep = separator
  if (separator == "tabulator"){
    sep = "\t"
  }
  if (separator == "space"){
    sep = " "
  }
  return(sep)
}
