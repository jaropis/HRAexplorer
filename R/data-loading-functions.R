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

#' List with usual flags coding
#' @export
flags_and_codes = list(
  sinus = 0,
  ventricular = 1,
  supraventricular = 2,
  artefact = 3
)

#' function to read and filter a single file containing rr intervals and annotations
#' @param file_addresses list with name, size, type and path of files with rr intervals
#' @param line_number the position of the file being loaded on the list
#' @param separator separator for the loaded file (tab, comma etc.)
#' @param column_data text, two numbers, the numbers of the RR column and the annotation column (typically "1 2")
#' @param minmax tex, two numbers, the minimum RR to be counted as RR of sinus origin, likewise for maximum
#' @param using_excel boolean, whether the files are Excel files
#' @param flags_coding list with flags_coding
#'
#' @return list with two elements, RR intervals column and annotations column
#' @export
read_and_filter_one_file <- function(file_addresses, line_number, separator, column_data, minmax, using_excel, flags_coding) {
  data <- raw_read_one_file(file_addresses, line_number, sep = separator)
  column_idx <- read_numbers_from_field(column_data)
  RR_idx <- column_idx[1]
  flag_idx <- ifelse(length(column_idx) > 1, column_idx[2], 0)
  RR <- as.numeric(data[[RR_idx]])
  if (flag_idx > 0) {
    flags <- as.character(data[[flag_idx]])
    for (flag in names(flags_coding)) {
      if (length(flags_coding[[flag]]) > 0) {
        flags[flags %in% flags_coding[[flag]]] <- flags_and_codes[[flag]]
      }
    }
    flags <- as.numeric(flags)
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

#' Function to read one file raw, without any pre or post processing
#' @param file_addresses file addresses on disk
#' @param file_no number of the file to be read
#' @return data.frame
#'
#' @export
raw_read_one_file <- function(file_addresses, file_no = 1, separator) {
  file_format <- check_for_format(file_addresses)
  if (file_format == 'csv') {
    return_data <- read.csv(file_addresses[file_no, c("datapath")], sep = separator)
  }
  if (file_format == 'excel') {
    return_data <- openxlsx::read.xlsx(file_addresses[file_no, c("datapath")])
  }
  if (file_format == 'matlab') {
    # this is our internal formad used for a specific study - don't expect the app to work with your matlab
    return_data <- R.matlab::readMat(file_addresses[file_no, c("datapath")])
    return_data <- data.frame(RR = diff(as.vector(return_data[["beatpos"]]) * 1000),
                              beats = return_data[["beats"]][2:length(return_data[["beats"]])])
  }
  return_data
}

#' Function collecting unique flags to be used in beat type dropdowns
#' @param file addresses file addresses on disk
#' @param data_columns string with numbers of columns with analyzed data
#' @param separator separator
#' @return vector
#'
#' @export
collect_unique_flags <- function(file_addresses, data_columns, separator) {
  unique_flags <- c()
  flag_column <- as.numeric(strsplit(data_columns, "[ ]+")[[1]][2])
  for (idx in seq(nrow(file_addresses))) {
    file_data <- raw_read_one_file(file_addresses[idx, ], separator = separator)
    unique_flags <- c(unique_flags, unique(as.character(file_data[[flag_column]])))
  }
  unique_flags
}

