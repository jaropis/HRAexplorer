#' alternative for null
#'
#' @param x value being checked for null
#' @param y default expression value
#' @return x if not null, y otherwise
#'
#' @export
`%||%` <- function(x, y) {
  if (is.null(x))
    return(y)
  x
}


#' function to get initial data addresses
#'
#' @return data frame with info on the initial files
#' @export
calculate_data_addresses <- function() {
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
}

loader <- function(path,
                   timeout = 1000,
                   interval = 1000,
                   sleep = 1000) {
  if (isTruthy(path)) {
    tagList(
      tags$div(
        class = "spinnerLoading overlay",
        id = "loader",
        tags$p(
          id = "pWithLoader",
          tags$img(src = path)
        )
      ),
      tags$script(
        sprintf("waitForEl('div.wrapper', startSpinner, [%d, %d, %d])", timeout, sleep, interval)
      )
    )
  } else {
    NULL
  }
}

#' Function checking if the uploaded files are Excel files
#' @param files_list
#' @return boolean
#'
#' @export
check_for_excel <- function(files_list) {
  first_file_split <- files_list[1, ]$datapath %>%
    strsplit(split = "\\.")
  if (first_file_split[[1]][length(first_file_split[[1]])] %in% c('xlsx', 'xls')) {
    return (TRUE)
  } else {
    return (FALSE)
  }
}

#' Function producing sample table for preview
#' @param samp_table data frame
#' @return data frame
#'
#' @export
sample_table <- function(samp_table) {
  values <- lapply(samp_table, as.character) %>%
    as.data.frame()
  samp_names <- names(samp_table)
  result <- rbind(samp_names, values)
  names(result) <- as.character(seq_len(ncol(samp_table)))
  result
}

#' Function to read one file raw, without any pre or post processing
#' @param file_addresses file addresses on disk
#' @param file_no number of the file to be read
#' @return data.frame
#'
#' @export
raw_read_one_file <- function(file_addresses, file_no = 1, separator) {
  if (!check_for_excel(file_addresses)) {
    read.csv((file_addresses %||% calculate_data_addresses())[file_no, c("datapath")], sep = separator)
  } else {
    openxlsx::read.xlsx((file_addresses %||% calculate_data_addresses())[file_no, c("datapath")])
  }
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
  flag_column <- as.numeric(strsplit(data_columns, " ")[[1]][2])
  for (idx in seq(nrow(file_addresses))) {
    file_data <- raw_read_one_file(file_addresses[idx, ], separator = separator)
    unique_flags <- c(unique_flags, unique(file_data[[flag_column]]))
  }

  rounding <- tryCatch({
    unique_flags <- as.numeric(unique_flags)
    unique_flags <- round(unique_flags)
  },
  error = function(cond) return (FALSE),
  warning = function(cond) return (FALSE))

  if (is.numeric(unique_flags)) {
  }
  unique_flags
}
