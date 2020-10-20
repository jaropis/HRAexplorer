#' export function to get analysis results
#'
#' @param analysis_type analysis type (poincare, runs, spectral)
#' @param file_addresses the addresses of the uploaded file(s)
#' @param separator the separator chosen by the user
#' @param column_data a 1x2 vector with the numbers of columns holding RR intervals and annotations
#' @param minmax 1x2 vector with the maximum and minimum acceptable RR intervals values
#' @param using_Excel boolean, whether Excel files are used
#' @param window_type string, jumping or sliding
#' @param move_type string, time based or index based
#' @param window_length numeric, window length
#'
#' @return the results of Poincare plot analysis
#' @export
get_dynamic_numerical_results <- function(analysis_type,
                                  fileAddresses,
                                  separator = "\t",
                                  column_data = c(1,2),
                                  minmax = c(0, 3000),
                                  using_excel = FALSE,
                                  use_ULF = "No",
                                  window_type,
                                  move_type,
                                  window_length) {
  if (analysis_type == "poincare_dynamic")
    return(get_dynamic_pp_results(fileAddresses,
                                  time_functions_list = glb_time_functions,
                                  separator,
                                  column_data,
                                  minmax,
                                  using_excel,
                                  window_type,
                                  move_type,
                                  window_length))
  if (analysis_type == "runs_dynamic")
    return(get_dynamic_runs_results(fileAddresses,
                                    time_functions_list = glb_time_functions,
                                    separator,
                                    column_data,
                                    minmax,
                                    using_excel,
                                    window_type,
                                    move_type,
                                    window_length))
  if (analysis_type == "spectral_dynamic")
    return(get_dynamic_spectral_results(fileAddresses,
                                        time_functions_list = glb_time_functions,
                                        separator,
                                        column_data,
                                        minmax,
                                        using_excel,
                                        use_ULF = use_ULF))
  if (analysis_type == "quality_dynamic")
    return(get_dynamic_quality_results(fileAddresses,
                                       time_functions_list = glb_time_functions,
                                       separator,
                                       column_data,
                                       minmax,
                                       using_excel))
}

#' function for getting the results of dynamic Poincare Plot analysis
#'
#' @param file_addresses the addresses of the uploaded file(s)
#' @param separator the separator chosen by the user
#' @param column_data a 1x2 vector with the numbers of columns holding RR intervals and annotations
#' @param minmax 1x2 vector with the maximum and minimum acceptable RR intervals values
#' @param using_Excel boolean, whether Excel files are used
#' @param window_type string, jumping or sliding
#' @param move_type string, time based or index based
#' @param window_length numeric, window length
#'
#' @return the results of Poincare plot analysis
get_dynamic_pp_results <- function(fileAddresses,
                                   time_functions_list = glb_time_functions,
                                   separator = "\t",
                                   column_data = c(1, 2),
                                   minmax = c(0, 3000),
                                   using_excel = FALSE,
                                   window_type,
                                   move_type,
                                   window_length) {
  results <- c()
  for (lineNumber in  1:length(fileAddresses[[1]])) {
    rr_and_flags <- read_and_filter_one_file(fileAddresses, lineNumber, separator, column_data, minmax, using_excel)
    temp_results <- get_single_pp_windowed_results(data.frame(RR = rr_and_flags[[1]], flags = rr_and_flags[[2]]),
                                                   time_functions_list = time_functions_list,
                                                   window_type = window_type,
                                                   move_type = move_type,
                                                   window_length = window_length) %>%
      colMeans(na.rm = TRUE)
    results <- rbind(results, temp_results)
  }
  results <- as.data.frame(round(results,3))
  results <- cbind(fileAddresses$name, results)
  colnames(results)[1] <- "file"
  rownames(results) <- NULL
  return(results)
}

#' function for getting the results of dynamic runs analysis
#'
#' @param file_addresses the addresses of the uploaded file(s)
#' @param separator the separator chosen by the user
#' @param column_data a 1x2 vector with the numbers of columns holding RR intervals and annotations
#' @param minmax 1x2 vector with the maximum and minimum acceptable RR intervals values
#' @param using_Excel boolean, whether Excel files are used
#' @param window_type string, jumping or sliding
#' @param move_type string, time based or index based
#' @param window_length numeric, window length
#'
#' @return the results of Poincare plot analysis
get_dynamic_runs_results <- function(fileAddresses,
                                     time_functions_list = glb_time_functions,
                                     separator = "\t",
                                     column_data = c(1, 2),
                                     minmax = c(0, 3000),
                                     using_excel = FALSE,
                                     window_type,
                                     move_type,
                                     window_length) {
  results <- c()
  for (lineNumber in  1:length(fileAddresses[[1]])){
    rr_and_flags <- read_and_filter_one_file(fileAddresses, lineNumber, separator, column_data, minmax, using_excel)
    temp_results <- get_single_runs_windowed_results(data.frame(RR = rr_and_flags[[1]], flags = rr_and_flags[[2]]),
                                                     time_functions_list = time_functions_list,
                                                     window_type = window_type,
                                                     move_type = move_type,
                                                     window_length = window_length) %>%
      dplyr::select(-c("file")) %>%
      colMeans(na.rm = TRUE) %>%
      t() %>%
      as.data.frame()

    results <- plyr::rbind.fill(results, temp_results) # rbinding columns with potentially different cols
  }
  results[is.na(results)] <- 0
  results <- as.data.frame(round(results,3))
  results <- cbind(fileAddresses$name, results)
  colnames(results)[1] <- "file"
  rownames(results) <- NULL
  return(results)
}

#' function for getting the results of Poincare Plot analysis
#'
#' @param file_addresses the addresses of the uploaded file(s)
#' @param separator the separator chosen by the user
#' @param column_data a 1x2 vector with the numbers of columns holding RR intervals and annotations
#' @param minmax 1x2 vector with the maximum and minimum acceptable RR intervals values
#' @param using_Excel boolean, whether Excel files are used
#'
#' @return the results of Poincare plot analysis
get_dynamic_spectral_results <- function(fileAddresses,
                                         use_ULF = FALSE,
                                         time_functions_list = glb_time_functions,
                                         separator = "\t",
                                         column_data = c(1, 2),
                                         minmax = c(0, 3000),
                                         using_excel = FALSE) {
  results <- c()
  for (lineNumber in  1:length(fileAddresses[[1]])){
    rr_and_flags <- read_and_filter_one_file(fileAddresses, lineNumber, separator, column_data, minmax, using_excel)
    temp_results <- get_single_spectral_windowed_results(data.frame(RR = rr_and_flags[[1]], flags = rr_and_flags[[2]]),
                                                         use_ULF = use_ULF,
                                                         time_functions_list = time_functions_list) %>%
      colMeans(na.rm = TRUE)
    results <- rbind(results, temp_results)
  }
  results <- as.data.frame(round(results, 3))
  results <- cbind(fileAddresses$name, results)
  colnames(results)[1] <- "file"
  rownames(results) <- NULL
  return(results)
}

#' function for getting the results of dynamic Poincare Plot analysis
#'
#' @param file_addresses the addresses of the uploaded file(s)
#' @param separator the separator chosen by the user
#' @param column_data a 1x2 vector with the numbers of columns holding RR intervals and annotations
#' @param minmax 1x2 vector with the maximum and minimum acceptable RR intervals values
#' @param using_Excel boolean, whether Excel files are used
#'
#' @return the results of Poincare plot analysis
get_dynamic_quality_results <- function(fileAddresses,
                                        time_functions_list = glb_time_functions,
                                        separator = "\t",
                                        column_data = c(1, 2),
                                        minmax = c(0, 3000),
                                        using_excel = FALSE) {
  results <- c()
  for (lineNumber in  1:length(fileAddresses[[1]])){
    rr_and_flags <- read_and_filter_one_file(fileAddresses, lineNumber, separator, column_data, minmax, using_excel)
    temp_results <- get_single_quality_windowed_results(data.frame(RR = rr_and_flags[[1]], flags = rr_and_flags[[2]]),
                                                        time_functions_list = time_functions_list) %>%
      colMeans(na.rm = TRUE)
    results <- rbind(results, temp_results)
  }
  results <- as.data.frame(round(results,3))
  results <- cbind(fileAddresses$name, results)
  colnames(results)[1] <- "file"
  rownames(results) <- NULL
  return(results)
}

#' time window functions as a list
#' @export
glb_time_functions <- list(time_jump = hrvhra::time_based_jump,
                           time_slide = hrvhra::time_based_slide,
                           index_jump = hrvhra::index_based_jump,
                           index_slide = hrvhra::index_based_slide)

#' Helper function cutting either the first row of the table, or the last row, or returning the whole table
#' @param resulting_table table with results from a windowing function
#' @param cut_end where the cut occurs -at the beginning or at the end or not at all (FALSE, TRUE)
#' @param return_all wheter all rows should be returned
#' @return table
cut_incomplete_rows <- function(resulting_table, cut_end, return_all) {
  if (return_all) {
    return(resulting_table)
  }
  if (cut_end) {
    resulting_table[1:(nrow(resulting_table) - 1), ]
  } else {
    resulting_table[2:(nrow(resulting_table)), ]
  }
}

#' Function calculating windowed hrvhra results for a single RR time series
#' @param RR rr object
#' @param window_type type of window, can be time or index
#' @param slide_type how should the window move: slide or jump
#' @param window_length length of the window, in minutes or beats, according to window_type
#' @retur data.frame with results for windows as rows
#' @export
get_single_pp_windowed_results <- function(RR,
                                           time_functions_list = glb_time_functions,
                                           window_type = "time",
                                           move_type = "jump",
                                           window_length = 5,
                                           cut_end = FALSE,
                                           return_all = FALSE) {
  window_slide = paste(move_type, window_type, sep = "_")
  rr_index <- 'if' (move_type == 'time', 2, 1) # index based windows do not have time track
  time_function <- time_functions_list[[window_slide]]
  lapply(time_function(RR, window = window_length, cut_end = cut_end),
         function(window_table) {
           hrvhra::hrvhra(window_table[[rr_index]], window_table[[rr_index + 1]])
         }) %>%
    dplyr::bind_rows() %>%
    cut_incomplete_rows(cut_end, return_all)
}

#' Function calculating windowed runs results for a single RR time series
#' @param RR rr object
#' @param window_type type of window, can be time or index
#' @param slide_type how should the window move: slide or jump
#' @param window_length length of the window, in minutes or beats, according to window_type
#' @retur data.frame with results for windows as rows
#' @export
get_single_runs_windowed_results <- function(RR,
                                             time_functions_list = glb_time_functions,
                                             window_type = "time",
                                             move_type = "jump",
                                             window_length = 5,
                                             cut_end = FALSE,
                                             return_all = FALSE) {
  window_slide = paste(move_type, window_type, sep = "_")
  rr_index <- 'if' (move_type == 'time', 2, 1) # index based windows do not have time track
  time_function <- time_functions_list[[window_slide]]
  runs_list <- lapply(time_function(RR, window = window_length, cut_end = cut_end),
                      function(window_table) {
                        hrvhra::countruns(window_table[[rr_index]], window_table[[rr_index + 1]])
                      })
  hrvhra::bind_runs_as_table(runs_list, as.character(seq_along(runs_list))) %>%
    cut_incomplete_rows(cut_end, return_all)

}

#' Function calculating windowed spectral results for a single RR time series
#' @param RR rr object
#' @param window_type type of window, can be time or index
#' @param slide_type how should the window move: slide or jump
#' @param window_length length of the window, in minutes or beats, according to window_type
#' @retur data.frame with results for windows as rows
#' @export
get_single_spectral_windowed_results <- function(RR,
                                                 time_functions_list = glb_time_functions,
                                                 window_type = "time",
                                                 move_type = "jump",
                                                 use_ULF = FALSE,
                                                 window_length = 5,
                                                 cut_end = FALSE,
                                                 return_all = FALSE) {
  window_slide = paste(window_type, move_type, sep = "_")
  time_function <- time_functions_list[[window_slide]]
  bands <- if (use_ULF == "Yes") {
    hrvhra::frequency_bands_24
  } else {
    hrvhra::frequency_bands
  }
  lapply(time_function(RR, window = window_length, cut_end = cut_end),
         function(window_table) {
           hrvhra::calculate_RR_spectrum(data.frame(RR = window_table[[2]], annotations = window_table[[3]]), bands)
         }) %>%
    dplyr::bind_rows() %>%
    cut_incomplete_rows(cut_end, return_all)
}

#' Function calculating windowed quality results for a single RR time series
#' @param RR rr object
#' @param window_type type of window, can be time or index
#' @param slide_type how should the window move: slide or jump
#' @param window_length length of the window, in minutes or beats, according to window_type
#' @retur data.frame with results for windows as rows
#' @export
get_single_quality_windowed_results <- function(RR,
                                                time_functions_list = glb_time_functions,
                                                window_type = "time",
                                                move_type = "jump",
                                                window_length = 5,
                                                cut_end = FALSE,
                                                return_all = FALSE) {
  window_slide = paste(window_type, move_type, sep = "_")
  time_function <- time_functions_list[[window_slide]]
  lapply(time_function(RR, window = window_length, cut_end = cut_end),
         function(window_table) {
           hrvhra::describerr(window_table[[3]])
         }) %>%
    dplyr::bind_rows() %>%
    cut_incomplete_rows(cut_end, return_all)
}
