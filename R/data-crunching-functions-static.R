#' export function to get analysis results
#'
#' @param analysis_type analysis type (poincare, runs, spectral)
#' @param file_addresses the addresses of the uploaded file(s)
#' @param separator the separator chosen by the user
#' @param column_data a 1x2 vector with the numbers of columns holding RR intervals and annotations
#' @param minmax 1x2 vector with the maximum and minimum acceptable RR intervals values
#' @param using_Excel boolean, whether Excel files are used
#' @param shuffle whether the data should be shuffled
#' @param flags_coding list with flags_coding
#'
#' @return the results of Poincare plot analysis
#' @export
get_numerical_results <- function(analysis_type,
                                  fileAddresses,
                                  separator = "\t",
                                  column_data = c(1,2),
                                  minmax = c(0, 3000),
                                  using_excel = FALSE,
                                  use_ULF = "No",
                                  flags_coding,
                                  shuffle) {
  if (analysis_type == "poincare")
    return(get_pp_results(fileAddresses, separator, column_data, minmax, using_excel,flags_coding, shuffle))
  if (analysis_type == "runs")
    return(get_runs_results(fileAddresses, separator, column_data, minmax, using_excel, flags_coding, shuffle))
  if (analysis_type == "spectral")
    return(get_spectral_results(fileAddresses, separator, column_data, minmax, using_excel, use_ULF, flags_coding, shuffle))
  if (analysis_type == "quality")
    return(get_quality_results(fileAddresses, separator, column_data, minmax, using_excel, flags_coding, shuffle))
}

#' function for getting the results of Poincare Plot analysis
#'
#' @param file_addresses the addresses of the uploaded file(s)
#' @param separator the separator chosen by the user
#' @param column_data a 1x2 vector with the numbers of columns holding RR intervals and annotations
#' @param minmax 1x2 vector with the maximum and minimum acceptable RR intervals values
#' @param using_Excel boolean, whether Excel files are used
#' @param flags_coding list with flags_coding
#' @param shuffle whether the data should be shuffled
#'
#' @return the results of Poincare plot analysis
#' @export
get_pp_results <- function(fileAddresses,
                         separator = "\t",
                         column_data = c(1, 2),
                         minmax = c(0, 3000),
                         using_excel = FALSE,
                         flags_coding,
                         shuffle) {
  results <- c()
    for (lineNumber in  1:length(fileAddresses[[1]])){
      rr_and_flags <- read_and_filter_one_file(fileAddresses, lineNumber, separator, column_data, minmax, using_excel, flags_coding, shuffle)
      temp_results <- hrvhra::hrvhra(rr_and_flags[[1]], rr_and_flags[[2]])
      results <- rbind(results, temp_results)
    }
  results <- as.data.frame(round(results,3))
  results <- cbind(fileAddresses$name, results)
  colnames(results)[1] <- "file"
  rownames(results) <- NULL
  return(results)
}

#' function for getting the results of monotonic runs analysis
#'
#' @param file_addresses the addresses of the uploaded file(s)
#' @param separator the separator chosen by the user
#' @param column_data a 1x2 vector with the numbers of columns holding RR intervals and annotations
#' @param minmax 1x2 vector with the maximum and minimum acceptable RR intervals values
#' @param using_Excel boolean, whether Excel files are used
#' @param flags_coding list with flags_coding
#' @param shuffle whether the data should be shuffled
#'
#' @return the results of Poincare plot analysis
#' @export
get_runs_results <- function(fileAddresses,
                             separator = "\t",
                             column_data = c(1, 2),
                             minmax = c(0, 3000),
                             using_excel = FALSE,
                             flags_coding,
                             shuffle) {
  results <- list()
  for (lineNumber in  1:length(fileAddresses[[1]])){
    rr_and_flags <- read_and_filter_one_file(fileAddresses, lineNumber, separator, column_data, minmax, using_excel, flags_coding, shuffle)
    temp_results <- list(hrvhra::countruns(rr_and_flags[[1]], rr_and_flags[[2]]))
    results <- c(results, temp_results)
  }
  results <- hrvhra::bind_runs_as_table(results, fileAddresses$name)
  results
}

#' function for getting the results of Poincare Plot analysis
#'
#' @param file_addresses the addresses of the uploaded file(s)
#' @param separator the separator chosen by the user
#' @param column_data a 1x2 vector with the numbers of columns holding RR intervals and annotations
#' @param minmax 1x2 vector with the maximum and minimum acceptable RR intervals values
#' @param using_Excel boolean, whether Excel files are used
#' @param flags_coding list with flags_coding
#' @param shuffle whether the data should be shuffled
#'
#' @return the results of Poincare plot analysis
#' @export
get_quality_results <- function(fileAddresses,
                         separator = "\t",
                         column_data = c(1, 2),
                         minmax = c(0, 3000),
                         using_excel = FALSE,
                         flags_coding,
                         shuffle) {
  results <- c()
  for (lineNumber in  1:length(fileAddresses[[1]])){
    rr_and_flags <- read_and_filter_one_file(fileAddresses, lineNumber, separator, column_data, minmax, using_excel, flags_coding, shuffle)
    temp_results <- hrvhra::describerr(rr_and_flags[[1]], rr_and_flags[[2]])
    results <- rbind(results, temp_results)
  }
  results <- as.data.frame(round(results, 3))
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
#' @param flags_coding list with flags_coding
#' @param shuffle whether the data should be shuffled
#'
#' @return the results of Poincare plot analysis
#' @export
get_spectral_results <- function(fileAddresses,
                                separator = "\t",
                                column_data = c(1, 2),
                                minmax = c(0, 3000),
                                using_excel = FALSE,
                                use_ULF = "No",
                                flags_coding,
                                shuffle) {
  results <- c()
  for (lineNumber in  1:length(fileAddresses[[1]])) {
    rr_and_flags <- read_and_filter_one_file(fileAddresses, lineNumber, separator, column_data, minmax, using_excel, flags_coding, shuffle)
    temp_results <- hrvhra::calculate_RR_spectrum(data.frame(RR = rr_and_flags[[1]], flags = rr_and_flags[[2]]),
                                                  bands = 'if'(use_ULF == "No", hrvhra::frequency_bands, hrvhra::frequency_bands_24))
    results <- rbind(results, temp_results)
  }
  results <- as.data.frame(round(results, 3))
  results <- cbind(fileAddresses$name, results)
  colnames(results)[1] <- "file"
  rownames(results) <- NULL
  return(results)
}
