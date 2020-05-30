#' export function to get analysis results
#'
#' @param analysis_type analysis type (poincare, runs, spectral)
#' @param file_addresses the addresses of the uploaded file(s)
#' @param separator the separator chosen by the user
#' @param column_data a 1x2 vector with the numbers of columns holding RR intervals and annotations
#' @param minmax 1x2 vector with the maximum and minimum acceptable RR intervals values
#' @param using_Excel boolean, whether Excel files are used
#'
#' @return the results of Poincare plot analysis
#' @export
get_numerical_results <- function(analysis_type, fileAddresses, separator = "\t", column_data = c(1,2), minmax = c(0, 3000), using_excel = FALSE) {
  if (analysis_type == "poincare")
    return(getPpResults(fileAddresses, separator, column_data, minmax, using_excel))
  if (analysis_type == "runs")
    return(get_runs_results(fileAddresses, separator, column_data, minmax, using_excel))
  if (analysis_type == "spectral")
    return(get_spectral_results())
  if (analysis_type == "quality")
    return(get_quality_results())
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
getPpResults <- function(fileAddresses,
                         separator = "\t",
                         column_data = c(1, 2),
                         minmax = c(0, 3000),
                         using_excel = FALSE) {
  results <- c()
    for (lineNumber in  1:length(fileAddresses[[1]])){
      rr_and_flags <- read_and_filter_one_file(fileAddresses, lineNumber, separator, column_data, minmax, using_excel)
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
#'
#' @return the results of Poincare plot analysis
get_runs_results <- function(fileAddresses,
                             separator = "\t",
                             column_data = c(1, 2),
                             minmax = c(0, 3000),
                             using_excel = FALSE) {
  results <- list()
  for (lineNumber in  1:length(fileAddresses[[1]])){
    rr_and_flags <- read_and_filter_one_file(fileAddresses, lineNumber, separator, column_data, minmax, using_excel)
    temp_results <- list(hrvhra::countruns(rr_and_flags[[1]], rr_and_flags[[2]]))
    results <- c(results, temp_results)
  }
  results <- hrvhra::bind_runs_as_table(results, fileAddresses$name)
  results
}