#' function for getting the results of Poincare Plot analysis
#'
#' @param file_addresses the addresses of the uploaded file(s)
#' @param separator the separator chosen by the user
#' @param column_data a 1x2 vector with the numbers of columns holding RR intervals and annotations
#' @param minmax 1x2 vector with the maximum and minimum acceptable RR intervals values
#' @param using_Excel boolean, whether Excel files are used
#'
#' @return the results of Poincare plot analysis
#' @export
getPpResults <- function(fileAddresses, separator = "\t", column_data, minmax, using_excel){
 # browser()
  results <- c()
    for (lineNumber in  1:length(fileAddresses[[1]])){
      rr_and_flags <- read_and_filter_one_file(fileAddresses, lineNumber, separator, column_data, minmax, using_excel)
      tempPP <- hrvhra::hrvhra(rr_and_flags[[1]], rr_and_flags[[2]])
      results <- rbind(results, HRAdescriptors(tempPP))
    }
  results <- round(results,3)
  results <- cbind(fileAddresses$name, results)
  colnames(results) <- c("file", "SD1", "SD2", "SDNN", "SD1d", "SD1a", "C1d", "SD2d", "SD2a", "C2d", "SDNNd", "SDNNa", "Cd")
  return(results)
}
