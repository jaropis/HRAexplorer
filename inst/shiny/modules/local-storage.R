# this script provides local store - it is part of the shiny app so it is placed in the modules folder

#' @param local_storage list containing data state of the application - this is the session$userData object
#' @param data_key key for storing the data
#' @param data_value data to be stored at data_key
#' @export
store_in_storage <- function(local_storage, data_key, data_value) {
  local_storage$userData[[data_key]] <- data_value
}

# this script provides local store - it is part of the shiny app so it is placed in the modules folder

#' @param local_storage list containing data state of the application - this is the session$userData object
#' @param data_key key for storing the data
#' @param data_value data to be stored at data_key
#' @export
get_from_storage <- function(local_storage, data_key) {
  local_storage$userData[[data_key]]
}


