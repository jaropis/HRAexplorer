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

