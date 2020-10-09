
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
  window_slide = paste(window_type, move_type, sep = "_")
  time_function <- time_functions_list[[window_slide]]
  lapply(time_function(RR, window = window_length, cut_end = cut_end),
         function(window_table) {
           hrvhra::hrvhra(window_table[[2]], window_table[[3]])
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
  window_slide = paste(window_type, move_type, sep = "_")
  time_function <- time_functions_list[[window_slide]]
  runs_list <- lapply(time_function(RR, window = window_length, cut_end = cut_end),
                      function(window_table) {
                        hrvhra::countruns(window_table[[2]], window_table[[3]])
                      })
  hrvhra::bind_runs_as_table(runs_list, as.character(seq_along(runs_list))) %>%
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
                                                 window_length = 5,
                                                 cut_end = FALSE,
                                                 return_all = FALSE) {
  window_slide = paste(window_type, move_type, sep = "_")
  time_function <- time_functions_list[[window_slide]]
  lapply(time_function(RR, window = window_length, cut_end = cut_end),
         function(window_table) {
           hrvhra::calculate_RR_spectrum(data.frame(RR = window_table[[2]], annotations = window_table[[3]]))
         }) %>%
    dplyr::bind_rows() %>%
    cut_incomplete_rows(cut_end, return_all)
}
