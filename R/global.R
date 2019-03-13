# CONSTANTS ----

# list holding the colors for selection
#' @export
glob_color_list <- list("orange" = "orange",
                 "white"="white",
                 "gray" = "gray",
                 "red"="#CC6666",
                 "violet"="#9999CC",
                 "green"="#66CC99",
                 "black" = "black")

# globals for initial values ----
#' @export
glob_init_line_number <- 1
glob_init_separator <- "tabulator"
glob_init_columns <- "2 3"
glob_init_min_max_sinus <- "0 3000"
glob_init_color <- "orange"
glob_init_var_name <- "RR"
glob_init_excel <- FALSE
glob_init_file_extension <- "rea"

#' other globals
#' @export
glob_marker_color = "black"

# state variables definitions ----
#' RR time series parameters
#' @export
state_RR_settings <- new.env(parent = emptyenv())
state_RR_settings$var_name <- glob_init_var_name
state_RR_settings$line_number <- glob_init_line_number
state_RR_settings$separator <- glob_init_separator
state_RR_settings$data_columns <- glob_init_columns
state_RR_settings$min_max_sinus <- glob_init_min_max_sinus
state_RR_settings$excel <- glob_init_excel
state_RR_settings$file_extension <- glob_init_file_extension

#' figures state
state_figures <- new.env(parent = emptyenv())
state_figures$color <- glob_init_color
