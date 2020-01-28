#' function to get results matrix
#' @param current_values values to be currently displayed
#' @return data_frame with values and View buttons
#' @export
get_results_matrix <- function(current_values) {
  results_matrix <- as.data.frame(current_values)
  buttons_row <- paste0('
                 <button type="button" class="btn btn-primary" id="btn_view_',1:nrow(results_matrix),'" onclick = "click_more(btn_view_',1:nrow(results_matrix),')">View</button>
               ')
  results_matrix <- data.frame(file = results_matrix[, 1, drop = FALSE],
                               "view" = buttons_row,
                               results_matrix[, 2:ncol(results_matrix)])
}
#' function returning Poincare descriptors to be shown on the flipside
#'
#' @param data_line named vector with Poincare descriptors
#' @param type_of_plot type of plot
#'
#' @return HTML string
#' @export
#'
description_string <- function(data_line, type_of_plot) {
  if (type_of_plot == "poincare") {
    html_description <- poincare_description_string(data_line)
  }

  if (type_of_plot == "runs") {
    html_description <- runs_description_string(data_line)
  }

  html_description
}
#' function returning Poincare descriptors to be shown on the flipside
#'
#' @param data_line named vector with Poincare descriptors
#'
#' @return HTML string
poincare_description_string <- function(data_line) {
  paste("<div id='poincare_plot'>",
        "<hr>",
        "<h4>HRV</h4>",
        "<hr>",
        "<strong>SDNN</strong>:", data_line["SDNN"], ", <strong>SD1</strong>:", data_line["SD1"], ", <strong>SD2</strong>:", data_line["SD2"],
        "<hr>",
        "<h4>HRA</h4>",
        "<hr>",
        "<strong>SD1<sub>d</sub></strong>:", data_line["SD1d"], ", <strong>SD1<sub>a</sub></strong>:", data_line["SD1a"], "<br />",
        "<strong>SD2<sub>d</sub></strong>:", data_line["SD2d"], ", <strong>SD2<sub>a</sub></strong>:", data_line["SD2a"], "<br />",
        "<hr>",
        "<strong>SDNN<sub>d</sub></strong>:", data_line["SDNNd"], ", <strong>SDNN<sub>a</sub></strong>:", data_line["SDNNa"],
        "</div>",
        collapse="")
}

#' function returning Poincare descriptors
#'
#' @param data_line named vector with runs
#'
#' @return HTML string
#' @export
runs_description_string <- function(data_line) {
  # TODO
}