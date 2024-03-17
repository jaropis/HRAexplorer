#' function to get results matrix
#' @param current_values values to be currently displayed
#' @param button_label label on the button
#' @param button_id if NULL, button will not be added
#' @return data_frame with values and View buttons
#' @export
get_results_matrix <- function(current_values, button_label = "View", button_id = NULL) {
  results_matrix <- as.data.frame(current_values)
  if (!is.null(button_id)) {
    buttons_col <- paste0(
      sprintf('<button type="button" class="btn btn-primary" id="%s', button_id),
      1:nrow(results_matrix),
      sprintf('" onclick = "click_more(%s', button_id),
      1:nrow(results_matrix),
      sprintf(')">%s</button>', button_label))
    return_tibble <- tibble::tibble(file = results_matrix[, 1, drop = FALSE],
                                    "view" = buttons_col,
                                    results_matrix[, 2:ncol(results_matrix), drop = FALSE]) %>%
      correct_dashes()
    return(return_tibble)
  }
  data.frame(file = results_matrix[, 1, drop = FALSE],
             results_matrix[, 2:ncol(results_matrix), drop = FALSE])
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

#' Function correcting dashes in col names so that it looks nicer
#' @param data_tibble tibble with data
#' @return tibble with fixed names
#' @export
correct_dashes <- function(data_tibble) {
  tibble_names <- names(data_tibble)
  prop_names <- which(grepl("_prop", tibble_names))
  pVal_names <- which(grepl("_pVal", tibble_names))
  for (idx in c(prop_names, pVal_names)) {
    tibble_names[idx] <- sub("_", " ", tibble_names[idx])
  }
  names(data_tibble) <- tibble_names
  data_tibble
}
