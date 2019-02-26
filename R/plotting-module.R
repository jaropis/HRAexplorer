#' module to plot various plots

plotsUI <- function(id) {
  ns <- NS(id)
  tagList(
    plotOutput(ns("current_plot"))
  )
}

#' module to plot various plots related to HRA/HRV
#'
#'
plots <- function(input, output, session,
                  type_of_plot = "poincare",
                  dataAddress,
                  line_number,
                  data_columns,
                  separator,
                  minmax,
                  using_excel,
                  variable_name,
                  color) {
  if (type_of_plot == "poincare") {
    print("ifek")
    output$current_plot <- renderPlot({
      print("plotek")
      rr_and_flags <- read_and_filter_one_file(dataAddress,
                                               line_number,
                                               getSep(separator),
                                               data_columns,
                                               minmax,
                                               using_excel
      )
      print("wlazl")
      # todo - what about errors??
      hrvhra::drawpp(rr_and_flags$RR, rr_and_flags$annotations,
                     vname = ifelse(variable_name  == "", "RR", variable_name),
                     col = "black", bg = color, pch = 21)

    })
  }
}
