#' module to plot various plots
plotsUI <- function(id) {
  ns <- NS(id)
  tagList(
    plotOutput(ns("current_plot")),
    downloadButton(ns('downloadPlot'), 'Download Plot')
  )
}

#' module to plot various plots related to HRA/HRV
#'
#' @param type_of_plot plot type (poincare, runs, spectrum etc.)
#' @param data_address address of the file to be read
#' @param rct_line_number reactive, the line holding the name of the file we want to analyze
#' @param inp_data_columns numbers of columns holding RR intervals and annotations
#' @param inp_separator separator used in file
#' @param inp_minmax minimum and maximum values of RR intervals of sinus origin
#' @param inp_using_excel whether or not use Excel format
#' @param inp_variable_name name of the variable for display
#' @param inp_color color for plotting
#'
#' @return plot (also causes side effect of saving the plot to disc)
plots <- function(input, output, session,
                  type_of_plot = "poincare",
                  data_address,
                  rct_line_number,
                  data_columns,
                  separator,
                  minmax,
                  using_excel,
                  variable_name,
                  color) {

  output$current_plot <- renderPlot({
    req(rct_line_number())
    if (type_of_plot == "poincare") {
      read_and_filter_one_file(data_address,
                               line_number = as.numeric(rct_line_number()),
                               separator = separator,
                               column_data = data_columns,
                               minmax = minmax,
                               using_excel = using_excel
      ) %>% # TODO - what about errors??
        as.data.frame() %>%
        hrvhra::pp() %>%
        hrvhra::drawpp(vname = variable_name,
                       col = glob_marker_color, bg = color, pch = 21)
    }})

  output$downloadPlot <- downloadHandler(
    filename = "PoincarePlot.png",
    content = function(file) {
      if (type_of_plot == "poincare") {
        req(rct_line_number())
        rr_and_flags <- read_and_filter_one_file(data_address,
                                                 line_number = rct_line_number(),
                                                 separator = separator,
                                                 column_data = data_columns,
                                                 minmax = minmax,
                                                 using_excel = using_excel)
        png(file, width=1800, height = 1900, res=300)
        hrvhra::drawpp(rr_and_flags$RR, rr_and_flags$annotations,
                       vname = variable_name,
                       col = glob_marker_color, bg = color, pch = 21)
        dev.off()} else {
          NULL
        }
    })
}
