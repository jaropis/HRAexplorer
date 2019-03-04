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
#'@param type_of_plot plot type (poincare, runs, spectrum etc.)
#'@param rct_data_address address of the file to be read
#'@param line_number the line holding the name of the file we want to analyze
#'@param inp_data_columns numbers of columns holding RR intervals and annotations
#'@param inp_separator separator used in file
#'@param inp_minmax minimum and maximum values of RR intervals of sinus origin
#'@param inp_using_excel whether or not use Excel format
#'@param inp_variable_name name of the variable for display
#'@param inp_color color for plotting
#'
#'@return plot (also causes side effect of saving the plot to disc)
plots <- function(input, output, session,
                  type_of_plot = "poincare",
                  rct_data_address,
                  line_number,
                  inp_data_columns,
                  inp_separator,
                  inp_minmax,
                  inp_using_excel,
                  inp_variable_name,
                  inp_color) {

  if (type_of_plot == "poincare") {
    output$current_plot <- renderPlot({
      rr_and_flags <- read_and_filter_one_file(rct_data_address(),
                                               line_number = 1,
                                               separator = getSep(inp_separator() %||% 'tabulator'),
                                               column_data = inp_data_columns() %||% "1 2",
                                               minmax = inp_minmax() %||% "0 3000",
                                               using_excel = inp_using_excel() %||% FALSE
                                               )
      # todo - what about errors??
      hrvhra::drawpp(rr_and_flags$RR, rr_and_flags$annotations,
                     vname = inp_variable_name() %||% "RR",
                     col = "black", bg = inp_color() %||% "orange", pch = 21)

    })

    output$downloadPlot <- downloadHandler(
      filename = "PoincarePlot.png",
      content = function(file) {
        rr_and_flags <- read_and_filter_one_file(rct_data_address(), 1,
                                                 separator=getSep(inp_separator() %||% 'tabulator'),
                                                 inp_data_columns() %||% '1 2',
                                                 inp_minmax() %||% "0 3000",
                                                 inp_using_excel() %||% FALSE)
        png(file, width=1800, height = 1900, res=300)
        hrvhra::drawpp(rr_and_flags$RR, rr_and_flags$annotations,
                       vname = inp_variable_name() %||% "RR",
                       col = "black", bg = inp_color() %||% "orange", pch = 21)
        dev.off()
      })
  }
}
