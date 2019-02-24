ui <-
  fluidPage(
    titlePanel("Measures of the Poincare Plot"),
    fluidRow(
      column(3,
             wellPanel(
               data_infoUI("data-info")
             )
      ),
      column(6,
             tabsetPanel(
               tabPanel("Poincare plot",
                        plotOutput("plot"),
                        downloadButton('downloadPlot', 'Download Plot')
               ),
               tabPanel("Numerical results", tags$style(type="text/css", "#filesView { overflow-x: auto; max-width: 100%; }"),
                        tableOutput("filesView"),
                        downloadButton('downloadResults', 'Download results as Excel file'))

             )
      ),

      column(3, HTML(paste("<h4>Explanation</h4>",PP_description()))
      )
    )
  )
