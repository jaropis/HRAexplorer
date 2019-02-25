ui <-
  fluidPage(
    titlePanel("Measures of the Poincare Plot"),
    fluidRow(
      column(3,
             wellPanel(
               data_infoUI("data-info")
             )
      ),
      column(9,
             tabsetPanel(
               tabPanel("Poincare plot",
                        plotOutput("plot"),
                        downloadButton('downloadPlot', 'Download Plot')
               ),
               tabPanel("Numerical results", tags$style(type="text/css", "#filesView { overflow-x: auto; max-width: 100%; }"),
                        DT::dataTableOutput("filesView"),
                        downloadButton('downloadResults', 'Download results as Excel file'))

             )
      )
      )
  )
