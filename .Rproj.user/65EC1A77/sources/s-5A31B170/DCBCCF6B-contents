ui <-
  fluidPage(
    titlePanel("Measures of the Poincare Plot"),
    fluidRow(
      column(3,
             wellPanel(
               textInput("variableName","variable name", "RR"),
               checkboxInput("usingExcel", "using Excel", value = FALSE),
               fileInput('files', label="load files in the correct format - see the information on the right", multiple=TRUE),
               selectInput("separator", "select separator",
                           list("tabulator", ",", ";", "space")),
               textInput("data_columns", "enter the column for RR intervals and flags - see explanations", "1 2"),
               textInput("minmax","enter minimum and maximum acceptable RR length", "0 3000"),
               selectInput("color", "select color from the list below",
                           glob_color_list)

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
