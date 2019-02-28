library(shinydashboard)
ui <- dashboardPage(
  dashboardHeader(title = "HRAExplorer"),
  dashboardSidebar(),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    fluidRow(
      box(main_tableUI("main-table"), title = "Numerical results", solidHeader = TRUE, width = 12)
    )
  )
)
# ui <-
#   fluidPage(
#     titlePanel("Measures of the Poincare Plot"),
#     fluidRow(
#       column(3,
#              wellPanel(
#                data_infoUI("data-info")
#              )
#       ),
#       column(9,
#              tabsetPanel(
#                tabPanel("Poincare plot",
#                         plotsUI("plots")
#                ),
#                tabPanel("Numerical results", tags$style(type="text/css", "#filesView { overflow-x: auto; max-width: 100%; }"),
#                        main_tableUI("main-table"))
#
#              )
#       )
#       )
#   )
