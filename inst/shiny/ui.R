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
