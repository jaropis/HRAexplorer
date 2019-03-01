library(shinydashboard)
ui <- dashboardPage(
  dashboardHeader(title = "HRAExplorer"),
  dashboardSidebar(),
  dashboardBody(
    fluidRow(
      box(main_tableUI("main-table"), title = "Numerical results", solidHeader = TRUE, width = 12)
    )
  )
)
