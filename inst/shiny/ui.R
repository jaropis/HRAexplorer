library(shinydashboard)
library(shinydashboardPlus)
ui <- dashboardPage(
  dashboardHeader(title = "HRAExplorer"),
  dashboardSidebar(),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    fluidRow(
      flipBox( id = 1,
               front_title = "Numerical results",
               solidHeader = FALSE,
               width = 12,
               main_tableUI("main-table"),
               header_img = NULL,
               main_img = NULL,
               back_content = {
                 plotsUI("plots")
               })
    )
  )
)
