ui <- dashboardPage(
  dashboardHeader(title = "HRAExplorer"),
  dashboardSidebar(
      sidebarMenu(
        id = "sidebar-tabs",
        menuItem("Import and filter data",
                 tabName = "data_import",
                 icon = icon("database")),
        menuItem("HRV time domain",
                 tabName = "hrv_time_domain",
                 icon = icon("heart")),
        menuItem("Runs",
                 tabName = "runs",
                 icon = icon("chart-line")),
        menuItem("Fourer spectrum",
                 tabName = "fourier_spectrum",
                 icon = icon("area-chart")),
        menuItem("Quality",
                 tabName = "quality",
                 icon = icon("thumbs-up"))
      )
    ),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "css/custom.css"),
      tags$script(src = "javascript/all.js")
    ),
    tabItems(
      tabItem(tabName = "data_import",
              data_upload_and_filterUI("get-filter-data")
              ),
      tabItem(tabName = "hrv_time_domain",
               fluidRow(
                 flipBox( id = 1,
                          front_title = "Numerical results",
                          solidHeader = FALSE,
                          width = 12,
                          main_tableUI("main-table"),
                          header_img = NULL,
                          main_img = NULL,
                          back_content = {
                            fluidRow(
                              box(title = "Poincare plot",
                                  plotsUI("plots")
                              ),
                              box(title = "Poincare plot descriptors",
                                  single_resultsUI("single-results")
                              )
                            )
                          })
               )
      ),
      tabItem(tabName = "runs",
              fluidRow(
                flipBox( id = 2,
                         front_title = "Numerical results",
                         solidHeader = FALSE,
                         width = 12,
                         main_tableUI("main-table-runs"),
                         header_img = NULL,
                         main_img = NULL,
                         back_content = {
                           fluidRow(
                             box(title = "Monotonic runs",
                                 plotsUI("runs-plots")
                             )
                           )
                         })
              )
      ),
      tabItem(tabName = "quality",
              fluidRow(
                flipBox( id = 2,
                         front_title = "Numerical results",
                         solidHeader = FALSE,
                         width = 12,
                         main_tableUI("main-table-quality"),
                         header_img = NULL,
                         main_img = NULL,
                         back_content = {
                           fluidRow(
                             box(title = "Recording quality",
                                 plotsUI("quality-plots")
                             )
                           )
                         })
              )
      )
    )
  )
)
