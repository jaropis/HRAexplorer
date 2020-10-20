ui <- dashboardPage(
  dashboardHeader(title = "HRAExplorer"),
  dashboardSidebar(
    sidebarMenu(
      id = "sidebar-tabs",
      menuItem("Import and filter data",
               tabName = "data_import",
               icon = icon("database")),
      menuItem("HRV time domain", icon = icon("heart"),
               menuSubItem("Static",
                           tabName = "hrv_time_domain"
               ),
               menuSubItem("Dynamic",
                           tabName = "hrv_time_domain-dynamic")),
      menuItem("Runs", icon = icon("chart-line"),
               menuSubItem("Static",
                           tabName = "runs"
               ),
               menuSubItem("Dynamic",
                           tabName = "runs-dynamic")
      ),
      menuItem("Fourer spectrum", icon = icon("area-chart"),
               menuSubItem("Static",
                           tabName = "spectral"),
               menuSubItem("Dynamic",
                           tabName = "spectral-dynamic")
      ),
      menuItem("Quality", icon = icon("thumbs-up"),
               menuSubItem("Static",
                           tabName = "quality",
               ),
               menuSubItem("Dynamic",
                           tabName = "quality-dynamic")
      )
    )
  ),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "css/custom.css"),
      tags$script(src = "javascript/all.js"),
      # now loader
      tags$link(rel = "stylesheet", type = "text/css", href = "css/loader.css"),
      loader("img/loader2.svg", timeout = 1000, interval = 300, sleep = 500)
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
      tabItem(tabName = "spectral",
              fluidRow(
                flipBox( id = 3,
                         front_title = "Numerical results",
                         solidHeader = FALSE,
                         width = 12,
                         main_tableUI("main-table-spectral"),
                         header_img = NULL,
                         main_img = NULL,
                         back_content = {
                           fluidRow(
                             box(title = "Lomb-Scargle periodogram",
                                 plotsUI("spectral-plots")
                             )
                           )
                         })
              )
      ),
      tabItem(tabName = "quality",
              fluidRow(
                flipBox( id = 4,
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
      ),
      tabItem(tabName = "hrv_time_domain-dynamic",
              fluidRow(
                flipBox( id = 5,
                         front_title = "Numerical results",
                         solidHeader = FALSE,
                         width = 12,
                         main_tableUI("main-table-dynamic"),
                         header_img = NULL,
                         main_img = NULL,
                         back_content = {
                           fluidRow(
                             box(width = 12,
                                 title = "Single file view",
                                 main_tableUI("details-table-pp")
                             ))
                         })
              )
      ),
      tabItem(tabName = "runs-dynamic",
              fluidRow(
                flipBox( id = 6,
                         front_title = "Numerical results",
                         solidHeader = FALSE,
                         width = 12,
                         main_tableUI("main-table-runs-dynamic"),
                         header_img = NULL,
                         main_img = NULL,
                         back_content = {
                           fluidRow(
                             box(title = "Poincare plot",
                                 #plotsUI("plots")
                                 tags$p("tyl 1")
                             ),
                             box(title = "Poincare plot descriptors",
                                 tags$p("tyl 2")
                                 #single_resultsUI("single-results")
                             )
                           )
                         })
              )
      ),
      tabItem(tabName = "spectral-dynamic",
              fluidRow(
                flipBox( id = 7,
                         front_title = "Numerical results",
                         solidHeader = FALSE,
                         width = 12,
                         main_tableUI("main-table-spectral-dynamic"),
                         header_img = NULL,
                         main_img = NULL,
                         back_content = {
                           fluidRow(
                             box(title = "Poincare plot",
                                 #plotsUI("plots")
                                 tags$p("tyl 1")
                             ),
                             box(title = "Poincare plot descriptors",
                                 tags$p("tyl 2")
                                 #single_resultsUI("single-results")
                             )
                           )
                         })
              )
      ),
      tabItem(tabName = "quality-dynamic",
              fluidRow(
                 flipBox(id = 8,
                         front_title = "Numerical results",
                         solidHeader = FALSE,
                         width = 12,
                         main_tableUI("main-table-quality-dynamic"),
                         header_img = NULL,
                         main_img = NULL,
                         back_content = {
                           fluidRow(
                             box(title = "Poincare plot",
                                 #plotsUI("plots")
                                 tags$p("tyl 1")
                             ),
                             box(title = "Poincare plot descriptors",
                                 tags$p("tyl 2")
                                 #single_resultsUI("single-results")
                             )
                           )
                         })
              )
      )
    )
  )
)
