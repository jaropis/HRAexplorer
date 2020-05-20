# options ----
options(java.parameters = c("-Xss2560k", "-Xmx2g"))

# libraries ----
library(HRAexplorer)
library(shinydashboard)
library(shinydashboardPlus)

# modules ----
source(file.path("modules", "data-info-module.R"))
source(file.path("modules", "upload-and-filter-module.R"))
source(file.path("modules", "main-table-module.R"))
source(file.path("modules", "plotting-module.R"))
source(file.path("modules", "single-results-module.R"))

# frontend utils ----
source(file.path("modules", "local-storage.R"))

# constants ----

# list holding the colors for selection
glob_color_list <- list("orange" = "orange",
                 "white"="white",
                 "gray" = "gray",
                 "red"="#CC6666",
                 "violet"="#9999CC",
                 "green"="#66CC99",
                 "black" = "black")

# globals for initial values ----
glob_init_line_number <- 1
glob_init_separator <- "tabulator"
glob_init_columns <- "2 3"
glob_init_min_max_sinus <- "0 3000"
glob_init_color <- "orange"
glob_init_var_name <- "RR"
glob_init_excel <- FALSE
glob_init_file_extension <- "rea"

#' other globals
glob_marker_color = "black"
