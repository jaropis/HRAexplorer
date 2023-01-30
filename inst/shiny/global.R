# libraries ----
library(shinydashboard)
library(shinydashboardPlus)
library(dplyr)
library(slider)
library(HRAexplorer)

# modules ----
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

glob_separators <- list(
  space = " ",
  tabulator = "\t",
  "," = ",",
  ";"  = ";"
)
# other globals
glob_marker_color = "black"

# variables to study dynamic asymmetry
glob_dynamic_asymmetry_vars <- c(
  "SD1d>SD1a",
  "SD2a>SD2d",
  "SDNNa>SDNNd"
)
runs_a <- paste0("AR", 1:50)
runs_d <- paste0("DR", 1:50)
runs_choices <- c(paste0(runs_a, '>', runs_d),
                  paste0(runs_d, '>', runs_a))
glob_dynamic_asymmetry_vars <- c(glob_dynamic_asymmetry_vars, runs_choices)

glob_tolerance = 0.05
glob_pnnX = "30 50"
glob_pnn_perc = "5 10"
