# HRAexplorer

## How to install locally:

- download and install rust (programming language) from [https://www.rust-lang.org/tools/install](https://www.rust-lang.org/tools/install)
- download R from [https://cloud.r-project.org/](https://cloud.r-project.org/) and install
- download and install free RStudio from [https://www.rstudio.com/products/rstudio/download/](https://www.rstudio.com/products/rstudio/download/)
- in the R console (the one with the `>` prompt) enter: `install.packages("devtools", dependencies = TRUE)`
- answer Y or the top choice to any questions
- in the console enter `devtools::install_github("jaropis/hrvhra", dependencies = TRUE)`
- in the console enter `devtools::install_github('jaropis/shinydashboardplus0.7.5')`
- in the console enter `devtools::install_github("jaropis/HRAexplorer", dependencies = TRUE)`
- from now on, you will only need the following: run RStudio and enter in the prompt `HRAexplorer::runHRAexplorer()`

## Fourier analysis for long recordings

If you are sure you want to calculate the spectral analysis for really long recordings, go to the `upload-and-filter-module.R` module and remove/change/disable this observer:

```R
observeEvent(c(input$files, input$separator), {
    # disabling spectral analysis for long recordings
    req(input$files)
    lengths <- c()
    for (file_idx in seq_along(input$files[[1]])) {
      lengths <- c(lengths,
                   raw_read_one_file(input$files %||% calculate_data_addresses(), file_no = file_idx, glob_separators[[input$separator]]) %>%
                     nrow()
      )}
    if(any(lengths > 6000)) {
      shinyjs::runjs("document.querySelectorAll(\"a[href='#shiny-tab-spectral']\")[0].style.pointerEvents = 'none'")
    } else {
      shinyjs::runjs("document.querySelectorAll(\"a[href='#shiny-tab-spectral']\")[0].style.pointerEvents = 'auto'")
    }
  }, ignoreInit = FALSE, ignoreNULL = FALSE)
  ```