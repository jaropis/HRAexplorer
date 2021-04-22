# HRAexplorer
## How to install locally on Windows:
- download R from https://cloud.r-project.org/ and install
- download and install free RStudio from https://www.rstudio.com/products/rstudio/download/
- in the R console (the one with the `>` prompt) enter: `install.packages("devtools", dependencies = TRUE)`
- answer Y or the top choice to any questions
- in the console enter `devtools::install_github("jaropis/hrvhra", dependencies = TRUE)`
- in the console enter  `devtools::install_github("jaropis/HRAexplorer", dependencies = TRUE)`
- from now on, you will only need the following: run RStudio and enter in the prompt `HRAexplorer::runHRAexplorer()`
