## ----rversion, eval = TRUE-----------------------------------------------
message("* Checking R version.")
stopifnot(base::version$major == "3")
stopifnot(base::version$minor >= "6.1")


## ----biocmanagermsg, eval = TRUE-----------------------------------------
message("* Installing BiocManager.")


## ----biocmanager, eval = TRUE--------------------------------------------
if (!require("BiocManager"))
    install.packages("BiocManager")


## ----pkgs = packages ----------------------------------------------------------------

pkgs <- c("superpc", "glmnet", "matrixStats", "survminer", "survival", "rms", "dplyr", "pls", "Rdpack",
     "methods", "stats", "gplots", "ggplot2","gdata","foreign","readxl","httr","RCurl",
     'Rcpp','pcaMethods', 'Matrix', 'cluster', 'foreign', 'lattice', 'mgcv',"RCurl","MetabolicSurv",
     'shiny', 'shinydashboard', 'shinyWidgets', 'shinycssloaders', 'shinyBS', 'shinyjs', 'sf', 'tidyverse', 'leaflet', 
     'RColorBrewer','classInt', 'metafolio', 'DT', 'shinythemes')

pkgs <- sort(unique(pkgs))
pkgs


## ----pkgsmsg, eval = TRUE------------------------------------------------
message("* Installing packages.")


## ----install, eval = TRUE------------------------------------------------
ip <- rownames(installed.packages())
pkgs_to_install <- pkgs[which(!sub("^.+/", "", pkgs) %in% ip)]
BiocManager::install(pkgs_to_install, update = TRUE, ask = FALSE, upgrade = 'always')


## ----install, eval = TRUE------------------------------------------------
for (i in 1:length(pkgs)) {
    library(pkgs[i],character.only = TRUE) 
}

## ----success, eval = TRUE------------------------------------------------
message("* Successful!")