######
# In a clean R installation (R 4.2 highly recommended) this script will install
# the packages required to run LAB-AID
######

# Install required packages from CRAN

install.packages(c("car", "DT", "jsonlite", "lme4", "magrittr", "patchwork", 
                   "pheatmap", "plotly", "readxl", "reshape2", "shiny", 
                   "shinycssloaders", "shinyjs", "shinyWidgets", 
                   "tidyverse", "WriteXLS"))

# Install required packages from Bioconductor

if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("ComplexHeatmap")