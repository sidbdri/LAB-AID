[![DOI](https://zenodo.org/badge/206056644.svg)](https://zenodo.org/badge/latestdoi/206056644)

# LAB-AID
LAB-AID (Laboratory Automated Interrogation of Data): an interactive web application for visualization of multi-level data from biological experiments

## About

A key step in understanding the results of biological experiments is visualization of the data, particular when measurements exist within a hierarchy of interdependence. LAB-AID is a simple tool specifically designed to automatically visualize and query data resulting from such experiments. It aims to:

* lead to improved understanding of results
* help to determine which statistical tests should be performed
* easily identify outliers and sources of batch effects

For more details, please see [our preprint](https://www.biorxiv.org/content/10.1101/763318v1).

## Installation
This is an [R Shiny](https://shiny.rstudio.com/) application and it can be run through [R Studio](https://www.rstudio.com/) locally or hosted via [Shiny Server](https://www.rstudio.com/products/shiny/shiny-server/). It is also possible to deploy the application to [shinyapps.io](https://www.shinyapps.io/). 

### Local installation via RStudio
The application can be run on your computer locally via RStudio. Simply download all files from the repository and execute the following command in RStudio, replacing PATH with the path to the folder the files have been saved in.
```
shiny::runApp(appDir = 'PATH')
```
Alternatively, you can open scripts ui.R or server.R (or both) in RStudio, and press the 'Run App' button in the top right corner.

Make sure your R session has all the required packages installed. See below for the full list of required packages.

### Installation via Shiny Server
If you have access to a Shiny Server, all that is required is to download the repository into directory containing Shiny applications. Make sure the R version the Shiny Server is using has all the required packages installed, and the Shiny Server has sufficient privileges to read and write files within the application directory.

### Installation via ShinyApps.io
You can use [shinyapps.io](https://www.shinyapps.io/) hosting service to deploy the LAB-AID application. It is required to create a [shinyapps.io](https://www.shinyapps.io/) account and to configure your local RStudio client. Please follow the instructions in [shinyapps.io documentation](https://docs.rstudio.com/shinyapps.io/getting-started.html) on how to deploy applications.

## Configuration
LAB-AID is configured through the config.json file. The file has the following structure:

- Title - application title. 
- About - application and/or data set description.
- Data sets - entries for each data set (an entry is required for every data set used).
  - Name - name of the data set.
  - Path - path to the data set .xlsx or .csv file.
  - nFactors - number of experimental factors.
  - Description - descripton of the data set.

Data sets can be added and/or removed via the 'Configuration' tab within the application. When adding new data sets, it is mandatory to fill in the 'Dataset name', 'Number of factors' and 'Description' fields.

Alternatively, entries can be modified to the user's liking and requirements, but the JSON structure must be adhered to. Please refer to the [JSON](https://www.json.org/) documentation.

## Input file structure
Input files can be Excel spreadsheets (.xls or .xlsx) or comma-separated value (.csv) files. Tables have to be in long format, with experimental factor columns first, followed by columns containing measured variables.

## Required R packages
- [shiny](https://cran.r-project.org/web/packages/shiny/index.html)
- [shinyjs](https://cran.r-project.org/web/packages/shinyjs/index.html)
- [shinyWidgets](https://github.com/dreamRs/shinyWidgets)
- [tidyverse](https://cran.r-project.org/web/packages/tidyverse/index.html)
- [magrittr](https://cran.r-project.org/web/packages/magrittr/index.html)
- [readxl](https://cran.r-project.org/web/packages/readxl/index.html)
- [jsonlite](https://cran.r-project.org/web/packages/jsonlite/index.html)
- [reshape2](https://cran.r-project.org/web/packages/reshape2/index.html)
- [patchwork](https://github.com/thomasp85/patchwork)
- [pheatmap](https://cran.r-project.org/web/packages/pheatmap/index.html)
- [plotly](https://cran.r-project.org/web/packages/plotly/index.html)
- [WriteXLS](https://cran.r-project.org/web/packages/WriteXLS/index.html)
