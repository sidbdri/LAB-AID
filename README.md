# LAB-AID
Laboratory-assisted interrogation of data: An interactive application for the visualisation and analysis of experimental properties.

## Implementation
This is an [R Shiny](https://shiny.rstudio.com/) application and it can be run through [R Studio](https://www.rstudio.com/) locally or hosted via [Shiny Server](https://www.rstudio.com/products/shiny/shiny-server/). It is also possible to deploy the application to [shinyapps.io](https://www.shinyapps.io/). 
Please refer to the [R Shiny documenation](https://www.rdocumentation.org/packages/shiny/versions/1.3.2) on how to deploy Shiny applications.

## Configuration
LAB-AID is configured through the config.json file. The file has the following structure:

- Title - application title. 
- About - application and/or dataset description.
- Datasets - entries for each datasets. These entries are required for every dataset used.
  - Name - name of the dataset.
  - Path - path to the dataset .xlsx or .csv file.
  - nFactors - number of experimental factors.
  - Description - descripton of the dataset.

All entries can be modified to the user's liking and requirement, but JSON structure has to be adhered. Please refer to the [JSON](https://www.json.org/) documentation.

## Input file structure
Input files can be Excel spreadsheets (.xls or .xlsx) or comma-separated value (.csv) files. Tables have to be in the long format, with experimental factor columns first, followed by columns with measured variables.

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
