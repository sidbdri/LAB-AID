[![DOI](https://zenodo.org/badge/206056644.svg)](https://zenodo.org/badge/latestdoi/206056644)

# LAB-AID
**LAB-AID** (**Lab**oratory **A**utomated **I**nterrogation of **D**ata): an interactive web application for visualization of multi-level data from biological experiments

## About

A key step in understanding the results of biological experiments is visualization of the data, particular when measurements exist within a hierarchy of interdependence. LAB-AID is a simple tool specifically designed to automatically visualize and query data resulting from such experiments. It aims to:

* lead to improved understanding of results
* help to determine which statistical tests should be performed
* easily identify outliers and sources of batch effects

For more details, please see [our preprint](https://www.biorxiv.org/content/10.1101/763318v4).

## Installation
LAB-AID is an [R Shiny](https://shiny.rstudio.com/) application. It can be run through [R Studio](https://www.rstudio.com/) locally, hosted via [Shiny Server](https://www.rstudio.com/products/shiny/shiny-server/) or run from a [Docker](https://www.docker.com) image. It is also possible to deploy the application to [shinyapps.io](https://www.shinyapps.io/). In the latter three cases, you will access the LAB-AID application through a web browser.

If you have any problems installing LAB-AID via the methods described below, please let us know by raising an issue [here](https://github.com/sidbdri/LAB-AID/issues).

### Local installation via RStudio

The LAB-AID application can be run on your computer locally via RStudio. First, please follow the instructions [here](https://posit.co/download/rstudio-desktop/) to download and install R and RStudio Desktop, if you do not already have these. Then download all files from the LAB-AID repository (for example, by clicking the green 'Code' button at the top of this page, selecting 'Download ZIP', and uncompressing the downloaded ZIP file). Finally, execute the following command in the [RStudio Console pane](https://docs.posit.co/ide/user/ide/guide/ui/ui-panes.html), replacing ```PATH``` with the path to the folder the repository files have been saved in:

```
shiny::runApp(appDir = 'PATH')
```
Alternatively, you can open the files ```ui.R``` or ```server.R``` (or both) in RStudio, and press the 'Run App' button in the upper-right corner of the [RStudio Source pane](https://docs.posit.co/ide/user/ide/guide/ui/ui-panes.html).

Before running LAB-AID, make sure your R session has all the required packages installed (see [below](#r-versions-and-required-r-packages) for the full list of required packages). Note that it is possible that if you have an older version of R installed, you may encounter problems installing the packages that LAB-AID needs; if so, you can run the application through one of the alternative methods described below.

### Installation via Docker image

[Docker](https://www.docker.com) is a platform for developing, shipping, and running applications which may have complex dependencies (such as the set of R packages on which LAB-AID relies). The computing environment needed to run a particular application is contained in a [Docker "container image"](https://docs.docker.com/get-started/). The LAB-AID docker container image is hosted on [Docker Hub](https://hub.docker.com/repository/docker/zkozic/lab-aid) (n.b. a Docker Hub account is required to see this page, but is _not_ required to download and run the LAB-AID image). 

To run LAB-AID via the Docker container image, first please refer to [Docker documentation](https://docs.docker.com/get-docker/) for instructions on how to setup Docker Desktop on your system. Then the image can be downloaded and run by executing the following command line (for example by opening the 'Terminal' application on Mac OS, pasting the command, and pressing return):

```
docker run --rm -itd -p 80:80 zkozic/lab-aid:latest
```

Once this has finished, open your web browser and navigate to the address [localhost:80](http://localhost:80) (for example by clicking this link).

### Installation via shinyapps.io

If preferred, you can also use the [shinyapps.io](https://www.shinyapps.io/) web hosting service to deploy the LAB-AID application. You will need to create a [shinyapps.io](https://www.shinyapps.io/) account and to configure your local RStudio client. To do this, please follow the detailed instructions in the [shinyapps.io documentation](https://docs.rstudio.com/shinyapps.io/getting-started.html) on how to deploy applications from RStudio Desktop to shinyapps.io. After deployment, LAB-AID can then be accessed through the web browser on the shinyapps.io website.

### Installation via Shiny Server

(_n.b. advanced usage_)

If you have access to, or manage, a [Shiny Server](https://posit.co/products/open-source/shinyserver/) installation, all that is required is to download the LAB-AID repository into the directory containing Shiny applications. Make sure that the R installation that the Shiny Server is using has [all the required packages installed](#r-versions-and-required-r-packages), and that the Shiny Server has sufficient privileges to read and write files within the application directory.

## R versions and required R packages

LAB-AID has been tested with R versions 4.1, 4.2 and 4.3. It requires the following R packages to be installed:

- [shiny](https://cran.r-project.org/web/packages/shiny/index.html)
- [shinyjs](https://cran.r-project.org/web/packages/shinyjs/index.html)
- [shinyWidgets](https://github.com/dreamRs/shinyWidgets)
- [shinycssloaders](https://cran.r-project.org/web/packages/shinycssloaders/index.html)
- [tidyverse](https://cran.r-project.org/web/packages/tidyverse/index.html)
- [magrittr](https://cran.r-project.org/web/packages/magrittr/index.html)
- [readxl](https://cran.r-project.org/web/packages/readxl/index.html)
- [jsonlite](https://cran.r-project.org/web/packages/jsonlite/index.html)
- [reshape2](https://cran.r-project.org/web/packages/reshape2/index.html)
- [patchwork](https://github.com/thomasp85/patchwork)
- [pheatmap](https://cran.r-project.org/web/packages/pheatmap/index.html)
- [plotly](https://cran.r-project.org/web/packages/plotly/index.html)
- [WriteXLS](https://cran.r-project.org/web/packages/WriteXLS/index.html)
- [lme4](https://cran.r-project.org/web/packages/lme4/index.html)
- [car](https://cran.r-project.org/web/packages/car/index.html)
- [DT](https://cran.r-project.org/web/packages/DT/index.html)
- [ComplexHeatmap](https://www.bioconductor.org/packages/release/bioc/html/ComplexHeatmap.html) (Bioconductor)

These packages can be installed by hand, or via the ```setup.R``` script provided in this repository (recommended). To use the latter, simply open the file in RStudio Desktop, and click the 'Source' button in the upper-right corner of the [RStudio Source pane](https://docs.posit.co/ide/user/ide/guide/ui/ui-panes.html); all required packages will then be installed. If you are asked whether to ```Update all/some/none``` old packages, answer 'none' by typing ```n```, then pressing ```return```. If you are asked ```Do you want to install from sources the package which needs compilation? (Yes/no/cancel)```, answer 'no' by typing ```no```, then pressing ```return```.

Note that the ```setup.R``` script works best in a fresh installation of R. If you have a pre-existing R installation, in which you have already installed packages, it is _possible_ that you might encounter problems installing the packages required by LAB-AID, as some of these may be incompatible with the versions of packages you already have. Unfortunately, these package version incompatibilites are beyond our control; in this case, we would recommend installing LAB-AID via the [Docker image](#installation-via-docker-image), or [on shinyapps.io](#installation-via-shinyappsio).

## Configuration

LAB-AID is configured through the ```config.json``` file. The file has the following structure:

- Title - application title. 
- About - application and/or data set description.
- Data sets - entries for each data set (an entry is required for every data set used).
  - Name - name of the data set.
  - Path - path to the data set .xlsx or .csv file.
  - nFactors - number of experimental factors.
  - Description - descripton of the data set.

In general, we recommend adding and/or removing data sets via the **Configuration** tab within the running LAB-AID application. When adding new data sets, it is necessary to fill in the 'Dataset name' and 'Number of factors' fields; the optional 'Description' field provides text to appear on LAB-AID's **About** tab. Alternatively, the ```config.json``` file can be edited manually, but in this case, the correct JSON structure must be adhered to; please refer to the [JSON](https://www.json.org/) documentation for further details on this format.

Note that when first run, LAB-AID is pre-installed with the example dataset described in [our preprint](https://www.biorxiv.org/content/10.1101/763318v4).

## Input file structure

LAB-AID input files can be Excel spreadsheets (.xls or .xlsx) or comma-separated value (.csv) files. Tables have to be in long format, with experimental factor columns first, followed by columns containing measured variables.
