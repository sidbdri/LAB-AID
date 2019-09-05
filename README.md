# LAB-AID
Laboratory-assisted interrogation of data: An interactive application for the visualisation and analysis of experimental properties.

## Implementation
This is an R Shiny application and it can be run through R Studio locally or hosted via Shiny Server. It is also possible to deploy the application to shinyapps.io. 
Please refer to the R Shiny documenation on how to deploy Shiny applications.

## Configuration
LAB-AID is configured through the config.json file. The file has the following structure:

1. Title - application title. 
2. About - application and/or dataset description.
3. Datasets - entries for each datasets. These entries are required for every dataset used.
  1. Name - name of the dataset.
  2. Path - path to the dataset .xlsx or .csv file.
  4. nFactors - number of experimental factors.
  5. Description - descripton of the dataset.
