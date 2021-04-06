library(shiny)
library(tidyverse)
library(magrittr)
library(patchwork)
library(pheatmap)
library(shinyjs)
library(plotly)
library(shinyWidgets)
library(readxl)
library(jsonlite)

useSweetAlert()

source('busyIndicator.R')

shinyUI(fluidPage(
  
  tags$style(type="text/css", ".shiny-output-error { visibility: hidden; }", ".shiny-output-error:before { visibility: hidden; }"),
  useShinyjs(),
  navbarPage(title = jsonlite::fromJSON(txt = 'config.json')$Title,
             tabPanel('Data Select',
                      fluidRow(
                        column(4, # Dataset selection
                               #uiOutput('data.select')
                               selectInput(inputId = 'dataset', label = 'Dataset:', choices = jsonlite::fromJSON(txt = 'config.json')$Datasets$Name, selected = jsonlite::fromJSON(txt = 'config.json')$Datasets$Name[1])
                              ),
                        column(4,
                               uiOutput('avg.select')
                              )
                      ),
                      fluidRow(
                        tabsetPanel(type = 'tabs', id = 'viewTabs',
                                    tabPanel('Selection', br(), br(),
                                             fluidRow(
                                               column(4,
                                                      lapply(seq(1, 200, 2), function(x){
                                                        uiOutput(paste0('e_', x))
                                                      })
                                               ),
                                               column(4,
                                                      lapply(seq(2, 200, 2), function(x){
                                                        uiOutput(paste0('e_', x))
                                                      })
                                               )
                                             )
                                    ),
                                    tabPanel('Highlights', br(), br(),
                                             fluidRow(
                                               column(4,
                                                      lapply(seq(1, 200, 2), function(x){
                                                        uiOutput(paste0('h_', x))
                                                      })
                                               ),
                                               column(4,
                                                      lapply(seq(2, 200, 2), function(x){
                                                        uiOutput(paste0('h_', x))
                                                      })
                                               )
                                             )
                                    ),
                          tabPanel('Preview', br(), br(),
                                   uiOutput('prev.factors'),
                                   radioButtons('prev.type', label = 'Plot type:', choices = list('Boxplot' = 'box', 'Violin plot' = 'violin'), inline = T),
                                   helpText('Note: Preview plots may take a while to load.'),
                                   plotlyOutput('prev.plot', height = '800px')
                                   ),
                          tabPanel('Summary', 
                                   radioButtons('statsumType', label = 'Show:', inline = T, choices = list('Factors' = 'factors', 'Variables' = 'variables')),
                                   conditionalPanel(condition = "input.statsumType == 'factors'", uiOutput('statsumFactors')),
                                   conditionalPanel(condition = "input.statsumType == 'variables'", uiOutput('statsumVariables'))
                          )
                        )
                      )
                      ),
             tabPanel('Data Plots',
                      sidebarLayout(
                        sidebarPanel(width = 3,
                                     radioButtons(inputId = 'plot_data', label = 'Data:', choices = c('Individual points' = 'cell', 'Averaged' = 'animal'), selected = 'cell'),
                                     conditionalPanel(condition = "input.main_plot_type == 'dist'", radioButtons(inputId = 'plot_dist_type', label = 'Plot type:', choices = c('Boxplot' = 'box', 'Violin plot' = 'violin'), selected = 'box')),
                                     conditionalPanel(condition = "input.main_plot_type == 'dist'", sliderInput(inputId = 'plot_dist_opacity', label = 'Point opacity:', value = 1, min = 0, max = 1, step = 0.1)),
                                     conditionalPanel(condition = "input.main_plot_type == 'bar'", radioButtons(inputId = 'bar_value', label = 'Error bars:', choices = c('SEM' = 'SEM', '95% CI' = 'CI'), selected = 'SEM')),
                                     conditionalPanel(condition = "input.main_plot_type == 'hist'", radioButtons(inputId = 'plot_hist_type', label = 'Plot type:', choices = c('Bars' = 'bars', 'Smooth' = 'smooth'), selected = 'bars')),
                                     conditionalPanel(condition = "input.main_plot_type == 'hist' && input.plot_hist_type == 'bars'", sliderInput(inputId = 'plot_hist_bins', label = 'Bins:', min = 10, max = 50, value = 20)),
                                     uiOutput('plot_out_var'),
                                     uiOutput('plot_effect'),
                                     uiOutput('plot_x'),
                                     uiOutput('plot_y'),
                                     downloadButton(outputId = 'quickDownload', label = 'Download plot data')
                                     ),
                        mainPanel(
                          radioButtons(inputId = 'main_plot_type', label = 'Data display:', choices = list('Distribution plots' = 'dist', 'Bar plots' = 'bar', 'Histograms' = 'hist', 'Cumulative' = 'cumsum'), inline = T),
                          conditionalPanel(condition = "input.main_plot_type == 'dist'", plotlyOutput(outputId = 'dist_plot', height = '400px')),
                          conditionalPanel(condition = "input.main_plot_type == 'bar'", plotlyOutput(outputId = 'bar_plot', height = '400px')),
                          #conditionalPanel(condition = "input.main_plot_type == 'bar'", tableOutput(outputId = 'sum_data')),
                          conditionalPanel(condition = "input.main_plot_type == 'hist'", plotlyOutput(outputId = 'hist_plot', height = '400px')),
                          conditionalPanel(condition = "input.main_plot_type == 'cumsum'", plotOutput(outputId = 'cumsum_plot', height = '400px'))
                        )
                      )
             ),
             tabPanel('Modelling',
                      fluidPage(
                        fluidRow(
                          column(6,
                                 uiOutput('lmm_vars_select'),
                                 conditionalPanel(condition = "output.log_error", verbatimTextOutput(outputId = 'log_warning')),
                                 
                                 checkboxInput(inputId = 'lmm_logtrans', label = 'Log-transformed', value = F),
                                 plotOutput('lmm_qqp')),
                          column(6,
                                 uiOutput('lmm_factor_select')),
                          column(6,
                                 uiOutput('lmm_random_select'))
                        ),
                        fluidRow(
                          verbatimTextOutput('lmm_summary'),
                          conditionalPanel(condition = "input.lmm_random.length > 0", verbatimTextOutput('lmm_Anova'))
                          
                        )
                      )),
             tabPanel('Correlation',
                      fluidPage(
                        tabsetPanel(type = 'pills',
                                    tabPanel('Condition',
                                             fluidRow(
                                               checkboxGroupInput(inputId = 'corr_opts', label = '', choices = list('Clustered' = 'clustered'), inline = T), helpText("Correlation data is selected on the 'Data select' tab.")
                                             ),
                                             sidebarLayout(
                                               sidebarPanel(
                                                 uiOutput('corr_factor'),
                                                 conditionalPanel(condition = "input.corr_factor != 'None'", uiOutput('corr_levels'))
                                                 #uiOutput('corr_levels')
                                               ),
                                               mainPanel(
                                                 plotOutput('corr_plot', height = '800px')
                                               )
                                             )
                                    ),
                                    tabPanel('Pairwise',
                                      fluidRow(
                                        checkboxGroupInput(inputId = 'corr_opts.p', label = '', choices = list('Clustered' = 'clustered'), inline = T), helpText("Correlation data is selected on the 'Data select' tab.")
                                      ),
                                      fluidRow(
                                        uiOutput('corr_factor.p')
                                      ),
                                      fluidRow(
                                        column(6,
                                               uiOutput('corr_levels.p1'),
                                               plotOutput('corr_plot.p1', height = '800px')
                                        ),
                                        column(6,
                                               uiOutput('corr_levels.p2'),
                                               plotOutput('corr_plot.p2', height = '800px')
                                          )
                                        )
                                      )
                                    )
                      ),
                    ),
             tabPanel('Downloads',
                      fluidPage(
                        fluidRow(
                          column(5, uiOutput('factor_select')),
                          column(5, uiOutput('variable_select'))
                        ),
                        fluidRow(
                          column(2, downloadButton(outputId = 'downloadData', label = 'Download .xlsx')),
                          column(2, downloadButton(outputId = 'downloadDataCSV', label = 'Download .csv'))
                          ), br(),
                        fluidRow(
                          DT::dataTableOutput('down.table')
                        )
                      )),
             tabPanel('Configuration',
                      h1('Dataset management'),
                      fluidRow(
                        column(width = 6,
                          h2('Add dataset'),
                          fileInput(inputId = 'add_file', label = 'Upload file', multiple = F, accept = c('.csv', '.xls', '.xlsx'), buttonLabel = 'Upload'),
                          textInput(inputId = 'ds_name', 'Dataset name:'),
                          textInput(inputId = 'ds_factors', 'Number of factors:'),
                          textAreaInput(inputId = 'ds_desc', 'Description:', rows = 3),
                          withBusyIndicatorUI(
                            actionButton(inputId = 'ds_submit', 'Submit') 
                          )
                        ),
                        column(width = 6,
                          h2('Update dataset'),
                          fileInput(inputId = 'update_file', label = 'Upload updated dataset', multiple = F, accept = c('.csv', '.xls', '.xlsx'), buttonLabel = 'Upload'),
                          uiOutput('ds_update_select'),
                          withBusyIndicatorUI(
                            actionButton(inputId = 'ds_update', 'Update')
                          ),
                          h2('Remove dataset'),
                          selectInput(inputId = 'rm_select', 'Select dataset to remove:', choices = jsonlite::fromJSON(txt = 'config.json')$Datasets$Name, multiple = F),
                          actionButton(inputId = 'ds_remove', 'Remove')
                        )
                      ),
                      hr(),
                      h1('Configuration'),
                      textInput(inputId = 'app_name', 'Application title:', value = jsonlite::fromJSON(txt = 'config.json')$Title),
                      textAreaInput(inputId = 'app_desc', 'Description:', rows = 3, value = jsonlite::fromJSON(txt = 'config.json')$About),
                      helpText('Application restart required for configuration changes to take effect.'),
                      actionButton(inputId = 'app_save', label = 'Save'),
                      hr()
                      ),
             tabPanel('About',
                      h1('About'),
                      p(jsonlite::fromJSON(txt = 'config.json')$About), br(),
                      apply(fromJSON(txt = 'config.json')$Datasets, 1, function(x){
                        tags$div(
                          HTML(paste0("<h3>", x[1], "</h3>", "<p>", x[4], "</p><br>"))
                        )
                      })
                      )
  )
  
))
