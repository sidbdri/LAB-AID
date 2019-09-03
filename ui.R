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

shinyUI(fluidPage(
  # The following line removes error messages when plots are being loaded. Can be commented out for debugging.
  tags$style(type="text/css", ".shiny-output-error { visibility: hidden; }", ".shiny-output-error:before { visibility: hidden; }"),
  useShinyjs(),
  navbarPage(jsonlite::fromJSON(txt = 'config.json')$Title,
             tabPanel('Data Select',
                      fluidRow(
                        column(4, # Dataset selection
                               uiOutput('data.select')
                              ),
                        column(4,
                               uiOutput('avg.select')
                              )
                      ),
                      fluidRow(
                        tabsetPanel(type = 'tabs', 
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
                                   )
                        )
                      )
                      ),
             tabPanel('Data Plots',
                      sidebarLayout(
                        sidebarPanel(width = 3,
                                     radioButtons(inputId = 'plot_data', label = 'Data:', choices = c('Individual point' = 'cell', 'Averaged' = 'animal'), selected = 'cell'),
                                     conditionalPanel(condition = "input.main_plot_type == 'dist'", radioButtons(inputId = 'plot_dist_type', label = 'Plot type:', choices = c('Boxplot' = 'box', 'Violin plot' = 'violin'), selected = 'box')),
                                     conditionalPanel(condition = "input.main_plot_type == 'hist'", radioButtons(inputId = 'plot_hist_type', label = 'Plot type:', choices = c('Bars' = 'bars', 'Smooth' = 'smooth'), selected = 'bars')),
                                     conditionalPanel(condition = "input.main_plot_type == 'hist' && input.plot_hist_type == 'bars'", sliderInput(inputId = 'plot_hist_bins', label = 'Bins:', min = 10, max = 50, value = 20)),
                                     uiOutput('plot_out_var'),
                                     uiOutput('plot_effect'),
                                     uiOutput('plot_x'),
                                     uiOutput('plot_y')
                                     ),
                        mainPanel(
                          radioButtons(inputId = 'main_plot_type', label = 'Data display:', choices = list('Distribution plots' = 'dist', 'Histograms' = 'hist', 'Cumulative' = 'cumsum'), inline = T),
                          conditionalPanel(condition = "input.main_plot_type == 'dist'", plotlyOutput(outputId = 'dist_plot', height = '400px')),
                          conditionalPanel(condition = "input.main_plot_type == 'hist'", plotlyOutput(outputId = 'hist_plot', height = '400px')),
                          conditionalPanel(condition = "input.main_plot_type == 'cumsum'", plotOutput(outputId = 'cumsum_plot', height = '400px'))
                        )
                      )
             ),
             tabPanel('Correlation',
                      fluidPage(
                        fluidRow(
                          checkboxGroupInput(inputId = 'corr_opts', label = 'Options:', choices = list('Clustered' = 'clustered', 'Pairwise' = 'pairwise'), inline = T), helpText("Correlation data is selected on the 'Data select' tab.")
                          ),
                        conditionalPanel(condition = "input.corr_opts.indexOf('pairwise') == -1",
                                         sidebarLayout(
                                           sidebarPanel(
                                             uiOutput('corr_factor'),
                                             uiOutput('corr_levels')
                                           ),
                                           mainPanel(
                                             plotOutput('corr_plot', height = '800px')
                                           )
                                         )
                        ),
                        conditionalPanel(condition = "input.corr_opts.indexOf('pairwise') != -1",
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
                      )),
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
