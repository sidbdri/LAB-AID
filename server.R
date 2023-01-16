library(shiny)
library(readxl)
library(tidyverse)
library(magrittr)
library(patchwork)
library(ComplexHeatmap)
library(shinyjs)
library(plotly)
library(shinyWidgets)
library(WriteXLS)
library(reshape2)
library(jsonlite)
library(lme4)
library(car)

source('busyIndicator.R')
source('summarySE.R')

shinyServer(function(input, output, session) {
  
  datasets <- jsonlite::fromJSON(txt = 'config.json')$Datasets
  

  # Loading selected dataset
  property.data <- reactive({
    fpath <- datasets %>% filter(Name == input$dataset) %>% pull(Path)
    nf <- datasets %>% filter(Name == input$dataset) %>% pull(n.Factors)
    if(str_detect(fpath, 'xlsx?')){
      d <- read_excel(fpath)
    }else if(str_detect(fpath, 'csv')){
      d <- read.csv(fpath, header = T)
    }
    
    d <- cbind(pID = (1:nrow(d)) + 1, d) # Adds pID column starting at 2
    d <- d %>% select(-c(which(colSums(is.na(d)) == nrow(d)))) # Removes all NA columns
    d <- d %>% mutate_at((1:nf) + 1, as.factor)
  })
  
  observeEvent(input$dataset,{
    updateTabsetPanel(session, inputId = 'viewTabs', selected = 'Selection')
  })
  
# Number of factors 
  
  n.factors <- reactive({
    nf <- datasets %>% filter(Name == input$dataset) %>% pull(n.Factors)
    nf + 1 
  })
  
  output$nf = renderText(n.factors())
  
  # UI element for averaging factors
  avg.select <- renderUI({
      selectInput(inputId = 'avg', label = 'Averaging factors:', choices = colnames(property.data())[2:n.factors()], multiple = T, colnames(property.data())[2:(n.factors() - 2)])
  })
  output$avg.select <- avg.select
  
# Exclusions
  observeEvent(input$dataset,{
   nf <- datasets %>% filter(Name == input$dataset) %>% pull(n.Factors) +1
   lapply(1:nf, function(x){
      cname <- reactive({colnames(property.data())[x]})
      if(x == 1){
        choices <- reactive({(property.data()[, x] %>% as.character %>% as.numeric %>% unique %>% sort %>% trunc)})
      }else{
        choices <- reactive({(property.data()[, x] %>% as.character %>% unique %>% sort)})
      }
      ui.element <- renderUI({
        pickerInput(inputId = str_c('exclusions.', cname()), label = cname(), choices = choices(), selected = choices(), multiple = T, options = list(`actions-box` = T, `live-search` = T))
      })
      output[[paste0('e_', x)]] <- ui.element
    })
   lapply((nf + 1):200, function(x){
     output[[paste0('e_', x)]] <- NULL
   }) 
  })

  
  # Exclusions UI
    ex.ui <- renderUI({
      lapply(1:n.factors(), function(x){
        uiOutput(paste0('e_', x))
      })
    })

  
  # Highlights 
  observeEvent(input$dataset,{
    nf <- datasets %>% filter(Name == input$dataset) %>% pull(n.Factors) +1
    lapply(1:nf, function(x){
      cname <- reactive({colnames(property.data())[x]})
      #choices <- reactive({(property.data()[, x] %>% as.character %>% unique %>% sort)})
      if(x == 1){
        choices <- reactive({(property.data()[, x] %>% as.character %>% as.numeric %>% unique %>% sort %>% trunc)})
      }else{
        choices <- reactive({(property.data()[, x] %>% as.character %>% unique %>% sort)})
      }
      ui.element <- renderUI({
        pickerInput(inputId = str_c('highlights.', cname()), label = cname(), choices = choices(), multiple = T, options = list(`actions-box` = T, `live-search` = T))
      })
      output[[paste0('h_', x)]] <- ui.element
    })
    lapply((nf + 1):200, function(x){
      output[[paste0('h_', x)]] <- NULL
    }) 
  })
  
  # Highlights UI
  hi.ui <- renderUI({
    lapply(1:n.factors(), function(x){
      uiOutput(paste0('h_', x))
    })
  })
  
  ## Functions for exclusions and highlights
  removeExclusions <- function(df){
    for(i in 1:18){
      df <- df %>% filter_at(i, all_vars((. %in% input[[paste0('exclusions.', colnames(property.data())[i])]])))
    }
    return(df)
  }
  
  filterData <- function(dfr){
    index <- rep(T, nrow(dfr))
    for(i in 1:n.factors()){
      #df <- df %>% filter_at(i, all_vars((. %in% input[[paste0('exclusions.', colnames(property.data())[i])]])))
      
      index <- index & (((dfr %>% pull(i)) %in% input[[paste0('exclusions.', colnames(property.data())[i])]]) | is.na(dfr %>% pull(i)))
    }
    #index <- as.logical(index)
    dfr <- dfr[index, ] %>% mutate(pID = as.integer(pID))
    return(dfr)
  }
  
  addHighlights <- function(dfr){
    h_vec <- rep(F, nrow(dfr))
    for(i in 1:n.factors()){
      h_vec <- h_vec | ((dfr %>% pull(i)) %in% input[[paste0('h_',i)]])
    }
    dfr <- dfr %>% mutate(highlight = h_vec)
    return(dfr)
  }
  
  ## Preview tab
  prev.factors <- reactive({colnames(property.data())[2:n.factors()]})
  output$prev.factors <- renderUI(selectInput('prev.factors', 'Select main effect:', choices = prev.factors(), selected = 'Genotype'))
  
  #filt.data <- reactive({removeExclusions(property.data())})
  filt.data <- reactive({filterData(property.data())})
  #hi.data <- reactive({addHighlights(filt.data())})
  
  output$test.table <- renderTable(filt.data())
  
  hl.pIDs <- reactive({
    p.IDs <- numeric()
    for(i in 1:n.factors()){
      p.IDs <- c(p.IDs, filt.data() %>% filter_at(i, all_vars(. %in% input[[paste0('highlights.', colnames(filt.data())[i])]])) %>% pull(pID)) %>% unique
    }
    p.IDs
  })
  

#  output$pIDs <- renderText(hl.pIDs())
#  output$animals <- renderText(hl.animals())
  
  out.vars <- reactive({colnames(property.data())[(n.factors() +1):ncol(property.data())]}) ### HARDCODED
  
  
  ###Preview plot
  prev.facs <- reactive({str_c('`', input$prev.factors, '`')})
  prev.plot <- reactive({
    prev.table <- melt(filt.data(), id.vars = colnames(filt.data()[1:n.factors()]))
    pl <- ggplot(prev.table, aes_string(x = prev.facs(), y = 'value')) + facet_wrap(~variable, scales = 'free') + theme_bw()
    if(input$prev.type == 'box'){
      pl <- pl + geom_boxplot(fill = 'gray87', outlier.shape = 0)
    }else{
      pl <- pl + geom_violin(fill = 'gray87')
    }
    
    hl <- prev.table$pID %in% hl.pIDs()
    
    tooltip.txt <- apply(prev.table[, 1:n.factors()], 1, function(x){
      paste(str_c(colnames(prev.table)[1:n.factors()], ': ', as.character(x), '<br>'), sep = '', collapse = '')
    })
    
    pl <- pl + geom_jitter(width = 0.2, height = 0, aes(text = tooltip.txt, colour = hl), alpha = 0.4) + scale_colour_manual(values = c('TRUE' = 'red', 'FALSE' = 'black', 'NA' = 'black')) +
      theme(legend.position="none", axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))
  
    ply <- list(plot = ggplotly(pl, tooltip = 'text') %>% layout(dragmode = "select", hoverlabel = list(font=list(size=10))), data = pl$data)
    
    #if(input$prev.type == 'box'){
    #  nvars <- prev.table %>% pull(variable) %>% unique %>% length
    #  for(i in 1:nvars){
    #    ply$plot$x$data[[i]]$marker$opacity = 0
    #  }
    #}
    
    ply
  })
  
  output$prev.plot <- renderPlotly(prev.plot()$plot)
  
  ### Main plot UI elements
  observeEvent(c(input$dataset, input$plot_data, input$avg),
    {
    lastPlotVar <- reactive({
      choice <- reactive({out.vars()})
      if(is.null(input$plot_out_var)){
        lpv <- choice()[1]
      }else{
        lpv <- input$plot_out_var
      }
      lpv
    })
    plot_out_var <-  renderUI({
      choice <- reactive({out.vars()})
      selectInput(inputId = 'plot_out_var', label = 'Output variable:', choices = choice(), selected = lastPlotVar())
    })
    output$plot_out_var <- plot_out_var
    
    lastPlotEffect <- reactive({
      if(is.null(input$plot_effect)){
        lpe <- intersect(colnames(property.data()[2:n.factors()]), input$avg)[1]
      }else{
        lpe <- input$plot_effect
      }
      lpe
    })
    
    plot_effect <- renderUI({
      choice <- reactive({
        if(input$plot_data == 'cell'){
          colnames(property.data())[2:n.factors()]
        }else if(input$plot_data == 'animal'){
          input$avg
        }
      })

      selectInput(inputId = 'plot_effect', label = 'Main effect:', choices = choice(), selected = lastPlotEffect())
    })
    output$plot_effect <- plot_effect
    
    lastPlotX <- reactive({
      if(is.null(input$plot_x)){
        lpx <- 'None'
      }else{
        lpx <- input$plot_x
      }
      lpx
    })
    
    plot_x <- renderUI({
      choice <- reactive({c('None', 
                            if(input$plot_data == 'cell'){
                              colnames(property.data())[2:n.factors()]
                            }else if(input$plot_data == 'animal'){
                              input$avg
                            }
                            )})
      selectInput(inputId = 'plot_x', label = 'X facet:', choices = choice(), selected = lastPlotX())
    })
    output$plot_x <- plot_x
    
    lastPlotY <- reactive({
      if(is.null(input$plot_y)){
        lpy <- 'None'
      }else{
        lpy <- input$plot_y
      }
      lpy
    })
    
    plot_y <- renderUI({
      choice <- reactive({c('None', 
                            if(input$plot_data == 'cell'){
                              colnames(property.data())[2:n.factors()]
                            }else if(input$plot_data == 'animal'){
                              input$avg
                            }
                            )})
      selectInput(inputId = 'plot_y', label = 'Y facet:', choices = choice(), selected = lastPlotY())
    })
    output$plot_y <- plot_y
  })
  
  addFaceting <- function(pl){
    if(input$plot_x != 'None' & input$plot_y == 'None'){
      form <- str_c('.~ `', input$plot_x, '`') %>% as.formula
      pl <- pl + facet_grid(form, switch = 'y', scales = 'free_x')
    }else if(input$plot_x == 'None' & input$plot_y != 'None'){
      form <- str_c('`', input$plot_y, '` ~.') %>% as.formula
      pl <- pl + facet_grid(form, switch = 'y', scales = 'free_x')
    }else if(input$plot_x != 'None' & input$plot_y != 'None'){
      form <- str_c('`', input$plot_y, '` ~ `', input$plot_x, '`') %>% as.formula
      pl <- pl + facet_grid(form, switch = 'y', scales = 'free_x')
    }
    return(pl)
  }
  
  # Modifier for plot height scaling based on y facets
  plotHeight <- reactive({
    ph <- 400
    if(input$plot_y != 'None'){
      nFacets <- filt.data() %>% pull(input$plot_y) %>% unique %>% length
      if(nFacets > 2){
        ph <- 200*nFacets
      }
    }
    str_c(ph, 'px')
  })
  
  ### Distribution plot
  
  dist.plot <- reactive({
    x.var <- str_c('`', input$plot_effect %>% as.character, '`')
    y.var <- str_c('`', input$plot_out_var %>% as.character, '`')
    
    if(input$plot_data == 'cell'){
      
      dplot <- ggplot(filt.data(), aes_string(x = x.var, y = y.var)) + theme_bw()
      
      gvar <- str_c('`', input$plot_effect, '`')
      if(input$plot_x != 'None'){
        gvar <- c(gvar, str_c('`', input$plot_x, '`'))
      }
      if(input$plot_y != 'None'){
        gvar <- c(gvar, str_c('`', input$plot_y, '`'))
      }
      
      sum.data <- summarySE(data = filt.data(), measurevar = input$plot_out_var,
                            groupvars = gvar,
                            na.rm = T)
      sum.data <- sum.data %>% mutate(lowerSE = sum.data[, input$plot_out_var] - se,
                                      upperSE = sum.data[, input$plot_out_var] + se,
                                      lowerCI = sum.data[, input$plot_out_var] - ci,
                                      upperCI = sum.data[, input$plot_out_var] + ci,
                                      lowerSD = sum.data[, input$plot_out_var] - sd,
                                      upperSD = sum.data[, input$plot_out_var] + sd)
      
      if(input$plot_dist_type == 'box'){
        dplot <- dplot + geom_boxplot(fill = 'gray87', outlier.shape = NA) 
      }else if(input$plot_dist_type == 'violin'){
        dplot <- dplot + geom_violin(trim = F, fill = 'gray87')
      }

      tooltip.txt <- apply(filt.data()[, 1:n.factors()], 1, function(x){
        paste(str_c(colnames(filt.data())[1:n.factors()], ': ', as.character(x), '<br>'), sep = '', collapse = '')
      })
      
      dplot$data$Info <- tooltip.txt
      if(length(hl.pIDs()) == 0){
        dplot <- dplot + geom_jitter(width = 0.2, height = 0, aes(text = Info), alpha = input$plot_dist_opacity)
      }else{
        dplot$data$hl <- as.numeric(filt.data()$pID) %in% hl.pIDs()
        dplot <- dplot + geom_jitter(width = 0.2, height = 0, aes(text = Info, colour = hl), alpha = input$plot_dist_opacity) + scale_colour_manual(values = c('TRUE' = 'red', 'FALSE' = 'black')) + theme(legend.position="none")
      }
      
      if(input$plot_dist_type == 'SEM'){
        dplot <- dplot + geom_pointrange(aes(ymin = lowerSE, ymax = upperSE),
                                         data = sum.data, width = 0.3, colour = 'red') + 
          geom_errorbar(aes(ymin = lowerSE, ymax = upperSE),
                        data = sum.data, width = 0.3, colour = 'red')
      }else if(input$plot_dist_type == 'CI'){
        dplot <- dplot + geom_pointrange(aes(ymin = lowerCI, ymax = upperCI),
                                         data = sum.data, width = 0.3, colour = 'red') + 
          geom_errorbar(aes(ymin = lowerCI, ymax = upperCI),
                        data = sum.data, width = 0.3, colour = 'red')
      }else if(input$plot_dist_type == 'SD'){
        dplot <- dplot + geom_pointrange(aes(ymin = lowerSD, ymax = upperSD),
                                         data = sum.data, width = 0.3, colour = 'red') + 
          geom_errorbar(aes(ymin = lowerSD, ymax = upperSD),
                        data = sum.data, width = 0.3, colour = 'red')
      }
      
    }else if(input$plot_data == 'animal'){
      
      
      a.hi.data <- filt.data() %>% group_by_at(match(input$avg, colnames(.))) %>% select((n.factors() + 1):ncol(.)) %>% summarize_all(mean, na.rm = T)
      
      gvar <- str_c('`', input$plot_effect, '`')
      if(input$plot_x != 'None'){
        gvar <- c(gvar, str_c('`', input$plot_x, '`'))
      }
      if(input$plot_y != 'None'){
        gvar <- c(gvar, str_c('`', input$plot_y, '`'))
      }
      
      sum.data <- summarySE(data = a.hi.data, measurevar = input$plot_out_var,
                            groupvars = gvar,
                            na.rm = T)
      sum.data <- sum.data %>% mutate(lowerSE = sum.data[, input$plot_out_var] - se,
                                      upperSE = sum.data[, input$plot_out_var] + se,
                                      lowerCI = sum.data[, input$plot_out_var] - ci,
                                      upperCI = sum.data[, input$plot_out_var] + ci,
                                      lowerSD = sum.data[, input$plot_out_var] - sd,
                                      upperSD = sum.data[, input$plot_out_var] + sd)
      
      tooltip.txt <- apply(a.hi.data[, 1:length(input$avg)], 1, function(x){
        paste(str_c(colnames(a.hi.data)[1:length(input$avg)], ': ', as.character(x), '<br>'), sep = '', collapse = '')
      })
      
      dplot <- ggplot(a.hi.data, aes_string(x = x.var, y = y.var)) + theme_bw()
      dplot$data$Info <- tooltip.txt
      

      if(input$plot_dist_type == 'box'){
        dplot <- dplot + geom_boxplot(fill = 'gray87', outlier.alpha = 0) 
      }else if(input$plot_dist_type == 'violin'){
        dplot <- dplot + geom_violin(trim = F, fill = 'gray87')
      }

      dplot <- dplot + geom_jitter(width = 0.2, height = 0, aes(text = Info), alpha = input$plot_dist_opacity)
      
      if(input$plot_dist_type == 'SEM'){
        dplot <- dplot + geom_pointrange(aes(ymin = lowerSE, ymax = upperSE),
                                         data = sum.data, width = 0.3, colour = 'red') + 
          geom_errorbar(aes(ymin = lowerSE, ymax = upperSE),
                        data = sum.data, width = 0.3, colour = 'red')
      }else if(input$plot_dist_type == 'CI'){
        dplot <- dplot + geom_pointrange(aes(ymin = lowerCI, ymax = upperCI),
                                         data = sum.data, width = 0.3, colour = 'red') + 
          geom_errorbar(aes(ymin = lowerCI, ymax = upperCI),
                        data = sum.data, width = 0.3, colour = 'red')
      }else if(input$plot_dist_type == 'SD'){
        dplot <- dplot + geom_pointrange(aes(ymin = lowerSD, ymax = upperSD),
                                         data = sum.data, width = 0.3, colour = 'red') + 
          geom_errorbar(aes(ymin = lowerSD, ymax = upperSD),
                        data = sum.data, width = 0.3, colour = 'red')
      }
    }
    dplot <- addFaceting(dplot)
    dplot <- dplot + theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))
    
    dplotly <- list(plot = ggplotly(dplot, tooltip = 'text') %>% layout(dragmode = "select", hoverlabel = list(font=list(size=10))), data = dplot$data)
    
    if(input$plot_dist_type == 'box'){
      nlayers <- length(dplotly$plot$x$data)/3
      for(i in 1:nlayers){
        dplotly$plot$x$data[[i]]$marker$opacity = 0
      }
    }
    
    dplotly
    
  })
  
  
  
  output$dist_plot <- renderPlotly(dist.plot()$plot)
  output$dist_plot_UI <- renderUI(
    plotlyOutput(outputId = 'dist_plot', height = plotHeight())
  )
  
  ### Errorbar plot
  
  bar_plot <- reactive({
    x.var <- str_c('`', input$plot_effect %>% as.character, '`')
    y.var <- str_c('`', input$plot_out_var %>% as.character, '`')
    
    if(input$plot_data == 'cell'){
      
      gvar <- str_c('`', input$plot_effect, '`')
      if(input$plot_x != 'None'){
        gvar <- c(gvar, str_c('`', input$plot_x, '`'))
      }
      if(input$plot_y != 'None'){
        gvar <- c(gvar, str_c('`', input$plot_y, '`'))
      }
      
      sum.data <- summarySE(data = filt.data(), measurevar = input$plot_out_var,
                            groupvars = gvar,
                            na.rm = T)
      sum.data <- sum.data %>% mutate(lowerSE = sum.data[, input$plot_out_var] - se,
                                      upperSE = sum.data[, input$plot_out_var] + se,
                                      lowerCI = sum.data[, input$plot_out_var] - ci,
                                      upperCI = sum.data[, input$plot_out_var] + ci,
                                      lowerSD = sum.data[, input$plot_out_var] - sd,
                                      upperSD = sum.data[, input$plot_out_var] + sd)
      
      
      bplot <- ggplot(sum.data, aes_string(x = x.var, y = y.var)) + theme_bw() + 
        geom_bar(aes_string(x = x.var, y = y.var), position_dodge(), stat = 'identity',
                 data = sum.data, width = 0.5, fill = 'grey', colour = 'darkgrey')
      
      if(input$bar_value == 'SEM'){
        bplot <- bplot + geom_errorbar(aes(ymin = lowerSE, ymax = upperSE),
                                       data = sum.data, width = 0.3)
      }else if(input$bar_value == 'CI'){
        bplot <- bplot + geom_errorbar(aes(ymin = lowerCI, ymax = upperCI),
                                       data = sum.data, width = 0.3)
      }else if(input$bar_value == 'SD'){
        bplot <- bplot + geom_errorbar(aes(ymin = lowerSD, ymax = upperSD),
                                       data = sum.data, width = 0.3)
      }
      
      if(all(na.omit(sum.data[, input$plot_out_var]) <= 0)){
        bplot <- bplot + scale_y_reverse()
      }
      
    }else if(input$plot_data == 'animal'){
      
      
      a.hi.data <- filt.data() %>% group_by_at(match(input$avg, colnames(.))) %>% select((n.factors() + 1):ncol(.)) %>% summarize_all(mean, na.rm = T)
      
      gvar <- input$plot_effect
      if(input$plot_x != 'None'){
        gvar <- c(gvar, input$plot_x)
      }
      if(input$plot_y != 'None'){
        gvar <- c(gvar, input$plot_y)
      }
      
      sum.data <- summarySE(data = a.hi.data, measurevar = input$plot_out_var,
                            groupvars = gvar, na.rm = T)
      sum.data <- sum.data %>% mutate(lowerSE = sum.data[, input$plot_out_var] - se,
                                      upperSE = sum.data[, input$plot_out_var] + se,
                                      lowerCI = sum.data[, input$plot_out_var] - ci,
                                      upperCI = sum.data[, input$plot_out_var] + ci,
                                      lowerSD = sum.data[, input$plot_out_var] - sd,
                                      upperSD = sum.data[, input$plot_out_var] + sd)
      
      
      bplot <- ggplot(sum.data, aes_string(x = x.var, y = y.var)) + theme_bw() + 
        geom_bar(aes_string(x = x.var, y = y.var), position_dodge(), stat = 'identity',
                 data = sum.data, width = 0.5, fill = 'grey', colour = 'darkgrey')
      
      if(input$bar_value == 'SEM'){
        bplot <- bplot + geom_errorbar(aes(ymin = lowerSE, ymax = upperSE), width = 0.3,
                                       position = position_dodge(0.9), data = sum.data)
      }else if(input$bar_value == 'CI'){
        bplot <- bplot + geom_errorbar(aes(ymin = lowerCI, ymax = upperCI), width = 0.3,
                                       position = position_dodge(0.9), data = sum.data)
      }else if(input$bar_value == 'SD'){
        bplot <- bplot + geom_errorbar(aes(ymin = lowerSD, ymax = upperSD), width = 0.3,
                                       position = position_dodge(0.9), data = sum.data)
      }
      
      if(all(na.omit(sum.data[, input$plot_out_var]) <= 0)){
        bplot <- bplot + scale_y_reverse()
      }
    }
    bplot <- addFaceting(bplot)
    bplot <- bplot + theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))
    
    bplot <- ggplotly(bplot)
    bplot
    
  })
  
  output$bar_plot <- renderPlotly(bar_plot())
  output$bar_plot_UI <- renderUI(
    plotlyOutput(outputId = 'bar_plot', height = plotHeight())
  )
  

  output$sum_data <- renderTable(sumSE_test())
  
  ### Histogram
  
  hist_plot <- reactive({

    fill.var <- input$plot_effect %>% as.character()
    m.var <- input$plot_out_var %>% as.character()
    
    if(input$plot_data == 'cell'){
      hplot <- ggplot(filt.data(), aes_string(x = as.name(m.var), fill = as.name(fill.var))) + theme_bw()
      
      if(input$plot_hist_type == 'bars'){
        hplot <- hplot + geom_histogram(aes(y =..density..), bins = input$plot_hist_bins)
      }else{
        hplot <- hplot + geom_density(alpha = 0.7)
      }
    }else if(input$plot_data == 'animal'){
      a.hi.data <- filt.data() %>% group_by_at(match(input$avg, colnames(.))) %>% select((n.factors() + 1):ncol(.)) %>% summarize_all(mean, na.rm = T)
      hplot <- ggplot(a.hi.data, aes_string(x = as.name(m.var), fill = as.name(fill.var))) + theme_bw()
      
      if(input$plot_hist_type == 'bars'){
        hplot <- hplot + geom_histogram(aes(y =..density..), bins = input$plot_hist_bins)
      }else{
        hplot <- hplot + geom_density(alpha = 0.7)
      }
    }
    
    hplot <- addFaceting(hplot)
    
    hplotly <- ggplotly(hplot)
    
    hplotly
  })
  output$hist_plot <- renderPlotly(hist_plot())
  output$hist_plot_UI <- renderUI(
    plotlyOutput(outputId = 'hist_plot', height = plotHeight())
  )
  
  ### Cumulative
  
  cumsum_plot <- reactive({
    fill.var <- input$plot_effect %>% as.character()
    m.var <- input$plot_out_var %>% as.character()
    
    if(input$plot_data == 'cell'){
      csplot <- ggplot(filt.data(), aes_string(x = as.name(m.var), colour = as.name(fill.var))) + stat_ecdf(geom = 'smooth') + theme_bw() + theme(text = element_text(size = 16), axis.title.y = element_blank())
    }else if(input$plot_data == 'animal'){
      a.hi.data <- filt.data() %>% group_by_at(match(input$avg, colnames(.))) %>% select((n.factors() + 1):ncol(.)) %>% summarize_all(mean, na.rm = T)
      csplot <- ggplot(a.hi.data, aes_string(x = m.var, colour = fill.var)) + stat_ecdf(geom = 'smooth') + theme_bw() + theme(text = element_text(size = 16), axis.title.y = element_blank())
    }
    csplot <- addFaceting(csplot)
    
    csplot
  })
  
  output$cumsum_plot <- renderPlot(cumsum_plot())
  output$cumsum_plot_UI <- renderUI(
    plotOutput(outputId = 'cumsum_plot', height = plotHeight())
  )
  
  # Correlation heatmaps
  
  var.warn <- reactive({
    if(ncol(filt.data()) - n.factors() <= 1){
      warn <- 'Correlation module requires more than one output variable.'
    }else{
      warn <- ''
    }
    warn
  })
  output$var_warn <- renderUI(helpText(var.warn()))

  
  output$corr_factor <- renderUI(
   selectInput(inputId = 'corr_factor', choices = c('All data' = 'None', colnames(property.data())[2:n.factors()]), label = 'Select effect:', selected = 'None')
  )
  
  output$corr_factor.p <- renderUI(
    selectInput(inputId = 'corr_factor.p', choices = c('All data' = 'None', colnames(property.data())[2:n.factors()]), label = 'Select effect:', selected = 'None')
  )
  

  corr_levels <- reactive({
    #if(input$corr_factor != 'None'){
    if(!('None' %in% input$corr_factor)){
      lvls <- filt.data() %>% pull(input$corr_factor) %>% unique
    }else{
      lvls <- 'All data'
    }
    lvls
  })
  
  output$corr_levels <- renderUI(
    selectInput(inputId = 'corr_levels', choices = corr_levels(), label = 'Select level:')
  )
  
  corr_levels.p <- reactive({
    if(input$corr_factor.p != 'None'){
      lvls <- filt.data() %>% pull(input$corr_factor.p) %>% unique
    }else{
      lvls <- 'All data'
    }
    lvls
  })
  
  output$corr_levels.p1 <- renderUI(
    selectInput(inputId = 'corr_levels.p1', choices = corr_levels.p(), label = 'Select level:')
  )
  output$corr_levels.p2 <- renderUI(
    selectInput(inputId = 'corr_levels.p2', choices = corr_levels.p(), label = 'Select level:')
  )
  
  hm.plot <- reactive({
    hm.data <- property.data()
    
    if(!('None' %in% input$corr_factor)){
      hm.data <- hm.data[hm.data[, input$corr_factor] == input$corr_levels, ] %>% select_at((n.factors()+1):(ncol(hm.data))) %>% select_if(~!all(is.na(.))) %>% as.matrix
    }else{
      hm.data <- hm.data %>% select_at((n.factors()+1):(ncol(hm.data))) %>% select_if(~!all(is.na(.))) %>% as.matrix
    }
    
    corrfun <- function(x, y){
      m <- hm.data[,c(x, y)] %>% na.omit
      r <- cor(m[, 1], m[, 2])
      return(r)
    }
    corr.data <- outer(colnames(hm.data), colnames(hm.data), Vectorize(corrfun))
    colnames(corr.data) <- colnames(hm.data)
    rownames(corr.data) <- colnames(hm.data)
    
    if(!('None' %in% input$corr_factor)){
      ttl = str_c(input$corr_factor, ' ', input$corr_levels)
    }else{
      ttl <- 'All data'
    }

    if('clustered' %in% input$corr_opts){
      pheatmap(corr.data, fontsize = 14, display_numbers = T, main = ttl, legend=F, column_names_max_height=unit(100, "cm"), row_names_max_width=unit(100, "cm"))
    }else{
      pheatmap(corr.data, fontsize = 14, display_numbers = T, main = ttl, cluster_rows = F, cluster_cols = F, legend=F, column_names_max_height=unit(100, "cm"), row_names_max_width=unit(100, "cm"))
    }
  })
  
  output$corr_plot <- renderPlot(hm.plot())
  
  hm.plot.p1 <- reactive({
    req(input$corr_factor.p, input$corr_levels.p1)
    hm.data <- filt.data()

    if(input$corr_factor.p != 'None'){
      hm.data <- hm.data[hm.data[, input$corr_factor.p] == input$corr_levels.p1, ] %>% select_at((n.factors()+1):(ncol(hm.data))) %>% select_if(~!all(is.na(.))) %>% as.matrix
    }else{
      hm.data <- hm.data %>% select_at((n.factors()+1):(ncol(hm.data))) %>% select_if(~!all(is.na(.))) %>% as.matrix
    }
    
    corrfun <- function(x, y){
      m <- hm.data[,c(x, y)] %>% na.omit
      r <- cor(m[, 1], m[, 2])
      return(r)
    }
    corr.data <- outer(colnames(hm.data), colnames(hm.data), Vectorize(corrfun))
    colnames(corr.data) <- colnames(hm.data)
    rownames(corr.data) <- colnames(hm.data)
    
    if(input$corr_factor.p != 'None'){
      ttl1 <- str_c(input$corr_factor.p, ' ', input$corr_levels.p1)
      ttl2 <- str_c(input$corr_factor.p, ' ', input$corr_levels.p2)
    }else{
      ttl1 <- 'All data'
      ttl2 <- 'All data'
    }
    
    if('clustered' %in% input$corr_opts.p){
      pheatmap(corr.data, fontsize = 14, display_numbers = T, main = ttl1, legend=F, column_names_max_height=unit(100, "cm"), row_names_max_width=unit(100, "cm"))
    }else{
      pheatmap(corr.data, fontsize = 14, display_numbers = T, main = ttl1, cluster_rows = F, cluster_cols = F,legend=F, column_names_max_height=unit(100, "cm"), row_names_max_width=unit(100, "cm"))
    }
  })
  
  output$corr_plot.p1 <- renderPlot(hm.plot.p1())
  
  hm.plot.p2 <- reactive({
    req(input$corr_factor.p, input$corr_levels.p2)
    hm.data <- property.data()

    if(input$corr_factor.p != 'None'){
    hm.data <- hm.data[hm.data[, input$corr_factor.p] == input$corr_levels.p2, ] %>% select_at((n.factors()+1):(ncol(hm.data))) %>% select_if(~!all(is.na(.))) %>% as.matrix
    }else{
      hm.data <- hm.data %>% select_at((n.factors()+1):(ncol(hm.data))) %>% select_if(~!all(is.na(.))) %>% as.matrix
    }
    
    if(input$corr_factor.p != 'None'){
      ttl1 <- str_c(input$corr_factor.p, ' ', input$corr_levels.p1)
      ttl2 <- str_c(input$corr_factor.p, ' ', input$corr_levels.p2)
    }else{
      ttl1 <- 'All data'
      ttl2 <- 'All data'
    }
    
    corrfun <- function(x, y){
      m <- hm.data[,c(x, y)] %>% na.omit
      r <- cor(m[, 1], m[, 2])
      return(r)
    }
    corr.data <- outer(colnames(hm.data), colnames(hm.data), Vectorize(corrfun))
    colnames(corr.data) <- colnames(hm.data)
    rownames(corr.data) <- colnames(hm.data)
    
    if('clustered' %in% input$corr_opts.p){
      pheatmap(corr.data, fontsize = 14, display_numbers = T, main = ttl2, legend=F, column_names_max_height=unit(100, "cm"), row_names_max_width=unit(100, "cm"))
    }else{
      pheatmap(corr.data, fontsize = 14, display_numbers = T, main = ttl2, cluster_rows = F, cluster_cols = F, legend=F, column_names_max_height=unit(100, "cm"), row_names_max_width=unit(100, "cm"))
    }
  })
  
  output$corr_plot.p2 <- renderPlot(hm.plot.p2())
  
  ## Quick download
  qDown.table <- reactive({
    qdt <- filt.data() %>% select(c(input$plot_out_var, input$plot_effect))
    if(input$plot_x != 'None'){
      qdt  <- cbind(qdt, filt.data() %>% select(input$plot_x))
    }
    if(input$plot_y != 'None'){
      qdt  <- cbind(qdt, filt.data() %>% select(input$plot_y))
    }
    qdt
  })
  
  output$quickDownload <- downloadHandler(filename = 'plotData.xlsx', content = function(file){
    d <- qDown.table()
    WriteXLS::WriteXLS(x = d, ExcelFileName = file, AdjWidth = T, FreezeRow = 1)
  })
  
  
  
  ## Downloads table
  
  factor_input <- reactive({
    colnames(filt.data())[1:(n.factors())]
  })
  
  variable_input <- reactive({
    colnames(filt.data())[(n.factors()+1):ncol(filt.data())]
  })
  
  output$factor_select <- renderUI(multiInput(inputId = 'factor_select', 'Select factors:', choices = factor_input()[-1], width = '600px', selected = factor_input(), options = list(enable_search = F)))
  output$variable_select <- renderUI(multiInput(inputId = 'variable_select', 'Select variables:', choices = variable_input(), width = '600px', selected = variable_input(), options = list(enable_search = F)))
  
  down.table <- reactive({
    filt.data() %>% select(c(input$factor_select, input$variable_select))
  })
  
  output$downloadData <- downloadHandler(filename = 'ephys_download.xlsx', content = function(file){
    d <- down.table()
    WriteXLS::WriteXLS(x = d, ExcelFileName = file, AdjWidth = T, FreezeRow = 1)
  })
  
  output$downloadDataCSV <- downloadHandler(filename = 'ephys_download.csv', content = function(file){
    d <- down.table()
    write.csv(d, file = file, quote = F, col.names = T, row.names = F)
  })
  
  output$down.table <- DT::renderDataTable(down.table(), server = T, filter = 'top', escape = F, selection = 'none', rownames = F)
  
  ### Configuration
  config <- jsonlite::fromJSON(txt = 'config.json')
  
  ### Function to update restart.txt when config files are changed
  updateRestart <- function(){
    write('', file = 'restart.txt')
  }
  
  ## App title and description
  observeEvent(input$app_save, {
    if(config$Title != input$app_name){
      config$Title <- input$app_name
    }
    if(config$About != input$app_desc){
      config$About <- input$app_desc
    }
    config %>% jsonlite::toJSON(pretty = T) %>% write(file = 'config.json')
    config <<- config
    sendSweetAlert(session = session, title = 'Configuration', text = 'Saved.', btn_labels = 'Confirm', type = 'success')
    updateRestart()
  })
  
  ##Remove dataset
  output$remove_ds_ui <- renderUI(selectInput(inputId = 'rm_select', 'Select dataset to remove:', choices = config$Datasets$Name, multiple = F))
  observeEvent(input$ds_remove, {
    if(nrow(config$Datasets) == 1){
      sendSweetAlert(session = session, title = 'Error', text = "Unable to remove. Minimum one dataset required.", btn_labels = 'Confirm', type = 'error')
    }else{
      config$Datasets <- config$Datasets %>% filter(Name != input$rm_select)
      config %>% jsonlite::toJSON(pretty = T) %>% write(file = 'config.json')
      output$remove_ds_ui <<- renderUI(selectInput(inputId = 'rm_select', 'Select dataset to remove:', choices = config$Datasets$Name, multiple = F))
      config <<- config
      updateSelectInput(session = session, inputId = 'rm_select', 'Select dataset to remove:', choices = jsonlite::fromJSON(txt = 'config.json')$Datasets$Name)
      updateSelectInput(session = session, inputId = 'dataset', label = 'Dataset:', choices = jsonlite::fromJSON(txt = 'config.json')$Datasets$Name, selected = jsonlite::fromJSON(txt = 'config.json')$Datasets$Name[1])
      datasets <<- jsonlite::fromJSON(txt = 'config.json')$Datasets
      sendSweetAlert(session = session, title = 'Success!', text = str_c(input$rm_select, " dataset removed."), btn_labels = 'Confirm', type = 'success')
      updateRestart()
    }

  })
  
  ##Add dataset
  observeEvent(input$ds_submit, {
    if(input$ds_factors == '' | input$ds_name == ''){
      sendSweetAlert(session = session, title = 'Error', text = "'Dataset name' and 'Number of factors' must be specified.", btn_labels = 'Confirm', type = 'error')
    }else{
      withBusyIndicatorServer('ds_submit',{
        ds_fname <- input$add_file$name
        file.copy(from = input$add_file$datapath, to = str_c('data/', ds_fname))
        config$Datasets <- config$Datasets %>% rbind(c(input$ds_name, str_c('data/', ds_fname), input$ds_factors, input$ds_desc)) %>% mutate(n.Factors = as.numeric(n.Factors))
        config %>% jsonlite::toJSON(pretty = T) %>% write(file = 'config.json')
        output$remove_ds_ui <<- renderUI(selectInput(inputId = 'rm_select', 'Select dataset to remove:', choices = config$Datasets$Name))
        config <<- config
        updateSelectInput(session = session, inputId = 'rm_select', 'Select dataset to remove:', choices = jsonlite::fromJSON(txt = 'config.json')$Datasets$Name)
        updateSelectInput(session = session, inputId = 'dataset', label = 'Dataset:', choices = jsonlite::fromJSON(txt = 'config.json')$Datasets$Name, selected = jsonlite::fromJSON(txt = 'config.json')$Datasets$Name[1])
        datasets <<- jsonlite::fromJSON(txt = 'config.json')$Datasets
        sendSweetAlert(session = session, title = 'Success!', text = str_c(input$ds_name, ' dataset added.'), btn_labels = 'Confirm', type = 'success')
        updateRestart()
      })
    }
  })
  
  ## Update dataset
  output$ds_update_select <- renderUI({
    selectInput(inputId = 'ds_update_name', label = 'Select dataset to update:', choices = datasets$Name, selected = datasets$Name[1], multiple = F)
  })
  observeEvent(input$ds_update, {
    withBusyIndicatorServer('ds_update', {
      old.file <- config$Datasets %>% filter(Name == input$ds_update_name) %>% pull(Path)
      new.file <- input$update_file$name
      file.rename(from = old.file, to = str_c('Archive_', format(Sys.time(), "%a%b%d%Y_%H%M", old.file)))
      file.copy(from = input$update_file$datapath, to = str_c('data/', new.file))
      config$Datasets[config$Datasets$Name == input$ds_update_name, 'Path'] <- str_c('data/', new.file)
      config %>% jsonlite::toJSON(pretty = T) %>% write(file = 'config.json')
      config <<- config
      updateSelectInput(session = session, inputId = 'rm_select', 'Select dataset to remove:', choices = jsonlite::fromJSON(txt = 'config.json')$Datasets$Name)
      updateSelectInput(session = session, inputId = 'dataset', label = 'Dataset:', choices = jsonlite::fromJSON(txt = 'config.json')$Datasets$Name, selected = jsonlite::fromJSON(txt = 'config.json')$Datasets$Name[1])
      datasets <<- jsonlite::fromJSON(txt = 'config.json')$Datasets
      sendSweetAlert(session = session, title = 'Success!', text = str_c(input$ds_update_name, ' dataset updated'), btn_labels = 'Confirm', type = 'success')
      updateRestart()
    })
  })
  
  ## Basic LMM
  lmm.vars <- reactive({
    colnames(filt.data())[(n.factors() + 1):ncol(filt.data())]
  })
  lmm.factors <- reactive({
    colnames(filt.data())[1:n.factors()]
  })
  output$lmm_vars_select <- renderUI({
    selectInput(inputId = 'lmm_var', label = 'Select variable:', choices = lmm.vars(), selected = lmm.vars()[1], multiple = F)
  })
  output$lmm_factor_select <- renderUI({
    selectInput(inputId = 'lmm_factor', label = 'Select main factor:', choices = lmm.factors(), selected = lmm.factors()[1], multiple = F)
  })
  output$lmm_random_select <- renderUI({
    multiInput(inputId = 'lmm_random', 'Select random factors:', choices = lmm.factors(), width = '400px', selected = tail(lmm.factors(), 2), options = list(enable_search = F))
  })
  lmm.qqp <- reactive({
    shiny::validate(need(input$lmm_var, message=FALSE))
    
    if(input$lmm_logtrans){
      lmm.variable <- log10((filt.data() %>% pull(input$lmm_var) %>% na.omit ) +1)
    }else{
      lmm.variable <- filt.data()%>% pull(input$lmm_var) %>% na.omit 
    }
    qqp(lmm.variable, main = input$lmm_var)
  })
  output$lmm_qqp <- renderPlot(lmm.qqp())
  
  output$log_error <- reactive({
    shiny::validate(need(input$lmm_var, message=FALSE))
    cond <- any((filt.data() %>% pull(input$lmm_var)) < 0)
    return(cond)
  })
  output$log_warning <- renderText({
    paste("WARNING: Variable contains negative values", "Log-transformation will cause errors.", sep="\n")
  })
  
  outputOptions(output, "log_error", suspendWhenHidden = FALSE)
  
  lmm.funct <- reactive({
    shiny::validate(need(input$lmm_var, message=FALSE))
    
    if(length(input$lmm_random) > 0){
      rand.factors <- sapply(input$lmm_random, function(x)str_c("`", x, "`"))
      rand.factors <- paste(rand.factors, sep = ':', collapse = ':')
      if(input$lmm_logtrans){
        form <- str_c("log10(`", input$lmm_var, "`) ~ `", input$lmm_factor, "` + (1|", rand.factors, ")")
      }else{
        form <- str_c("`", input$lmm_var, "` ~ `", input$lmm_factor, "` + (1|", rand.factors, ")")
      }
      full.lm <- lmer(formula = form, data = filt.data())
    }else{
      if(input$lmm_logtrans){
        form <- str_c("log10(`", input$lmm_var, "`) ~ `", input$lmm_factor, "`")
      }else{
        form <- str_c("`", input$lmm_var, "` ~ `", input$lmm_factor, "`")
      }
      full.lm <- lm(form, data = filt.data())
    }
    full.lm
  })
  
  lmm.sum <- reactive({
    shiny::validate(need(input$lmm_var, message=FALSE))
    
    summary(lmm.funct())
  })
  
  lmm.Anova <- reactive({
    shiny::validate(need(input$lmm_var, message=FALSE))
    
    if(length(input$lmm_random) > 0){
      lmm.an <- Anova(lmm.funct())
    }else{
      lmm.an <- aov(lmm.funct())
    }
    lmm.an
  })
  
  rand.text <- reactive({
    f <- sapply(input$lmm_random, function(x)str_c("`", x, "`"))
    paste(f, sep = ':', collapse = ':')
  })
  output$rand_text <- renderText(input$lmm_random %>% class)
  output$lmm_summary <- renderPrint(lmm.sum())
  output$lmm_Anova <- renderPrint(lmm.Anova())
})



