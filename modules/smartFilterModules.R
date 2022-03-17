# helper functions --------------------------------------------------------

# function to wrap text with "select 'x'(s)"
wrapselect <- function(x){
  paste0(
    "Select "
    , x
    , "(s)"
  )
}

# function to eval(parse(text))
evalparset <- function(text){
  eval(
    parse(
      text = text
    )
  )
}


# smartFilter module UI ---------------------------------------------------
# UI - displays filter(s) availaible

smartFilterInput <- function(id){
  
  library(shiny)
  library(shinyWidgets)
  library(htmltools)
  
  ns <- NS(id)
  
  tagList(
    uiOutput(ns("selectfields"))
    , HTML("<br>")
    , uiOutput(ns("fieldfilters"))
  )
  
}


# smartFilter module Server -----------------------------------------------
# Server - generates/updates 'smartFilterInput' filter options & actual filters based on df & poss_fields

smartFilterServer <- function(input, output, session, df, poss_fields){
  
  library(shiny)
  library(shinyWidgets)
  library(htmltools)
  library(shinyWidgets)
  
  # select fields to work with
  output$selectfields <- renderUI({
    
    pickerInput(
      inputId = session$ns("selectfieldsin")
      , label = "Select Fields to Include"
      , choices = poss_fields
      , multiple = TRUE
      , selected = poss_fields[1]
      , options = list(
        `actions-box` = TRUE
      )
    )
    
  })
  
  # distinct attributes in selected fields
  dist_atts <- reactive({
    
    req(input$selectfieldsin)
    
    df %>%
      select(input$selectfieldsin) %>%
      distinct()
    
  })
  
  # filter check/save list, must use $list sub element
  fcsl <- reactiveValues(l = list())
  
  # field filters
  output$fieldfilters <- renderUI({
    
    ## update elements in fcsl based on selections in input$selectfieldsin
    isolate(
      if( length(fcsl$l) >=1 ){
        # names in fcsl
        nifcsl <- names(fcsl$l)
        
        j <- 1
        while( j <= length(nifcsl) ){
          
          if( !nifcsl[j] %in% input$selectfieldsin ){
            fcsl$l[[nifcsl[j]]] <- NULL
          }
          j <- j+1
        }
      }
    )
    
    ## begin new filter work
    req(dist_atts)
    df <- dist_atts()
    allfields <- input$selectfieldsin
    fl <- tagList() # html filter list
    
    i <- 0
    while(i <= length(allfields)){
      
      ## first filter
      if(i == 1){
        
        # generate first selection
        initselect <- df %>%
          select(allfields[i]) %>%
          distinct() %>%
          pull()
        
        # EXISTS == FALSE & i == 1
        if( isolate( exists( x = allfields[i], where = fcsl$l )!=TRUE ) ){
          
          # create the filter
          fl[[i]] <- pickerInput(
            inputId = session$ns(allfields[i])
            , label = wrapselect(allfields[i])
            , choices = initselect
            , selected = initselect[1]
            , multiple = TRUE
            , options = list(
              `actions-box` = TRUE
            )
          )
          
          # create allfields[i] entry in fcsl with selection as contents
          
          isolate(
            fcsl$l[[allfields[i]]] <- eval(
              parse(
                text = paste0("input$", allfields[i])
              )
            )
          )
          
          # EXISTS == TRUE & i == 1
        } else if( isolate( exists( x = allfields[i], where = fcsl$l )==TRUE ) ){
          
          # save current value in filter to fcsl
          isolate(
            fcsl$l[[allfields[i]]] <- eval(
              parse(
                text = paste0("input$", allfields[i])
              )
            )
          )
          
          # recreate the filter with saved selections
          fl[[i]] <- pickerInput(
            inputId = session$ns(allfields[i])
            , label = wrapselect(allfields[i])
            , choices = initselect
            , selected = isolate(fcsl$l[[allfields[i]]])
            , multiple = TRUE
            , options = list(
              `actions-box` = TRUE
            )
            
          )
          
        }
        # All subsequent filters # i > 1
      } else if(i > 1){
        
        ## generate possible selections
        cfname <- allfields[i] # current field (cf)
        pfname <- allfields[i-1] # previous field (pf)
        pfvalue <- eval(
          parse(
            text = paste0("input$",pfname)
          )
        )
        # statement to evaluate
        stmnt <- paste0(
          "df %>% filter("
          , pfname
          , " %in% c('"
          , paste(
            pfvalue
            , collapse = "','"
          )
          , "'))"
        )
        
        # parse statement for previous filter
        df <- eval(
          parse(text = stmnt)
        )
        
        # possible choices based on previous filter
        cfchoices <- df %>%
          select(cfname) %>%
          distinct() %>%
          pull()
        
        # EXISTS == FALSE & i > 1
        if( isolate( exists( x = allfields[i], where = fcsl$l )!=TRUE ) ){
          
          # save current possible choices to fcsl
          isolate(
            fcsl$l[[allfields[i]]] <- cfchoices
          )
          
          # create the filter
          fl[[i]] <- pickerInput(
            inputId = session$ns(allfields[i])
            , label = wrapselect(allfields[i])
            , choices = cfchoices
            , selected = cfchoices[1]
            , multiple = TRUE
            , options = list(
              `actions-box` = TRUE
            )
          )
          
          # EXISTS == TRUE & i > 1
        } else if( isolate( exists( x = allfields[i], where = fcsl$l )==TRUE ) ){
          
          # save current value in filter to fcsl
          isolate(
            fcsl$l[[allfields[i]]] <- eval(
              parse(
                text = paste0("input$", allfields[i])
              )
            )
          )
          
          # recreate the filter with saved selections
          fl[[i]] <- pickerInput(
            inputId = session$ns(allfields[i])
            , label = wrapselect(allfields[i])
            , choices = cfchoices
            , selected = isolate(fcsl$l[[allfields[i]]])
            , multiple = TRUE
            , options = list(
              `actions-box` = TRUE
            )
          )
          
        }
        
      }
      i <- i+1
    }
    return(fl)
  })
  
}


# smartDf module Server ---------------------------------------------------
# Server - constructs & evaluates dplyr statement to a dataframe
# based on filters created with smartFilterServer & smartFilterInput (requires both)
# must be called in same namespace as smartFilterServer & smartFilterInput
# show_fields can either be "all", or "spec"
# "spec" will only include poss_fields selected to be filtered

smartDfServer <- function(input, output, session, df, poss_fields, req_fields, show_fields = "all"){
  
  # reactive data frame - filtered by smart filters
  rdf <- reactive({
    
    req(input$selectfieldsin)
    
    curfields <- input$selectfieldsin
    
    # list for named filter fields & selected values (l$"name":"yada","yada1")
    lnff <- list()
    
    for(i in 1:length(curfields)){
      
      lnff[[curfields[i]]] <- eval(
        parse(
          text = paste0("input$",curfields[i])
        )
      )
      
    }
    
    # statement objects list
    stmnt <- list()
    
    # fields to select - based on show_fields ( . = "all" or "spec")
    stmnt$fts <- if( show_fields == "all"  ){
      names(df)
    } else if( show_fields == "spec" ){
      c(
        curfields
        , req_fields
      )
    }
    
    # select statement
    stmnt$select <- paste0(
      "df %>% select("
      , paste0(
        stmnt$fts
        , collapse = ", "
      )
      , ")"
    )
    
    # individual filter statement(s) list
    filtlist <- list()
    
    for(j in 1:length(curfields)){
      
      filtlist[[curfields[j]]] <- paste0(
        "%>% filter( "
        , curfields[j]
        , " %in% c('"
        , paste0(lnff[[curfields[j]]], collapse = "','")
        , "'))"
      )
      
    }
    
    # combined results of 'filtlist' go in stmnt$filters
    stmnt$filters <- paste(filtlist, collapse = " ")
    
    # full statement, select + filters
    stmnt$full <- paste(
      stmnt$select
      , stmnt$filters
      , collapse = " "
    )
    
    df2 <- eval(
      parse(
        text = stmnt$full
      )
    )
    
    return(df2)
    
  })
  
  return( reactive({ rdf() }) )
  
}



















