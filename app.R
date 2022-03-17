# Launch ------------------------------------------------------------------
library(shiny)
library(shinyWidgets)
library(htmltools)
library(DT)
library(dplyr)

# get smart filter funs/modules
source("./modules/smartFilterModules.R")


# create data set with enough categorical columns for filters
indata <- data.frame(
  class = c(
    rep("mammel",50)
    , rep("reptile", 30)
    , rep("amphibian",20)
  )
  , animal = c(
    rep("dog", 20)
    , rep("bear", 10)
    , rep("rabbit", 10)
    , rep("cat", 10)
    , rep("lizzard", 10)
    , rep("turtle", 10)
    , rep("alligator", 10)
    , rep("frog", 10)
    , rep("newt", 10)
  )
  , is_furry = c(
    rep("yes",50)
    , rep("no", 50)
  )
  , diet = c(
    rep("omnivore", 30)
    , rep("herbivore",10)
    , rep("carnivore", 10)
    , rep("carnivore", 10)
    , rep("omnivore", 10)
    , rep("carnivore", 30)
  )
  , sightings = round(
    runif(100, 1, 100)
  )
  , photos = round(
    runif(100, 10, 50)
  )
) %>%
  arrange(desc(sightings))



# UI ----------------------------------------------------------------------

ui <- fluidPage(
  fluidRow(
    
    # column for wellpanel (and inputs)
    column(
      width = 4
      # wellpanel for inputs
      , wellPanel(
        # smart Filter Input
        smartFilterInput(
          id = "s1"
        )
        , HTML("<br>")
        , downloadButton(
          outputId = 'downloadData'
          , label = 'Download'
        )
      )
    )
    # column for data table
    , column(
      width = 8
      , HTML("<br>")
      # data table output
      , DTOutput("testtable")
    )
    
  )
)


# Server ------------------------------------------------------------------

server <- function(input, output, session){

  #### start up variables - not reactive ####
  
  # these fields can be filtered
  poss_fields <- c("class", "animal", "is_furry", "diet")
  
  # these fields cannot be filtered
  req_fields <- c("sightings", "photos")

  #### reactivity begins ####
  
  ## smart filter module - just drives the filters
  callModule(
    module = smartFilterServer
    , id = "s1"
    , df = indata
    , poss_fields = poss_fields
  )
  
  ## smart df module - filtered by smartFilterServer
  ## this is the filtered dataframe that can be used however needed
  sfdf <- callModule(
    module = smartDfServer
    , id = "s1"
    , df = indata
    , poss_fields = poss_fields
    , req_fields = req_fields
    , show_fields = "all"
  )
  
  # create DT data table
  output$testtable <- renderDT(
    sfdf()
    , selection="none"
    , rownames=FALSE
    , class= "cell-border compact stripe"
    , options = list(
      pageLength = 25
    )
  )
  
  # download handler
  output$downloadData <- downloadHandler(
    filename = function(){
      paste0(
        "mydata"
        , gsub("-","",Sys.Date())
        , '.csv'
        , sep=''
      )
    }
    , content = function(file1) {
      write.csv(
        x = sfdf()
        , file = file1
        , row.names=FALSE
      )
    }
  )


}

# call app--------------------------------------------------------------------
shinyApp(ui=ui, server=server)




