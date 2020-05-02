library(tidyverse)
library(promises)
library(future)
plan(multiprocess)

# bring in our custom functions
source('./functions.R')


tagList(
  div(class='card mr-1 mb-3 px-0 d-flex flex-column',
      div(class='card-header',
        h4('Heat Map')
      ),
      div(class='card-body px-3',
        plotOutput('pitchPlot')
        )
      )
)







