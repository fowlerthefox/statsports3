library(tidyverse)
library(promises)
library(future)
plan(multiprocess)

# bring in our custom functions
source('./functions.R')


tagList(
  div(class='card-body px-3',
      plotOutput('pitchPlot')
  )
)