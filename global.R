library(shiny)
library(shinyWidgets)
require(bartCause)
require(ranger)
require(arm)
require(tidyverse)

# load R files 

source('R/dgp.R')
source('R/modeler.R')
source('R/ploter.R')

# init <- dgp(n = 500)
# write_csv(init, 'Data/sample_df.csv')

map(list.files('UI'), function(file) source(file.path("UI", file)))

ploter(modeler(dgp()))
dif