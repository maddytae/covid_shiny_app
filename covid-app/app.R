# Load packages ----
library(shiny)
library(tidyverse)
library(readr)
library(scales)
library(reshape2)
rm(list = ls())


source("scripts/graphs.R")
source("scripts/ui.R")
source("scripts/server.R",local=TRUE)
source("scripts/process_data.R")





# Run app ----
shinyApp(ui, server)