# Load packages ----
library(shiny)
library(tidyverse)
library(readr)
library(scales)
library(reshape2)
library(DT)
rm(list = ls())


source("scripts/ui.R")
source("scripts/server.R",local=TRUE)
source("scripts/process_data.R")
source("scripts/graphs.R")
source("scripts/data_output.R")



# Run app ----
shinyApp(ui, server)