#global environment

library(shiny)
library(plotly)
library(DT)
library(shinythemes)

if (!exists("statedata"))
        statedata = readRDS("data/statedata.rds")
if (!exists("elections"))
        elections = readRDS("data/electiondata.rds")

mapcolors <- c("#808080", "#B2182B", "#8D2B4B", "#693F6B", "#45528B", "#2166AC")
