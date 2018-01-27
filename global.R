#global environment

library(shiny)
library(plotly)
library(DT)

if (!exists("statedata"))
        statedata = readRDS("data/statedata.rds")
if (!exists("elections"))
        elections = readRDS("data/electiondata.rds")

mapcolors <- c("#808080",get_palette(c("#b2182b","#2166ac"), 5))
