################################################################################
# Library
################################################################################
# install.packages(c("mwshiny","shiny","shinydashboard","DT","shinyWidgets","colourpicker","shinyjs","shinycssloaders","writexl","openxlsx","ClustVarLV","ClustBlock","mwshiny","webshot","shinyBS","kableExtra","shiny))
library(dplyr)
library(shinydashboard)
library(DT)
library(plotly)
library(shinyWidgets)
library(colourpicker)
library(shinyjs)
library(shinycssloaders)
library(writexl)
library(openxlsx)
library(ClustVarLV)
library(ClustBlock)
library(car)
library(RColorBrewer)
library(ggplot2)
library(utf8)
library(ggdendro)
library(webshot)
library(kableExtra)
library(shinyBS)
library(shinycssloaders)
library(ggpubr)
library(htmltools)
library(htmlwidgets)
library(mwshiny)
# library(chemosensR)
library(rmarkdown)

library(chemosensR)

options(warn = -1, spinner.color="#0275D8", spinner.color.background="#ffffff", spinner.size=2)

################################################################################
# lexique
################################################################################

addResourcePath(prefix = 'pics', directoryPath = paste0(getwd(),'/www'))
#addResourcePath(prefix = 'pics', directoryPath = '~/www')

df <- data.frame(
    val = c("english","francais")
)

df$img = c(
    sprintf("<img src='pics/drapeauen.png' width=30px><div class='jhr'>%s</div></img>", df$val[1]),
    sprintf("<img src='pics/drapeaufr.png' width=30px><div class='jhr'>%s</div></img>", df$val[2])
)

lexi = read.csv(file = paste0(getwd(),'/www/dictionaryChemosensShiny.csv'), sep = ';', row.names = 1)

source(paste0(getwd(),'/ui.R'))
source(paste0(getwd(),'/server.R'))

#source('~/inst/extdata/shiny/ui.R')

################################################################################
# App
################################################################################

mwsApp(ui_win, serv_calc, serv_out)
