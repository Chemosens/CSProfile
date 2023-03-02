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
library(chemosensR)
library(rmarkdown)
library(shiny)
library(multcompView)
library(readxl)

addResourcePath(prefix = 'pics', directoryPath = 'www')

df <- data.frame(
    val = c("english","francais")
)

df$img = c(
    sprintf("<img src='pics/drapeauen.png' width=30px><div class='jhr'>%s</div></img>", df$val[1]),
    sprintf("<img src='pics/drapeaufr.png' width=30px><div class='jhr'>%s</div></img>", df$val[2])
)

lexi = read.csv(file = 'www/dictionaryChemosensShiny.csv', sep = ';', row.names = 1)

CustomHeader <- shinydashboard::dashboardHeader(
    title="ChemosensShiny",
    titleWidth=400
)

CustomHeader$children[[3]]$children <- list(
    div(style="width:130px;float:right;height:50px", pickerInput(inputId = "chL",
                                                                 label = NULL,
                                                                 choices = df$val,
                                                                 choicesOpt = list(content = df$img)))
)

ui <- dashboardPage(
    CustomHeader,
    dashboardSidebar(
        sidebarMenu(
            tags$head( # code HTML pour differents usages, generalement pour placer correctement les differents elements du shiny, par exemple navbar-nav pour distancier les onglets TDS ou les margin pour aligner les help avec l entree correspondante 
                tags$style(HTML('.jhr{
                                display: inline;
                                vertical-align: middle;
                                padding-left: 10px;
                                }
                                
                                .navbar-nav li:nth-child(2) {margin-left:40px}
                                .navbar-nav li:nth-child(5) {margin-left:40px}
                                .navbar-nav li:nth-child(6) {margin-left:40px}
                                
                                #presentationTDS{color: #337ab7;
                                 top: 100px;
                                 left: 0px;
                                 width: 100%;
                                 padding: 3% 3%;
                                 text-align: center;
                                 font-weight: normal;
                                 font-size: 300%;
                                 z-index: 105;
                                }
                                
                                #presentationFC{color: #337ab7;
                                 top: 100px;
                                 left: 0px;
                                 width: 100%;
                                 padding: 3% 3%;
                                 text-align: center;
                                 font-weight: normal;
                                 font-size: 300%;
                                 z-index: 105;
                                }
                                
                                #bsspecificWordsTabNameFC{margin-left:1px}
                                #bsspecificWordsTabNameFC{margin-top:25px}
                                #bssubject{margin-left:1px}
                                #bssubject{margin-top:25px}
                                #bsproduct{margin-left:1px}
                                #bsproduct{margin-top:25px}
                                #bsdescription{margin-left:1px}
                                #bsdescription{margin-top:25px}
                                #bscitation{margin-left:1px}
                                #bscitation{margin-top:25px}
                                #bsseparatorFC{margin-left:1px}
                                #bsseparatorFC{margin-top:25px}
                                #bsdata{margin-left:1px}
                                #bsdata{margin-top:1px}
                                #bslexique{margin-left:1px}
                                #bslexique{margin-top:1px}
                                #bsalFC{margin-left:1px}
                                #bsalFC{margin-top:25px}
                                #bsnbFC{margin-left:1px}
                                #bsnbFC{margin-top:25px}
                                #bsaxFC{margin-left:1px}
                                #bsaxFC{margin-top:25px}
                                #bschFC{margin-left:1px}
                                #bschFC{margin-top:25px}
                                #bsactionLexiconFC{margin-left:-75px}
                                #bsactionLexiconFC{margin-top:5px}
                                
                                #carteTDS { display:none; }
                                #bsdataTDS{margin-left:1px}
                                #bsdataTDS{margin-top:1px}
                                #bssubjectTDS{margin-left:1px}
                                #bssubjectTDS{margin-top:25px}
                                #bsproductTDS{margin-left:1px}
                                #bsproductTDS{margin-top:25px}
                                #bsdescriptorTDS{margin-left:1px}
                                #bsdescriptorTDS{margin-top:25px}
                                #bstimeTDS{margin-left:1px}
                                #bstimeTDS{margin-top:25px}
                                #bsscoreTDS{margin-left:1px}
                                #bsscoreTDS{margin-top:25px}
                                #bsreplicateTDS{margin-left:1px}
                                #bsreplicateTDS{margin-top:25px}
                                #bsseparatorTDS{margin-left:1px}
                                #bsseparatorTDS{margin-top:25px}
                                #bsstartWithFirstCitationTDS{margin-left:1px}
                                #bsstartWithFirstCitationTDS{margin-top:25px}
                                #bsdiscretizationTDS{margin-left:1px}
                                #bsdiscretizationTDS{margin-top:25px}
                                #bschTDS{margin-left:1px}
                                #bschTDS{margin-top:25px}
                                #bsalTDSana1{margin-left:1px}
                                #bsalTDSana1{margin-top:25px}
                                #bsalTDSana2{margin-left:1px}
                                #bsalTDSana2{margin-top:25px}
                                #bsalTDSana3{margin-left:1px}
                                #bsalTDSana3{margin-top:25px}
                                #bsclrepAsAna2{margin-left:1px}
                                #bsclrepAsAna2{margin-top:25px}
                                #bsclAna2st{margin-left:1px}
                                #bsclAna2st{margin-top:25px}
                                #bsa2sequenceStartTDS{margin-left:1px}
                                #bsa2sequenceStartTDS{margin-top:25px}
                                #bsa2sequenceDurationTDS{margin-left:1px}
                                #bsa2sequenceDurationTDS{margin-top:25px}
                                #bsa2nbDescriptorsTDS{margin-left:1px}
                                #bsa2nbDescriptorsTDS{margin-top:25px}
                                #bsa2nbClicksTDS{margin-left:1px}
                                #bsa2nbClicksTDS{margin-top:25px}
                                #bsa2descriptorDurationTDS{margin-left:1px}
                                #bsa2descriptorDurationTDS{margin-top:25px}
                                #bschMethod{margin-left:1px}
                                #bschMethod{margin-top:25px}
                                #bslisteDeroulanteTDS{margin-left:1px}
                                #bslisteDeroulanteTDS{margin-top:25px}'),
                           type="text/css",
                           ".shiny-output-error { visibility: hidden; }",
                           ".shiny-output-error:before { visibility: hidden; }",
                           "#loadmessage {
                                 position: fixed;
                                 top: 0%;
                                 left: 0%;
                                 width: 100%;
                                 padding: 25% 25%;
                                 text-align: center;
                                 font-weight: bold;
                                 font-size: 500%;
                                 color: #000000;
                                 background-clip: content-box;
                                 background-color: transparent;
                                 z-index: 105;}"
                )
                
            ),
            uiOutput('TDSmenuData'), # TDS entrees pour le pretraitement
            uiOutput('TDSmenuSeparator'),
            
            uiOutput('choiceMethod'),
            
            uiOutput('menuSubjectTDS'),
            uiOutput('menuProductTDS'),
            uiOutput('menuDescriptorTDS'),
            uiOutput('menuTimeTDS'),
            uiOutput('menuScoreTDS'),
            uiOutput('menuReplicateTDS'),
            
            uiOutput('menuListTDS'),
            uiOutput('TDSmenuStart'),
            uiOutput('TDSmenuDiscretization'),
            
            uiOutput('TDSmenuBouton'), # a changer
            
            conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                             tags$div(img(src='pics/loading-buffering.gif'),id="loadmessage")),
            
            conditionalPanel(condition="input.tabselected==7", # TDS onglet telechargement
                             uiOutput('titleRapportTDS'),
                             downloadButton("htmlTDS", "html"),
                             downloadButton("zipTDS", "zip")),
            
            conditionalPanel(condition="input.tabselected==2", # TDS onglet Panel Behavior
                             uiOutput('alphaAna1TDS'),
                             uiOutput('panelTablesequenceStartTDS'),
                             uiOutput('panelTablesequenceDurationTDS'),
                             uiOutput('panelTablenbDescriptorsTDS'),
                             uiOutput('panelTablenbClicksTDS'),
                             uiOutput('panelTabledescriptorDurationTDS'),
                             uiOutput('actionAna1TDS')),
            
            conditionalPanel(condition="input.tabselected==3", # TDS onglet Product temporality
                             uiOutput('alphaAnaTDS'),
                             uiOutput('repAsAna2'),
                             uiOutput('Ana2st'),
                             uiOutput('actionAna2TDS')),
            
            conditionalPanel(condition="input.tabselected==6", # TDS onglet Dominance Durations
                             uiOutput('alphaAna3TDS'),
                             uiOutput('actionAna3TDS')),
            
            conditionalPanel(condition="input.tabselected==4", # TDS onglet Custom Analysis
                             uiOutput('choiceTDS'),
                             uiOutput('a3boutonchTDS'),
                             uiOutput('c1boutonchTDS'),
                             uiOutput('a1boutonchTDS'),
                             uiOutput('a4boutonchTDS'),
                             uiOutput('b6AnaDiffDomCurvStTDS'),
                             uiOutput('b6alphachTDS'),
                             uiOutput('b6boutonchTDS'),
                             uiOutput('c2confintTDS'),
                             uiOutput('c2boutonchTDS'),
                             uiOutput('d1boutonchTDS'),
                             uiOutput('d2confintTDS'),
                             uiOutput('d2boutonchTDS'),
                             uiOutput('d56TDSrepch'),
                             uiOutput('d56boutonchTDSrep'),
                             uiOutput('d7TDSrepch'),
                             uiOutput('d7boutonchTDSrep'),
                             uiOutput('d34TDSrepch'),
                             uiOutput('d34boutonchTDSrep')
            ),
            
            uiOutput('FCmenuLexique'), # FC entrees pour le pretraitement
            uiOutput('FCmenuSpecific'),
            uiOutput('FCmenuSujet'),
            uiOutput('FCmenuProduit'),
            uiOutput('FCmenuDescription'),
            uiOutput('FCmenuCitation'),
            
            uiOutput('FCmenuBouton'),
            
            conditionalPanel(condition="input.tabselectedFC==2", # FC onglet summary
                             uiOutput('addLexicon'),
                             uiOutput('addLexicon2'),
                             uiOutput('boutonLexiconFC')),
            
            conditionalPanel(condition="input.tabselectedFC==1", # FC onglet Analysis
                             #uiOutput('productchoice'),
                             uiOutput('alphaFC'),              
                             uiOutput('nbootFC'),
                             uiOutput('choiceFC'),
                             uiOutput('axesFC'),
                             uiOutput('boutonAnalysisFC')),
            
            conditionalPanel(condition="input.tabselectedFC==5", # FC onglet telechargement
                             uiOutput('titleRapportFC'),
                             downloadButton("htmlFC", "html"),
                             downloadButton("zipFC", "zip"))
            
        )
    ),
    dashboardBody(
        useShinyjs(),
        uiOutput("bodyTDS"),
        uiOutput("bodyFC")
    )
)




