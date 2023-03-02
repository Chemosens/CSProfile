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

lexi = read.csv(file = 'www/dictionaryChemosensShiny.csv', sep = ';', row.names = 1) # lecture du lexique pour les mots francais ou anglais



# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    # les fonctions reactive et eventReactive sont utilisees pour stocker des variables
    
    choiceL <- reactive({ # fonction reactive pour le choix de la langue
        out <- 'en'
        if (input$chL == 'francais' | input$chL == 'english'){
            if (input$chL == 'francais') {
                out = 'fr'
            }
            else if (input$chL == 'english') {
                out = 'en'
            }
        }
        out
    })
    
    output$choiceMethod <- renderUI({ # entree du choix de la methode
        div(
            div(style="width:80%; display:inline-block; vertical-align: middle;",
                selectInput('chMethod', label = lexi['chMethod', choiceL()], choices = c(' ','TDS','FC'))
            ),
            div(style="display:inline-block; vertical-align: middle;",
                bsButton("bschMethod", label = "", icon = icon("question"), style = "info", size = "extra-small", placement = 'left'),
                shinyBS::bsTooltip("bschTDS", lexi['bschMethod', choiceL()])
            )
        )
    })
    
    lectureAll <- eventReactive(input$dataTDS,{ # fonction reactive de lecture du jeu de donnees, elle permet d avoir un vecteur des noms de colonne du jeu de donnees
        lecture = readXlsxOrCsv(input$dataTDS$datapath, sep = input$separatorTDS)
        lecture_table <- names(lecture)
        lecture_table
    })
    
    sheetsExcel <- eventReactive(input$lexique,{ # fonction reactive de la lecture du lexique FC, elle permet d avoir un vecteur des noms des onglets d un fichier xlsx
        lecture = excel_sheets(input$lexique$datapath)
        lecture
    })
    
    # calcul pour FC
    
    lexFC <- eventReactive(input$actionFC | input$actionLexiconFC,{ # lexique pour le pretraitement du fc
        a = read.xlsx(input$lexique$datapath, sheet = input$specificWordsTabNameFC)
        a
    })
    
    Act_table <- eventReactive(input$actionFC | input$actionLexiconFC,{ # donnees pretraitement fc, on cache ensuite les entrees utilisees pour faire ce pretraitement
        if (input$actionLexiconFC){
            tryCatch({
                fc = fcRead(file=input$dataTDS$datapath, sep=input$separatorTDS, cols=list(subject=input$subject, product=input$product, text=input$description),pathToLexicon=input$lexique$datapath, specificWordsTabName=input$specificWordsTabNameFC, minCitations=input$citation)
                fc = addSpecificLexiconEntry(fc, input$wordLexicon, input$lemmaLexicon)
                fc
            },
            error = function(err){
                showNotification(paste0(err), type = 'err')
            })
        }
        else{
            tryCatch({
                fc = fcRead(file=input$dataTDS$datapath, sep=input$separatorTDS, cols=list(subject=input$subject, product=input$product, text=input$description),pathToLexicon=input$lexique$datapath, specificWordsTabName=input$specificWordsTabNameFC, minCitations=input$citation)
                shinyjs::hide('lexique')
                shinyjs::hide('specificWordsTabNameFC')
                shinyjs::hide('subject')
                shinyjs::hide('product')
                shinyjs::hide('description')
                shinyjs::hide('citation')
                shinyjs::hide('actionFC')
                shinyjs::hide('bsspecificWordsTabNameFC')
                shinyjs::hide('bssubject')
                shinyjs::hide('bsproduct')
                shinyjs::hide('bsdescription')
                shinyjs::hide('bscitation')
                shinyjs::hide('bslexique')
                shinyjs::hide('dataTDS')
                shinyjs::hide('separatorTDS')
                shinyjs::hide('chMethod')
                shinyjs::hide('bsdataTDS')
                shinyjs::hide('bsseparatorTDS')
                shinyjs::hide('bschMethod')
                fc
            },
            error = function(err){
                showNotification(paste0(err), type = 'err')
            })
        }
    })
    
    #Act_table <- eventReactive(input$actionLexiconFC,{
    #    fc=fcRead(file=input$dataTDS$datapath, sep=input$separatorTDS, cols=list(subject=input$subject, product=input$product, text=input$description),pathToLexicon=input$lexique$datapath, specificWordsTabName=input$specificWordsTabNameFC, minCitations=input$citation)
    #    fc = addSpecificLexiconEntry(fc, input$wordLexicon, input$lemmaLexicon)
    #    fc
    #})
    
    Act_tableFCAnalyse <- eventReactive(input$actionAnalysisFC,{ # analyse du fc, on fait ine boucle if si l utilisateur beut les axes significatifs ou non
        tryCatch({
            fco=fcRead(file=input$dataTDS$datapath, sep=input$separatorTDS, cols=list(subject=input$subject, product=input$product, text=input$description),pathToLexicon=input$lexique$datapath, specificWordsTabName=input$specificWordsTabNameFC, minCitations=input$citation)
            if (input$axFC == 'signif'){
                r=analysis(fco,type="MRCA", title="MR CA", nBoot=input$nbFC, alpha=input$alFC, nbAxes=input$axFC, twoSided=FALSE, choice=input$chFC)
            }
            else {
                axes = as.numeric(input$axFC)
                r=analysis(fco,type="MRCA", title="MR CA", nBoot=input$nbFC, alpha=input$alFC, nbAxes=axes, twoSided=FALSE, choice=input$chFC)
            }
            graphics.off()
            r
        },
        error = function(err){
            showNotification(paste0(err), type = 'err')
        })
    })
    
    nbProd <- eventReactive(input$actionFC,{ # valeur du nombre de produit du TDS
        nbProd = nlevels(Act_table()$contingencyTable[,2])
        nbProd
    })
    
    nbSub <- eventReactive(input$actionFC,{ # valeur du nombre de sujet du TDS
        nbSub = nlevels(Act_table()$contingencyTable[,1])
        nbSub
    })
    
    # pour les calculs, les fonctions observeEvent sont utilises pour lancer les calculs avant que les fonctions d affichage appellent les fonctions reactives, ca empeche un bug qui est de rendre les tailles de graphe nulle
    
    # calcul pour TDS
    
    lectureTDS <- eventReactive(input$dataTDS,{ # lecture du jeu de donnees TDS, plus utilisee
        read.csv(input$dataTDS$datapath, sep = input$separatorTDS)
    })
    
    ActTDS <- eventReactive(input$actionTDS,{ # pretraitement des donnees TDS, on cache ensuite avec la fonction hide les entrees du pretraitement
        tryCatch({
            if ((isTruthy(lectureAll() == "SubjectCode") & isTruthy(lectureAll() == "ProductCode") & isTruthy(lectureAll() == "AttributeCode") & isTruthy(lectureAll() == "Time") & isTruthy(lectureAll() == "Score") & isTruthy(lectureAll() == "Replicate")) == FALSE){
                tds = tdsRead(file=input$dataTDS$datapath, cols=list(subject=input$subjectTDS, product=input$productTDS, descriptor=input$descriptorTDS,time=input$timeTDS,score=input$scoreTDS,rep=input$replicateTDS), supCols="", sep=input$separatorTDS,startWithFirstCitation=input$startWithFirstCitationTDS,discretization=input$discretizationTDS,periods=1)
            }
            else{
                tds = tdsRead(file=input$dataTDS$datapath, cols=list(subject="SubjectCode", product="ProductCode", descriptor="AttributeCode",time="Time",score="Score",rep="Replicate"), supCols="", sep=input$separatorTDS,startWithFirstCitation=input$startWithFirstCitationTDS,discretization=input$discretizationTDS,periods=1)
            }
            shinyjs::hide('dataTDS')
            shinyjs::hide('separatorTDS')
            shinyjs::hide('startWithFirstCitationTDS')
            shinyjs::hide('discretizationTDS')
            shinyjs::hide('bsdataTDS')
            shinyjs::hide('bsseparatorTDS')
            shinyjs::hide('bsstartWithFirstCitationTDS')
            shinyjs::hide('bsdiscretizationTDS')
            shinyjs::hide('actionTDS')
            shinyjs::hide('chMethod')
            shinyjs::hide('bschMethod')
            shinyjs::hide('subjectTDS')
            shinyjs::hide('productTDS')
            shinyjs::hide('descriptorTDS')
            shinyjs::hide('timeTDS')
            shinyjs::hide('scoreTDS')
            shinyjs::hide('replicateTDS')
            shinyjs::hide('bssubjectTDS')
            shinyjs::hide('bsproductTDS')
            shinyjs::hide('bsdescriptorTDS')
            shinyjs::hide('bstimeTDS')
            shinyjs::hide('bsscoreTDS')
            shinyjs::hide('bsreplicateTDS')
            tds
        },
        warning = function(warn){
            showNotification(paste0(warn), type = 'warning', duration = NULL)
        },
        error = function(err){
            showNotification(paste0(err), type = 'err', duration = NULL)
        })
    })
    
    observeEvent(input$a1actionchTDS,{ # les fonctions observeEvent sont des fonctions declenchant un evenement, ici on calcule les analyses avant la fonction eventReactive correspondante pour des soucis de sorties graphiques, si on ne le fait pas, les sorties plot seront de taille 0x0
        req(a1TDS())
    }, ignoreInit = TRUE, ignoreNULL = TRUE)
    
    a1TDS <- eventReactive(input$a1actionchTDS,{
        tryCatch({
            a1 = analysis(ActTDS(),type="Panel behaviour distribution", title="Panel behaviour distribution")
            graphics.off()
            a1
        },
        error = function(err){
            showNotification(paste0(err), type = 'err')
        })
    })
    
    
    observeEvent(input$actAna1TDS,{
        req(a2TDS())
        req(a3TDS())
    }, ignoreInit = TRUE, ignoreNULL = TRUE)
    
    # calcul Panelist table Panelist Behavior
    
    a2TDS <- eventReactive(input$actAna1TDS,{ 
        tryCatch({
            a2 = analysis(ActTDS(),type="Panel behaviour table", title="Panel behaviour table", sequenceStart = input$a2sequenceStartTDS, sequenceDuration = input$a2sequenceDurationTDS, nbDescriptors = input$a2nbDescriptorsTDS, nbClicks = input$a2nbClicksTDS, descriptorDuration = input$a2descriptorDurationTDS)
            graphics.off()
            a2
        },
        error = function(err){
            showNotification(paste0(err), type = 'err')
        })
    })
    
    # calcul custom analysis
    
    observeEvent(input$a4actionchTDS,{
        req(a4TDS())
    }, ignoreInit = TRUE, ignoreNULL = TRUE)
    
    a4TDS <- eventReactive(input$a4actionchTDS,{
        tryCatch({
            a4 = analysis(ActTDS(),type="Clusters of subjects durations", title="Clusters of subjects durations")
            graphics.off()
            a4
        },
        error = function(err){
            showNotification(paste0(err), type = 'err')
        })
    })
    
    # calcul Individual sequences Product Temporality
    
    b1TDS <- eventReactive(input$actAna2TDS,{
        tryCatch({
            b1=analysis(ActTDS(),type="Individual sequences", title="Individual sequences")
            graphics.off()
            b1
        },
        error = function(err){
            showNotification(paste0(err), type = 'err')
        })
    })
    
    # calcul Dominance curves Product Temporality
    
    b23TDS <- eventReactive(input$actAna2TDS,{
        tryCatch({
            if (input$clAna2st == TRUE){
                b23 = analysis(ActTDS(),type="Dominance curves", title="Dominance curves", alpha=input$alTDSana2, repAsIndividual=input$repAsAna2, rows="product", cols="rep", color="descriptor", smooth=TRUE, draw=c("significance","hasard","graymask"))
            }
            else {
                b23 = analysis(ActTDS(),type="Standardized dominance curves", title="Dominance curves", alpha=input$alTDSana2, repAsIndividual=input$repAsAna2, rows="product", cols="rep", color="descriptor", smooth=TRUE, draw=c("significance","hasard","graymask"))
            }
            graphics.off()
            b23
        },
        error = function(err){
            showNotification(paste0(err), type = 'err')
        })
    })
    
    # calcul Panel Bandplot Product Temporality
    
    b89TDS <- eventReactive(input$actAna2TDS,{
        tryCatch({
            if (input$clAna2st == TRUE){
                b89=analysis(ActTDS(),type="Panel sequences", title="Panel bandplot", alpha=input$alTDSana2, repAsIndividual=input$repAsAna2)
            }
            else {
                b89=analysis(ActTDS(),type="Standardized panel sequences", title="Panel bandplot", alpha=input$alTDSana2, repAsIndividual=input$repAsAna2)
            }
            graphics.off()
            b89
        },
        error = function(err){
            showNotification(paste0(err), type = 'err')
        })
    })
    
    # calcul ANOVA of behaviours Panelist Behavior
    
    a3TDS <- eventReactive(input$actAna1TDS,{
        tryCatch({
            a3=analysis(ActTDS(),type="ANOVA of behaviours", title="ANOVA of behaviours", columns=c("G_Mean","F_Product","P_Product","SE_Product","R2","P_ShapiroWilks","P_Levene_Product","Mean_Product","Group_Product"), alpha=input$alTDSana1)
            graphics.off()
            a3
        },
        error = function(err){
            showNotification(paste0(err), type = 'err')
        })
    })
    
    # calcul custom analysis
    
    observeEvent(input$c1actionchTDS,{
        req(c1TDS())
    }, ignoreInit = TRUE, ignoreNULL = TRUE)
    
    c1TDS <- eventReactive(input$c1actionchTDS,{
        tryCatch({
            c1=analysis(ActTDS(),type="Citation distribution", title="Citation distribution")
            graphics.off()
            c1
        },
        error = function(err){
            showNotification(paste0(err), type = 'err')
        })
    })
    
    observeEvent(input$chTDS,{
        updateTabsetPanel(session = getDefaultReactiveDomain(), "carteTDS",
                          selected = input$chTDS)
    }, ignoreInit = TRUE, ignoreNULL = TRUE)
    
    observeEvent(input$actAna3TDS,{
        req(d56TDS())
        req(d3TDS())
        req(d7TDS())
    }, ignoreInit = TRUE, ignoreNULL = TRUE)
    
    # calcul PCA of durations Dominance Durations
    
    d56TDS <- eventReactive(input$actAna3TDS,{
        tryCatch({
            d56=analysis(ActTDS(),type="PCA of durations", title="PCA", fontSizeCex = 2)
            graphics.off()
            d56
        },
        error = function(err){
            showNotification(paste0(err), type = 'err')
        })
    })
    
    # calcul ANOVA of durations Dominance Durations
    
    d3TDS <- eventReactive(input$actAna3TDS,{
        tryCatch({
            d3=analysis(ActTDS(),type="ANOVA of durations", title="ANOVA of durations", columns=c("G_Mean","F_Product","P_Product","F_Product_Significance","SE_Product","R2","P_ShapiroWilks","P_Levene_Product","Mean_Product","Group_Product","P_Subject"), alpha=input$alTDSana3)
            graphics.off()
            d3
        },
        error = function(err){
            showNotification(paste0(err), type = 'err')
        })
    })
    
    # calcul CVA of durations Dominance Durations
    
    d7TDS <- eventReactive(input$actAna3TDS,{
        tryCatch({
            d7=analysis(ActTDS(),type="CVA of durations", title="CVA", fontSizeCex = 2)
            graphics.off()
            d7
        },
        error = function(err){
            showNotification(paste0(err), type = 'err')
        })
    })
    
    # calcul PCA of trajectories Product temporality
    
    d9TDS <- eventReactive(input$actAna2TDS,{
        tryCatch({
            d9=analysis(ActTDS(),type="PCA of trajectories", title="PCA of trajectories", axes=list(c(1,2)), periods=7, fontSizeCex = 2)
            graphics.off()
            d9
        },
        error = function(err){
            showNotification(paste0(err), type = 'err')
        })
    })
    
    observeEvent(input$actAna2TDS,{
        req(b23TDS())
        req(b89TDS())
        req(d9TDS())
        req(b1TDS())
    }, ignoreInit = TRUE, ignoreNULL = TRUE)
    
    # calcul custom analysis
    
    observeEvent(input$b6actionchTDS,{
        req(b6TDS())
    }, ignoreInit = TRUE, ignoreNULL = TRUE)
    
    b6TDS <- eventReactive(input$b6actionchTDS,{
        tryCatch({
            if (input$clAnaDiffDomCurvSt == TRUE){
                b6=analysis(ActTDS(),type="Differences of dominance curves", title="Difference curves", alpha=input$b6alpha, repAsIndividual=TRUE, rows="product", cols="rep", color="descriptor", smooth=TRUE)
            }
            else {
                b6=analysis(ActTDS(),type="Standardized differences of dominance curves", title="Difference curves",  alpha=input$b6alpha, repAsIndividual=TRUE, rows="product", cols="rep", color="descriptor", smooth=TRUE)
            }
            graphics.off()
            b6
        },
        error = function(err){
            showNotification(paste0(err), type = 'err')
        })
    })
    
    # calcul custom analysis
    
    observeEvent(input$c2actionchTDS,{
        req(c2TDS())
    }, ignoreInit = TRUE, ignoreNULL = TRUE)
    
    c2TDS <- eventReactive(input$c2actionchTDS,{
        tryCatch({
            c2=analysis(ActTDS(),type="Barplot of citations", title="Barplot of citations", confInt=input$TDSconfintc2, errorBars="CI")
            graphics.off()
            c2
        },
        error = function(err){
            showNotification(paste0(err), type = 'err')
        })
    })
    
    # calcul custom analysis
    
    observeEvent(input$d1actionchTDS,{
        req(d1TDS())
    }, ignoreInit = TRUE, ignoreNULL = TRUE)
    
    d1TDS <- eventReactive(input$d1actionchTDS,{
        tryCatch({
            d1=analysis(ActTDS(),type="Duration distribution", title="Duration distribution",plot=c("violin","boxplot","jitter"))
            graphics.off()
            d1
        },
        error = function(err){
            showNotification(paste0(err), type = 'err')
        })
    })
    
    # calcul custom analysis
    
    observeEvent(input$d2actionchTDS,{
        req(d2TDS())
    }, ignoreInit = TRUE, ignoreNULL = TRUE)
    
    d2TDS <- eventReactive(input$d2actionchTDS,{
        tryCatch({
            d2=analysis(ActTDS(),type="Barplot of durations", title="Barplot of durations", confInt=input$TDSconfintd2, errorBars="CI")
            graphics.off()
            d2
        },
        error = function(err){
            showNotification(paste0(err), type = 'err')
        })
    })
    
    # calcul custom analysis
    
    observeEvent(input$d56actionchTDSrep,{
        req(d56TDSRep())
    }, ignoreInit = TRUE, ignoreNULL = TRUE)
    
    d56TDSRep <- eventReactive(input$d56actionchTDSrep,{
        tryCatch({
            d56rep=analysis(ActTDS(),type="PCA of durations", title="PCA", runBy="rep", fontSizeCex = 2)
            graphics.off()
            d56rep
        },
        error = function(err){
            showNotification(paste0(err), type = 'err')
        })
    })
    
    # calcul custom analysis
    
    d34TDS <- eventReactive(input$d34actionchTDSrep,{
        tryCatch({
            d34=analysis(ActTDS(),type="ANOVA of durations", title="ANOVA of durations by rep", runBy="rep", columns=c("G_Mean","F_Product","P_Product","F_Product_Significance","SE_Product","R2","P_ShapiroWilks","P_Levene_Product","Mean_Product","Group_Product","P_Subject"), alpha=input$alphaAnovaTDSrep)
            graphics.off()
            d34
        },
        error = function(err){
            showNotification(paste0(err), type = 'err')
        })
    })
    
    # calcul custom analysis
    
    observeEvent(input$d7actionchTDSrep,{
        req(d7TDSRep())
    }, ignoreInit = TRUE, ignoreNULL = TRUE)
    
    d7TDSRep <- eventReactive(input$d7actionchTDSrep,{
        tryCatch({
            d7rep=analysis(ActTDS(),type="CVA of durations", title="CVA", runBy="rep", fontSizeCex = 2)
            graphics.off()
            d7rep
        },
        error = function(err){
            showNotification(paste0(err), type = 'err')
        })
    })
    
    # dans les fonctions de parametre des noms de colonne du jeu de donnees, ils afficheront les parametres seulement s il n'a pas un prenom basique comme "sujet", sinon le parametre sera une entree de texte cache avec le nom de la colonne 
    
    # server pour FC
    
    output$presentationFC <- renderText({ # affichage du texte affichant le nombre de produit sujet
        if (choiceL() == 'en'){
            sub = 'subject'
            prod = 'product'
            andet = 'and'
        }
        else{
            sub = 'sujet'
            prod = 'produit'
            andet = 'et'
        }
        if (nlevels(Act_table()$rawData$subject) > 1){
            presSubFC <- paste(sub,'s',sep='')
        }
        else{
            presSubFC <- sub
        }
        if (nlevels(Act_table()$rawData$product) > 1){
            presProdFC <- paste(prod,'s',sep='')
        }
        else{
            presProdFC <- prod
        }
        paste(as.character(nlevels(Act_table()$rawData$subject)), presSubFC, andet, as.character(nlevels(Act_table()$rawData$product)), presProdFC)
    })
    
    output$htmlFC <- downloadHandler( # les boutons de telechargement permettant de telecharger les sorties graphiques, ici c est celui du rapport global TDS
        filename = function() {
            "report.html"
        },
        content = function(file) {
            
            tempReport <- file.path("www/reportFC.Rmd")
            file.copy("reportFC.Rmd", tempReport, overwrite = TRUE)
            
            if (isTruthy(input$actionAnalysisFC)){
                table = kable_styling(Act_tableFCAnalyse()[[1]]$output$testPerCell)
                graphique = Act_tableFCAnalyse()[[1]]$output$biplot
            }
            else{
                
                fco=fcRead(file=input$dataTDS$datapath, sep=input$separatorTDS, cols=list(subject=input$subject, product=input$product, text=input$description),pathToLexicon=input$lexique$datapath, specificWordsTabName=input$specificWordsTabNameFC, minCitations=input$citation)
                r=analysis(fco,type="MRCA", title="MR CA", nBoot=100, alpha=0.05, nbAxes="signif", twoSided=FALSE, choice="percent.cont")
                graphics.off()
                
                table = kable_styling(r[[1]]$output$testPerCell)
                graphique = r[[1]]$output$biplot
            }
            
            params <- list(
                titleRFC = input$titleFC,
                tablefc = table,
                graphiquefc = graphique
            )
            
            rmarkdown::render(tempReport, output_file = file,
                              params = params,
                              envir = new.env(parent = globalenv())
            )
            
        })
    
    output$zipFC <- downloadHandler( # telechargement d un fichier zip contenant les sorties TDS
        filename = function() {
            "FC.zip"
        },
        content = function(fname) {     #'percent.cont','original.cont','null.cont','p.values','derived.cont','percent.derived.cont'
            
            fs <- 1:3
            fs[1] <- paste0(tempdir(),"\\analysis.html")
            fs[2] <- paste0(tempdir(),"\\analysis.csv")
            fs[3] <- paste0(tempdir(),"\\axes.png")
            
            if (isTruthy(input$actionAnalysisFC)){
                a1 = kable_styling(Act_tableFCAnalyse()[[1]]$output$testPerCell)
                if (input$chFC == 'percent.cont'){
                    a2 = Act_tableFCAnalyse()[[1]]$testPerCell$percent.cont
                }
                else if(input$chFC == 'original.cont'){
                    a2 = Act_tableFCAnalyse()[[1]]$testPerCell$original.cont
                }
                else if(input$chFC == 'null.cont'){
                    a2 = Act_tableFCAnalyse()[[1]]$testPerCell$null.cont
                }
                else if(input$chFC == 'p.value'){
                    a2 = Act_tableFCAnalyse()[[1]]$testPerCell$p.value
                }
                else if(input$chFC == 'derived.cont'){
                    a2 = Act_tableFCAnalyse()[[1]]$testPerCell$derived.cont
                }
                else if(input$chFC == 'percent.derived.cont'){
                    a2 = Act_tableFCAnalyse()[[1]]$testPerCell$percent.derived.cont
                }
                a3 = Act_tableFCAnalyse()[[1]]$output$biplot
            }
            else{
                fco=fcRead(file=input$dataTDS$datapath, sep=input$separatorTDS, cols=list(subject=input$subject, product=input$product, text=input$description),pathToLexicon=input$lexique$datapath, specificWordsTabName=input$specificWordsTabNameFC, minCitations=input$citation)
                r=analysis(fco,type="MRCA", title="MR CA", nBoot=100, alpha=0.05, nbAxes="signif", twoSided=FALSE, choice="percent.cont")
                graphics.off()
                
                a1 = kable_styling(r[[1]]$output$testPerCell)
                a2 = r[[1]]$testPerCell$percent.cont
                a3 = r[[1]]$output$biplot
            }
            save_kable(a1, file = fs[1])
            write.csv2(a2, file = fs[2], row.names = FALSE)
            ggsave(fs[3], a3)
            zip::zipr(zipfile=fname, files=fs)
        }, contentType = "application/zip")
    
    
    output$titleRapportFC <- renderUI({ # FC nom du rapport html
        textInput('titleFC', lexi['titleRapportFC', choiceL()], value = 'Report FC')
    })
    
    output$FCmenuBouton <- renderUI({ # FC entree pour lancer le pretraitement des donnees FC
        if (input$chMethod == "FC"){
            actionButton(inputId = 'actionFC', lexi['fcstart', choiceL()])
        }
    })
    
    output$FCmenuCitation <- renderUI({ # FC entree du nombre minimum d un mot pour le garder
        if (input$chMethod == "FC"){
            div(
                div(style="width:80%; display:inline-block; vertical-align: middle;",
                    numericInput('citation', lexi['fccitation', choiceL()], value = 5, min = 1)
                ),
                div(style="display:inline-block; vertical-align: middle;",
                    bsButton("bscitation", label = "", icon = icon("question"), style = "info", size = "extra-small", placement = 'left'),
                    shinyBS::bsTooltip("bscitation", lexi['bsfccitation', choiceL()], placement =  "right", options=list(container="body"))
                )
            )
        }
    })
    
    output$FCmenuDescription <- renderUI({ # FC entree du choix de la colonne description, il ne s affiche pas si son nom est basique
        if (input$chMethod == "FC"){
            if ('description' %in% lectureAll()){
                hidden(
                    textInput('description', label = NULL, value = 'description')
                )
            }
            else if ('Description' %in% lectureAll()){
                hidden(
                    textInput('description', label = NULL, value = 'Description')
                )
            }
            else {
                div(
                    div(style="width:80%; display:inline-block; vertical-align: middle;",
                        selectInput('description',
                                    lexi['fcdescription', choiceL()],
                                    choices = lectureAll()[lectureAll()!= 'sujet' & lectureAll()!= 'Sujet' & lectureAll()!= 'subject' & lectureAll()!= 'Subject'
                                                           & lectureAll()!= 'produit' & lectureAll()!= 'Produit' & lectureAll()!= 'product' & lectureAll()!= 'Product'],
                                    multiple = FALSE)
                    ),
                    div(style="display:inline-block; vertical-align: middle;",
                        bsButton("bsdescription", label = "", icon = icon("question"), style = "info", size = "extra-small", placement = 'left'),
                        shinyBS::bsTooltip("bsdescription", lexi['bsfcdescription', choiceL()], placement =  "right", options=list(container="body"))
                    )
                )
            }
        }
    })
    
    output$FCmenuProduit <- renderUI({ # FC entree du choix de la colonne produit, il ne s affiche pas si son nom est basique
        if (input$chMethod == "FC"){
            if ('product' %in% lectureAll()){
                hidden(
                    textInput('product', label = NULL, value = 'product')
                )
            }
            else if ('Product' %in% lectureAll()){
                hidden(
                    textInput('product', label = NULL, value = 'Product')
                )
            }
            else if ('produit' %in% lectureAll()){
                hidden(
                    textInput('product', label = NULL, value = 'produit')
                )
            }
            else if ('Produit' %in% lectureAll()){
                hidden(
                    textInput('product', label = NULL, value = 'Produit')
                )
            }
            else {
                div(
                    div(style="width:80%; display:inline-block; vertical-align: middle;",
                        selectInput('product',
                                    lexi['fcproduct', choiceL()],
                                    choices = lectureAll()[lectureAll()!= 'sujet' & lectureAll()!= 'Sujet' & lectureAll()!= 'subject' & lectureAll()!= 'Subject'
                                                           & lectureAll()!= 'description' & lectureAll()!= 'Description'],
                                    multiple = FALSE)
                        
                    ),
                    div(style="display:inline-block; vertical-align: middle;",
                        bsButton("bsproduct", label = "", icon = icon("question"), style = "info", size = "extra-small", placement = 'left'),
                        shinyBS::bsTooltip("bsproduct", lexi['bsfcproduct', choiceL()], placement =  "right", options=list(container="body"))
                    )
                )
            }
        }
    })
    
    output$FCmenuSujet <- renderUI({ # FC entree du choix de la colonne sujet, il ne s affiche pas si son nom est basique
        if (input$chMethod == "FC"){
            if ('subject' %in% lectureAll()){
                hidden(
                    textInput('subject', label = NULL, value = 'subject')
                )
            }
            else if ('Subject' %in% lectureAll()){
                hidden(
                    textInput('subject', label = NULL, value = 'Subject')
                )
            }
            else if ('sujet' %in% lectureAll()){
                hidden(
                    textInput('subject', label = NULL, value = 'sujet')
                )
            }
            else if ('Sujet' %in% lectureAll()){
                hidden(
                    textInput('subject', label = NULL, value = 'Sujet')
                )
            }
            else {
                div(
                    div(style="width:80%; display:inline-block; vertical-align: middle;",
                        selectInput('subject',
                                    lexi['fcsubject', choiceL()],
                                    choices = lectureAll()[lectureAll()!= 'produit' & lectureAll()!= 'Produit' & lectureAll()!= 'product' & lectureAll()!= 'Product'
                                                           & lectureAll()!= 'description' & lectureAll()!= 'Description'],
                                    multiple = FALSE)
                    ),
                    div(style="display:inline-block; vertical-align: middle;",
                        bsButton("bssubject", label = "", icon = icon("question"), style = "info", size = "extra-small", placement = 'left'),
                        shinyBS::bsTooltip("bssubject", lexi['bsfcsubject', choiceL()], placement =  "right", options=list(container="body"))
                    )
                )
            }
        }
    })
    
    output$FCmenuSpecific <- renderUI({ # FC entree de l onglet specifique aux donnees du lexique FC
        if (input$chMethod == "FC"){
            div(
                div(style="width:80%; display:inline-block; vertical-align: middle;",
                    selectInput('specificWordsTabNameFC',
                                lexi['fcspecific', choiceL()],
                                choices = sheetsExcel(),
                                multiple = FALSE)
                ),
                div(style="display:inline-block; vertical-align: middle;",
                    bsButton("bsspecificWordsTabNameFC", label = "", icon = icon("question"), style = "info", size = "extra-small", placement = 'left'),
                    shinyBS::bsTooltip("bsspecificWordsTabNameFC", lexi['bsfcspecific', choiceL()], placement =  "right", options=list(container="body"))
                )
            )
        }
    })
    
    output$FCmenuLexique <- renderUI({ # FC entree choix du lexique pour le pretraitement des donnees FC
        if (input$chMethod == "FC"){
            div(
                div(style="width:80%; display:inline-block; vertical-align: middle;",
                    fileInput('lexique', label = lexi['fclexic', choiceL()], buttonLabel = "Browse...", placeholder = "No file selected")
                ),
                div(style="display:inline-block; vertical-align: middle;",
                    bsButton("bslexique", label = "", icon = icon("question"), style = "info", size = "extra-small", placement = 'left'),
                    shinyBS::bsTooltip("bslexique", lexi['bsfclexic', choiceL()], placement =  "right", options=list(container="body"))
                )
            )
        }
    })
    
    output$addLexicon <- renderUI({ # FC entree pour ajouter un mot au lexique de pretraitement
        if (input$actionFC){
            h5(lexi['fcaddLex1', choiceL()],textInput('wordLexicon', label = lexi['fcaddLex2', choiceL()], value = ""))
        }
    })
    
    output$addLexicon2 <- renderUI({ # FC entree pour ajouter un mot au lexique de pretraitement
        if (input$actionFC){
            textInput('lemmaLexicon', label = lexi['fcaddLex3', choiceL()], value = "")
        }
    })
    
    output$boutonLexiconFC <- renderUI({ # FC entree pour ajouter un mot au lexique de pretraitement
        if (input$actionFC){
            div(
                div(style="width:80%; display:inline-block; vertical-align: middle;",
                    actionButton(inputId = 'actionLexiconFC', lexi['fcaddLex4', choiceL()])
                ),
                div(style="display:inline-block; vertical-align: middle;",
                    bsButton("bsactionLexiconFC", label = "", icon = icon("question"), style = "info", size = "extra-small", placement = 'left'),
                    shinyBS::bsTooltip("bsactionLexiconFC", lexi['fcaddLexHelp', choiceL()], placement =  "right", options=list(container="body"))
                )
            )
        }
    })
    
    output$boutonAnalysisFC <- renderUI({ # FC entree pour demarrer l analyse FC
        if (input$actionFC){
            actionButton(inputId = 'actionAnalysisFC', lexi['fcAn', choiceL()],style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
        }
    })
    
    output$alphaFC <- renderUI({ # FC entree pour le parametre alpha de l analyse FC
        if (input$actionFC){
            div(
                div(style="width:80%; display:inline-block; vertical-align: middle;",
                    numericInput('alFC', label = lexi['fcalpha', choiceL()],value = 0.05, min = 0, max = 1, step = 0.01)
                ),
                div(style="display:inline-block; vertical-align: middle;",
                    bsButton("bsalFC", label = "", icon = icon("question"), style = "info", size = "extra-small", placement = 'left'),
                    shinyBS::bsTooltip("bsalFC", lexi['bsfcalpha', choiceL()], placement =  "right", options=list(container="body"))
                )
            )
        }
    })
    
    output$nbootFC <- renderUI({ # FC entree pour le nombre de boots dans l analyse FC
        if (input$actionFC){
            div(
                div(style="width:80%; display:inline-block; vertical-align: middle;",
                    numericInput('nbFC', label = lexi['fcnboot', choiceL()],value = 100, min = 0)
                ),
                div(style="display:inline-block; vertical-align: middle;",
                    bsButton("bsnbFC", label = "", icon = icon("question"), style = "info", size = "extra-small", placement = 'left'),
                    shinyBS::bsTooltip("bsnbFC", lexi['bsfcnboot', choiceL()], placement =  "right", options=list(container="body"))
                )
            )
        }
    })
    
    output$axesFC <- renderUI({ # FC entree pour le choix d axes a garder dans l analyse FC
        if (input$actionFC){
            div(
                div(style="width:80%; display:inline-block; vertical-align: middle;",
                    selectInput('axFC', label = lexi['fcchAxes', choiceL()], choices = c('signif',2:(nlevels(Act_table()$contingencyTable[,2])-1)))
                ),
                div(style="display:inline-block; vertical-align: middle;",
                    bsButton("bsaxFC", label = "", icon = icon("question"), style = "info", size = "extra-small", placement = 'left'),
                    shinyBS::bsTooltip("bsaxFC", lexi['bsfcchAxes', choiceL()], placement =  "right", options=list(container="body"))
                )
            )
        } 
    })
    
    output$choiceFC <- renderUI({ # FC entree pour choisir le type de tableau de l analyse FC
        if (input$actionFC){
            div(
                div(style="width:80%; display:inline-block; vertical-align: middle;",
                    selectInput('chFC', label = lexi['fcchoice', choiceL()], choices = c('percent.cont','original.cont','null.cont','p.value','derived.cont','percent.derived.cont'))
                ),
                div(style="display:inline-block; vertical-align: middle;",
                    bsButton("bschFC", label = "", icon = icon("question"), style = "info", size = "extra-small", placement = 'left'),
                    shinyBS::bsTooltip("bschFC", title = lexi['fcchoiceHelp', choiceL()], placement =  "right", options=list(container="body"))
                )
            )
        }
    })
    
    output$tabContingence <- renderDataTable({ # FC sortie table de contingence des donnees FC
        if (input$actionFC & is.null(input$lexique) == FALSE){
            Act_table()$contingencyTable
        }
    })
    
    output$keptWords <- renderDataTable({ # FC sortie mots gardes des donnees FC
        if (input$actionFC & is.null(input$lexique) == FALSE){
            data.frame(Act_table()$keptWords)
        }
    })
    
    output$removedWords <- renderDataTable({ # FC sortie mots supprimes des donnees FC
        if (input$actionFC & is.null(input$lexique) == FALSE){
            data.frame(Act_table()$removedWords)
        }
    })
    
    output$groupsOfWords <- renderDataTable({ # FC sortie groupe des mots du lexique FC
        if (input$actionFC & is.null(input$lexique) == FALSE){
            Act_table()$groupsOfWords
            convertDataFC = matrix(0,nrow = length(names(Act_table()$groupsOfWords)), ncol = 2)
            convertDataFC[,1] = names(Act_table()$groupsOfWords)
            convertDataFC[,2] = as.numeric(as.vector(Act_table()$groupsOfWords))
            colnames(convertDataFC) <- c('Words','Class')
            convertDataFC
        }
    })
    
    output$dendogram <- renderPlot({ # FC sortie dendogram des donnees FC
        if (input$actionFC & is.null(input$lexique) == FALSE){
            Act_table()$dendogram
        }
    })
    
    output$plotAnalyse <- renderPlot({ # FC sortie MRCA de l analyse FC
        if (input$actionFC){
            if (input$actionAnalysisFC){
                Act_tableFCAnalyse()[[1]]$output$biplot
            }
        }
    })
    
    output$plotAnalyse2 <- renderText({ # FC sortie tableau de l analyse FC
        if (input$actionFC){
            if (input$actionAnalysisFC){
                kable_styling(Act_tableFCAnalyse()[[1]]$output$testPerCell)
            }
        }
    })
    
    output$downloadanaFC <- downloadHandler( # FC telechargement html du tableau
        filename = function() {
            paste("analysis", ".html", sep = '')
        },
        content = function(file) {
            save_kable(kable_styling(Act_tableFCAnalyse()[[1]]$output$testPerCell), file = file, zoom = 1.5)
        }
    )
    
    output$downloadanaFCcsv <- downloadHandler( # FC telechargement csv du tableau
        filename = function() {
            paste("analysis", ".csv", sep = '')
        },
        content = function(file) {
            if (input$chFC == 'percent.cont'){
                write.csv2(Act_tableFCAnalyse()[[1]]$testPerCell$percent.cont, file, row.names = FALSE)
            }
            else if(input$chFC == 'original.cont'){
                write.csv2(Act_tableFCAnalyse()[[1]]$testPerCell$original.cont, file, row.names = FALSE)
            }
            else if(input$chFC == 'null.cont'){
                write.csv2(Act_tableFCAnalyse()[[1]]$testPerCell$null.cont, file, row.names = FALSE)
            }
            else if(input$chFC == 'p.value'){
                write.csv2(Act_tableFCAnalyse()[[1]]$testPerCell$p.value, file, row.names = FALSE)
            }
            else if(input$chFC == 'derived.cont'){
                write.csv2(Act_tableFCAnalyse()[[1]]$testPerCell$derived.cont, file, row.names = FALSE)
            }
            else if(input$chFC == 'percent.derived.cont'){
                write.csv2(Act_tableFCAnalyse()[[1]]$testPerCell$percent.derived.cont, file, row.names = FALSE)
            }
        }
    )
    
    output$downloadaxeFC <- downloadHandler( # FC telechargement png du MRCA
        filename = function() {
            paste("axes", ".png", sep = '')
        },
        content = function(file) {
            device <- function(..., width, height) {
                grDevices::png(..., width = width, height = height,
                               res = 300, units = "in")
            }
            ggsave(file, plot = Act_tableFCAnalyse()[[1]]$output$biplot, device = device)
        }
    )
    
    output$downloadaxeFCsvg <- downloadHandler( # FC telechargement svg du MRCA
        filename = function() {
            paste("axes", ".svg", sep = '')
        },
        content = function(file) {
            device <- function(..., width, height) {
                grDevices::svg(..., width = width, height = height)
            }
            ggsave(file, plot = Act_tableFCAnalyse()[[1]]$output$biplot, device = device)
        }
    )
    
    output$downloadDendogramFC <- downloadHandler( # FC telechargement png du dendogram
        filename = function() {
            paste("dendogram", ".png", sep = '')
        },
        content = function(file) {
            device <- function(..., width, height) {
                grDevices::png(..., width = width, height = height,
                               res = 300, units = "in")
            }
            ggsave(file, plot = Act_table()$dendogram, device = device)
        }
    )
    
    output$downloadDendogramFCsvg <- downloadHandler( # FC telechargement svg du dendogram
        filename = function() {
            paste("dendogram", ".svg", sep = '')
        },
        content = function(file) {
            device <- function(..., width, height) {
                grDevices::svg(..., width = width, height = height)
            }
            ggsave(file, plot = Act_table()$dendogram, device = device)
        }
    )
    
    output$downloadContingenceFCcsv <- downloadHandler( # FC telechargement csv de la table de contingence
        filename = function() {
            paste("contingence", ".csv", sep = '')
        },
        content = function(file) {
            write.csv2(Act_table()$contingencyTable, file, row.names = FALSE)
        }
    )
    
    output$downloadContingenceFCxlsx <- downloadHandler( # FC telechargement xlsx de la table de contingence
        filename = function() {
            paste("contingence", ".xlsx", sep = '')
        },
        content = function(file) {
            write.xlsx(Act_table()$contingencyTable, file, row.names = FALSE)
        }
    )
    
    output$downloadKeptwordsFCcsv <- downloadHandler( # FC telechargement csv des mots gardes
        filename = function() {
            paste("keptwords", ".csv", sep = '')
        },
        content = function(file) {
            write.csv2(data.frame(Act_table()$keptWords), file, row.names = FALSE)
        }
    )
    
    output$downloadKeptwordsFCxlsx <- downloadHandler( # FC telechargement xlsx des mots gardes
        filename = function() {
            paste("keptwords", ".xlsx", sep = '')
        },
        content = function(file) {
            write.xlsx(data.frame(Act_table()$keptWords), file, row.names = FALSE)
        }
    )
    
    output$downloadRemovedwordsFCcsv <- downloadHandler( # FC telechargement csv des mots supprimes
        filename = function() {
            paste("removedwords", ".csv", sep = '')
        },
        content = function(file) {
            write.csv2(data.frame(Act_table()$removedWords), file, row.names = FALSE)
        }
    )
    
    output$downloadRemovedwordsFCxlsx <- downloadHandler( # FC telechargement xlsx des mots supprimes
        filename = function() {
            paste("removedwords", ".xlsx", sep = '')
        },
        content = function(file) {
            write.xlsx(data.frame(Act_table()$removedWords), file, row.names = FALSE)
        }
    )

    output$downloadGroupsFCcsv <- downloadHandler( # FC telechargement csv des groupes de mots
        filename = function() {
            paste("groupofwords", ".csv", sep = '')
        },
        content = function(file) {
            convertDataFC = matrix(0,nrow = length(names(Act_table()$groupsOfWords)), ncol = 2)
            convertDataFC[,1] = names(Act_table()$groupsOfWords)
            convertDataFC[,2] = as.numeric(as.vector(Act_table()$groupsOfWords))
            colnames(convertDataFC) <- c('Words','Class')
            write.csv2(convertDataFC, file, row.names = FALSE)
        }
    )
    
    output$downloadGroupsFCxlsx <- downloadHandler( # FC telechargement xlsx des groupes de mots
        filename = function() {
            paste("groupofwords", ".xlsx", sep = '')
        },
        content = function(file) {
            convertDataFC = matrix(0,nrow = length(names(Act_table()$groupsOfWords)), ncol = 2)
            convertDataFC[,1] = names(Act_table()$groupsOfWords)
            convertDataFC[,2] = as.numeric(as.vector(Act_table()$groupsOfWords))
            colnames(convertDataFC) <- c('Words','Class')
            write.xlsx(data.frame(convertDataFC), file, row.names = FALSE)
        }
    )
    
    output$TabLexiconFC <- renderDataTable({ # FC sortie du lexique specifique aux donnees 
        if (input$actionFC & is.null(input$lexique) == FALSE){
            lexFC()
        }
    })
    
    output$TabLexiconFCcsv <- downloadHandler( # FC telechargement csv du lexique specifique aux donnees 
        filename = function() {
            paste("lexicon", ".csv", sep = '')
        },
        content = function(file) {
            write.csv2(lexFC(), file, row.names = FALSE)
        }
    )
    
    output$TabLexiconFCxlsx <- downloadHandler( # FC telechargement xlsx du lexique specifique aux donnees 
        filename = function() {
            paste("lexicon", ".xlsx", sep = '')
        },
        content = function(file) {
            write.xlsx(lexFC(), file, row.names = FALSE)
        }
    )
    
    output$productFC <- renderDataTable({ # FC sortie d une liste des produits des donnees
        if (input$actionFC & is.null(input$lexique) == FALSE){
            data.frame(levels(Act_table()$contingencyTable[,2]))
        }
    })
    
    output$nbSubFC <- renderText({ # FC sortie du nombre de sujets des donnees
        if (input$actionFC & is.null(input$lexique) == FALSE){
            as.character(nbSub())
        }
    })
    
    output$nbProdFC <- renderText({ # FC sortie du nombre de produits des donnees
        if (input$actionFC & is.null(input$lexique) == FALSE){
            as.character(nbProd())
        }
    })
    
    output$bodyFC <- renderUI({
        if (input$actionFC){
            navbarPage("", id = "tabselectedFC",
                       tabPanel(lexi['fctabResume', choiceL()], value = 2,
                                tabsetPanel(
                                    tabPanel(lexi['fctabUnit1', choiceL()],
                                             fluidRow(
                                                 textOutput('presentationFC')
                                             ),
                                             fluidRow(
                                                 box(title = 'products', width = 6, solidHeader = TRUE, dataTableOutput('productFC'))
                                             )
                                    ),
                                    tabPanel(lexi['fctabContingence', choiceL()],
                                             dataTableOutput('tabContingence'),
                                             downloadButton("downloadContingenceFCcsv", "csv"),
                                             downloadButton("downloadContingenceFCxlsx", "xlsx")),
                                    tabPanel(lexi['fctabKeptWord', choiceL()],
                                             dataTableOutput('keptWords'),
                                             downloadButton("downloadKeptwordsFCcsv", "csv"),
                                             downloadButton("downloadKeptwordsFCxlsx", "xlsx")),
                                    tabPanel(lexi['fctabRemovedWord', choiceL()],
                                             dataTableOutput('removedWords'),
                                             downloadButton("downloadRemovedwordsFCcsv", "csv"),
                                             downloadButton("downloadRemovedwordsFCxlsx", "xlsx")),
                                    tabPanel(lexi['fctabGroup', choiceL()],
                                             dataTableOutput('groupsOfWords'),
                                             downloadButton("downloadGroupsFCcsv", "csv"),
                                             downloadButton("downloadGroupsFCxlsx", "xlsx")),
                                    tabPanel(lexi['fctabDendogram', choiceL()],
                                             plotOutput('dendogram', height = "800px"),
                                             downloadButton("downloadDendogramFC", "png"),
                                             downloadButton("downloadDendogramFCsvg", "svg")),
                                    tabPanel(lexi['fctabLexicon', choiceL()],
                                             dataTableOutput('TabLexiconFC'),
                                             downloadButton("TabLexiconFCcsv", "csv"),
                                             downloadButton("TabLexiconFCxlsx", "xlsx"))
                                )
                       ),
                       tabPanel(lexi['fctabAnalysis', choiceL()], value = 1,
                                tabsetPanel(
                                    tabPanel(lexi['fctabAnalysis1', choiceL()],
                                             htmlOutput('plotAnalyse2'),
                                             downloadButton("downloadanaFC", "html"),
                                             downloadButton("downloadanaFCcsv", "csv")),
                                    tabPanel(lexi['fctabAnalysis2', choiceL()],
                                             plotOutput('plotAnalyse', height = "800px"),
                                             downloadButton("downloadaxeFC", "png"),
                                             downloadButton("downloadaxeFCsvg", "svg"))
                                    
                                )
                       ),
                       tabPanel(lexi['fctabUnit', choiceL()], value=5
                       )
            )
        }
    })
    
    # server general
    
    output$TDSmenuData <- renderUI({ # entree pour choisir le jeu de donnees, il s appelle dataTDS mais il est commun a toutes les methodes
        div(
            div(style="width:80%; display:inline-block; vertical-align: middle;",
                fileInput('dataTDS', label = lexi['tdsData', choiceL()], buttonLabel = paste(lexi['TDSmenuBrowse', choiceL()],'...'), placeholder = lexi['TDSmenuFileSelected', choiceL()])
            ),
            div(style="display:inline-block; vertical-align: middle;",
                bsButton("bsdataTDS", label = "", icon = icon("question"), style = "info", size = "extra-small", placement = 'left'),
                shinyBS::bsTooltip("bsdataTDS", lexi['bstdsData', choiceL()])
            )
        )
    })
    
    output$TDSmenuSeparator <- renderUI({ # entree pour le separateur si le fichier choisi est un csv, la aussi il y a TDS dans le nom mais il est commun
        div(
            div(style="width:80%; display:inline-block; vertical-align: middle;",
                textInput('separatorTDS', label = lexi['tdsSeparator', choiceL()], value = ';')
            ),
            div(style="display:inline-block; vertical-align: middle;",
                bsButton("bsseparatorTDS", label = "", icon = icon("question"), style = "info", size = "extra-small", placement = 'left'),
                shinyBS::bsTooltip("bsseparatorTDS", lexi['bstdsSeparator', choiceL()])
            )
        )
    })
    
    # server pour TDS
    
    output$TDSmenuStart <- renderUI({ # TDS entree pour start citation
        if (input$chMethod == 'TDS'){
            div(
                div(style="width:80%; display:inline-block; vertical-align: middle;",
                    checkboxInput('startWithFirstCitationTDS', label = lexi['tdsFirst', choiceL()], value = FALSE)
                ),
                div(style="display:inline-block; vertical-align: middle;",
                    bsButton("bsstartWithFirstCitationTDS", label = "", icon = icon("question"), style = "info", size = "extra-small", placement = 'left'),
                    shinyBS::bsTooltip("bsstartWithFirstCitationTDS", lexi['bstdsFirst', choiceL()])
                )
            )
       } 
    })
    
    output$TDSmenuDiscretization <- renderUI({ # TDS entree pour discretization
        if (input$chMethod == "TDS"){
            div(
                div(style="width:80%; display:inline-block; vertical-align: middle;",
                    numericInput('discretizationTDS', label = lexi['tdsDiscretization', choiceL()],value = 0.2, min = 0, max = 1, step = 0.1)
                ),
                div(style="display:inline-block; vertical-align: middle;",
                    bsButton("bsdiscretizationTDS", label = "", icon = icon("question"), style = "info", size = "extra-small", placement = 'left'),
                    shinyBS::bsTooltip("bsdiscretizationTDS", lexi['bstdsDiscretization', choiceL()])
                )
            )
        }
    })
    
    output$repAsAna2 <- renderUI({ # TDS entree repAsIndividual pour Product Temporality
        div(
            div(style="width:80%; display:inline-block; vertical-align: middle;",
                checkboxInput('clrepAsAna2', label = lexi['ana2repAs', choiceL()], value = FALSE)
            ),
            div(style="display:inline-block; vertical-align: middle;",
                bsButton("bsclrepAsAna2", label = "", icon = icon("question"), style = "info", size = "extra-small", placement = 'left'),
                shinyBS::bsTooltip("bsclrepAsAna2", lexi['bsana2repAs', choiceL()])
            )
        )
    })
    
    output$Ana2st <- renderUI({ # TDS entree pour standardiser ou non les donnees pour Product Temporality
        div(
            div(style="width:80%; display:inline-block; vertical-align: middle;",
                checkboxInput('clAna2st', label = lexi['ana2st', choiceL()], value = FALSE)
            ),
            div(style="display:inline-block; vertical-align: middle;",
                bsButton("bsclAna2st", label = "", icon = icon("question"), style = "info", size = "extra-small", placement = 'left'),
                shinyBS::bsTooltip("bsclAna2st", lexi['bsana2st', choiceL()])
            )
        )
    })
    
    
    output$menuSubjectTDS <- renderUI({ # TDS entree pour choisir colonne sujet
        if (input$chMethod == 'TDS'){
            if ('subject' %in% lectureAll()){
                hidden(
                    textInput('subjectTDS', label = NULL, value = 'subject')
                )
            }
            else if ('Subject' %in% lectureAll()){
                hidden(
                    textInput('subjectTDS', label = NULL, value = 'Subject')
                )
            }
            else if ('sujet' %in% lectureAll()){
                hidden(
                    textInput('subjectTDS', label = NULL, value = 'sujet')
                )
            }
            else if ('Sujet' %in% lectureAll()){
                hidden(
                    textInput('subjectTDS', label = NULL, value = 'Sujet')
                )
            }
            else {
                div(
                    div(style="width:80%; display:inline-block; vertical-align: middle;",
                        selectInput('subjectTDS',
                                    lexi['tdsSubject', choiceL()],
                                    choices = lectureAll()[lectureAll()!= 'produit' & lectureAll()!= 'Produit' & lectureAll()!= 'product' & lectureAll()!= 'Product'
                                                           & lectureAll()!= 'time' & lectureAll()!= 'Time' & lectureAll()!= 'temps' & lectureAll()!= 'Temps'
                                                           & lectureAll()!= 'replicate' & lectureAll()!= 'Replicate' & lectureAll()!= 'description' & lectureAll()!= 'Description'
                                                           & lectureAll()!= 'reproduction' & lectureAll()!= 'Reproduction' & lectureAll()!= 'score' & lectureAll()!= 'Score'],
                                    multiple = FALSE)
                    ),
                    div(style="display:inline-block; vertical-align: middle;",
                        bsButton("bssubjectTDS", label = "", icon = icon("question"), style = "info", size = "extra-small", placement = 'left'),
                        shinyBS::bsTooltip("bssubjectTDS", lexi['bssubjectTDS', choiceL()])
                    )
                )
            }
        }
    })
    
    output$menuProductTDS <- renderUI({ # TDS entree pour choisir colonne produit
        if (input$chMethod == 'TDS'){
            
            if ('product' %in% lectureAll()){
                hidden(
                    textInput('productTDS', label = NULL, value = 'product')
                )
            }
            else if ('Product' %in% lectureAll()){
                hidden(
                    textInput('productTDS', label = NULL, value = 'Product')
                )
            }
            else if ('produit' %in% lectureAll()){
                hidden(
                    textInput('productTDS', label = NULL, value = 'produit')
                )
            }
            else if ('Produit' %in% lectureAll()){
                hidden(
                    textInput('productTDS', label = NULL, value = 'Produit')
                )
            }
            else {
                
                div(
                    div(style="width:80%; display:inline-block; vertical-align: middle;",
                        selectInput('productTDS',
                                    lexi['tdsProduct', choiceL()],
                                    choices = lectureAll()[lectureAll()!= 'sujet' & lectureAll()!= 'Sujet' & lectureAll()!= 'subject' & lectureAll()!= 'Subject'
                                                           & lectureAll()!= 'time' & lectureAll()!= 'Time' & lectureAll()!= 'temps' & lectureAll()!= 'Temps'
                                                           & lectureAll()!= 'replicate' & lectureAll()!= 'Replicate' & lectureAll()!= 'description' & lectureAll()!= 'Description'
                                                           & lectureAll()!= 'reproduction' & lectureAll()!= 'Reproduction' & lectureAll()!= 'score' & lectureAll()!= 'Score'],
                                    multiple = FALSE)
                    ),
                    div(style="display:inline-block; vertical-align: middle;",
                        bsButton("bsproductTDS", label = "", icon = icon("question"), style = "info", size = "extra-small", placement = 'left'),
                        shinyBS::bsTooltip("bsproductTDS", lexi['bsproductTDS', choiceL()])
                    )
                )
            }
        }
    })
    
    output$menuDescriptorTDS <- renderUI({# TDS entree pour choisir colonne descriptor
        if (input$chMethod == 'TDS'){
            
            if ('description' %in% lectureAll()){
                hidden(
                    textInput('descriptorTDS', label = NULL, value = 'description')
                )
            }
            else if ('Description' %in% lectureAll()){
                hidden(
                    textInput('descriptorTDS', label = NULL, value = 'Description')
                )
            }
            else {   
                div(
                    div(style="width:80%; display:inline-block; vertical-align: middle;",
                        selectInput('descriptorTDS',
                                    lexi['tdsDescriptor', choiceL()],
                                    choices = lectureAll()[lectureAll()!= 'sujet' & lectureAll()!= 'Sujet' & lectureAll()!= 'subject' & lectureAll()!= 'Subject'
                                                           & lectureAll()!= 'produit' & lectureAll()!= 'Produit' & lectureAll()!= 'product' & lectureAll()!= 'Product'
                                                           & lectureAll()!= 'time' & lectureAll()!= 'Time' & lectureAll()!= 'temps' & lectureAll()!= 'Temps'
                                                           & lectureAll()!= 'replicate' & lectureAll()!= 'Replicate'
                                                           & lectureAll()!= 'reproduction' & lectureAll()!= 'Reproduction' & lectureAll()!= 'score' & lectureAll()!= 'Score'],
                                    multiple = FALSE)
                    ),
                    div(style="display:inline-block; vertical-align: middle;",
                        bsButton("bsdescriptorTDS", label = "", icon = icon("question"), style = "info", size = "extra-small", placement = 'left'),
                        shinyBS::bsTooltip("bsproductTDS", lexi['bsdescriptorTDS', choiceL()])
                    )
                )
            }
        }
    })
    
    output$menuTimeTDS <- renderUI({ # TDS entree pour choisir colonne time
        if (input$chMethod == 'TDS'){
            
            if ('time' %in% lectureAll()){
                hidden(
                    textInput('timeTDS', label = NULL, value = 'time')
                )
            }
            else if ('Time' %in% lectureAll()){
                hidden(
                    textInput('timeTDS', label = NULL, value = 'Time')
                )
            }
            else if ('temps' %in% lectureAll()){
                hidden(
                    textInput('timeTDS', label = NULL, value = 'temps')
                )
            }
            else if ('Temps' %in% lectureAll()){
                hidden(
                    textInput('timeTDS', label = NULL, value = 'Temps')
                )
            }
            else {    
                div(
                    div(style="width:80%; display:inline-block; vertical-align: middle;",
                        selectInput('timeTDS',
                                    lexi['tdsTime', choiceL()],
                                    choices = lectureAll()[lectureAll()!= 'sujet' & lectureAll()!= 'Sujet' & lectureAll()!= 'subject' & lectureAll()!= 'Subject'
                                                           & lectureAll()!= 'produit' & lectureAll()!= 'Produit' & lectureAll()!= 'product' & lectureAll()!= 'Product'
                                                           & lectureAll()!= 'description' & lectureAll()!= 'Description' & lectureAll()!= 'replicate' & lectureAll()!= 'Replicate'
                                                           & lectureAll()!= 'reproduction' & lectureAll()!= 'Reproduction' & lectureAll()!= 'score' & lectureAll()!= 'Score'],
                                    multiple = FALSE)
                    ),
                    div(style="display:inline-block; vertical-align: middle;",
                        bsButton("bstimeTDS", label = "", icon = icon("question"), style = "info", size = "extra-small", placement = 'left'),
                        shinyBS::bsTooltip("bstimeTDS", lexi['bstimeTDS', choiceL()])
                    )
                )
            }
        }
    })
    
    output$menuScoreTDS <- renderUI({ # TDS entree pour choisir colonne score
        if (input$chMethod == 'TDS'){
            
            if ('score' %in% lectureAll()){
                hidden(
                    textInput('scoreTDS', label = NULL, value = 'score')
                )
            }
            else if ('Score' %in% lectureAll()){
                hidden(
                    textInput('scoreTDS', label = NULL, value = 'Score')
                )
            }
            else {    
                div(
                    div(style="width:80%; display:inline-block; vertical-align: middle;",
                        selectInput('scoreTDS',
                                    lexi['tdsScore', choiceL()],
                                    choices = lectureAll()[lectureAll()!= 'sujet' & lectureAll()!= 'Sujet' & lectureAll()!= 'subject' & lectureAll()!= 'Subject'
                                                           & lectureAll()!= 'produit' & lectureAll()!= 'Produit' & lectureAll()!= 'product' & lectureAll()!= 'Product'
                                                           & lectureAll()!= 'time' & lectureAll()!= 'Time' & lectureAll()!= 'temps' & lectureAll()!= 'Temps'
                                                           & lectureAll()!= 'description' & lectureAll()!= 'Description' & lectureAll()!= 'replicate' & lectureAll()!= 'Replicate'
                                                           & lectureAll()!= 'reproduction' & lectureAll()!= 'Reproduction'],
                                    multiple = FALSE)
                    ),
                    div(style="display:inline-block; vertical-align: middle;",
                        bsButton("bsscoreTDS", label = "", icon = icon("question"), style = "info", size = "extra-small", placement = 'left'),
                        shinyBS::bsTooltip("bsscoreTDS", lexi['bsscoreTDS', choiceL()])
                    )
                )
            }
        }
    })
    
    output$menuReplicateTDS <- renderUI({ # TDS entree pour choisir colonne replicate
        if (input$chMethod == 'TDS'){
            
            if ('replicate' %in% lectureAll()){
                hidden(
                    textInput('replicateTDS', label = NULL, value = 'replicate')
                )
            }
            else if ('Replicate' %in% lectureAll()){
                hidden(
                    textInput('replicateTDS', label = NULL, value = 'Replicate')
                )
            }
            else if ('reproduction' %in% lectureAll()){
                hidden(
                    textInput('replicateTDS', label = NULL, value = 'reproduction')
                )
            }
            else if ('Reproduction' %in% lectureAll()){
                hidden(
                    textInput('replicateTDS', label = NULL, value = 'Reproduction')
                )
            }
            else {     
                div(
                    div(style="width:80%; display:inline-block; vertical-align: middle;",
                        selectInput('replicateTDS',
                                    lexi['tdsReplicate', choiceL()],
                                    choices = lectureAll()[lectureAll()!= 'sujet' & lectureAll()!= 'Sujet' & lectureAll()!= 'subject' & lectureAll()!= 'Subject'
                                                           & lectureAll()!= 'produit' & lectureAll()!= 'Produit' & lectureAll()!= 'product' & lectureAll()!= 'Product'
                                                           & lectureAll()!= 'time' & lectureAll()!= 'Time' & lectureAll()!= 'temps' & lectureAll()!= 'Temps'
                                                           & lectureAll()!= 'description' & lectureAll()!= 'Description' & lectureAll()!= 'score' & lectureAll()!= 'Score'],
                                    multiple = FALSE) 
                    ),
                    div(style="display:inline-block; vertical-align: middle;",
                        bsButton("bsreplicateTDS", label = "", icon = icon("question"), style = "info", size = "extra-small", placement = 'left'),
                        shinyBS::bsTooltip("bsreplicateTDS", lexi['bsreplicateTDS', choiceL()])
                    )
                )
            }
        }
    })
    
    output$TDSmenuBouton <- renderUI({ # TDS entree pour lancer le pretraitement des donnees TDS
        if (input$chMethod == "TDS"){
            actionButton(inputId = 'actionTDS', lexi['tdsStart', choiceL()])
        }
    })
    
    output$alphaAnaTDS <- renderUI({ # TDS entree pour alpha pour Product Temporality
        if (input$actionTDS){
            div(
                div(style="width:80%; display:inline-block; vertical-align: middle;",
                    numericInput('alTDSana2', label = 'alpha :',value = 0.05, min = 0, max = 1, step = 0.01)
                ),
                div(style="display:inline-block; vertical-align: middle;",
                    bsButton("bsalTDSana2", label = "", icon = icon("question"), style = "info", size = "extra-small", placement = 'left'),
                    shinyBS::bsTooltip("bsalTDSana2", 'info')
                )
            )
        }
    })
    
    output$alphaAna1TDS <- renderUI({ # TDS entree parametre alpha pour Panelist Behavior
        if (input$actionTDS){
            div(
                div(style="width:80%; display:inline-block; vertical-align: middle;",
                    numericInput('alTDSana1', label = lexi['TDSana1alpha', choiceL()],value = 0.05, min = 0, max = 1, step = 0.01)
                ),
                div(style="display:inline-block; vertical-align: middle;",
                    bsButton("bsalTDSana1", label = "", icon = icon("question"), style = "info", size = "extra-small", placement = 'left'),
                    shinyBS::bsTooltip("bsalTDSana1", lexi['bsTDSana1alpha', choiceL()])
                )
            )
        }
    })
    
    output$alphaAna3TDS <- renderUI({ # TDS entree parametre alpha pour Dominance Durations
        if (input$actionTDS){
            div(
                div(style="width:80%; display:inline-block; vertical-align: middle;",
                    numericInput('alTDSana3', label = lexi['TDSana3alpha', choiceL()],value = 0.05, min = 0, max = 1, step = 0.01)
                ),
                div(style="display:inline-block; vertical-align: middle;",
                    bsButton("bsalTDSana3", label = "", icon = icon("question"), style = "info", size = "extra-small", placement = 'left'),
                    shinyBS::bsTooltip("bsalTDSana3", lexi['bsTDSana3alpha', choiceL()])
                )
            )
        }
    })
    
    output$actionAna1TDS <- renderUI({ # TDS entree pour lancer l analyse de Panelist Behavior
        if (input$actionTDS){
            actionButton(inputId = 'actAna1TDS', 'start', style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
        }
    })
    
    output$panelTablesequenceStartTDS <- renderUI({ # TDS entree parametre sequence start pour Panelist Behavior
        if (input$actionTDS){
            div(
                div(style="width:80%; display:inline-block; vertical-align: middle;",
                    numericInput('a2sequenceStartTDS', label = lexi['a2sequenceStartTDS', choiceL()],value = 15, min = 0, max = NA, step = 1)
                ),
                div(style="display:inline-block; vertical-align: middle;",
                    bsButton("bsa2sequenceStartTDS", label = "", icon = icon("question"), style = "info", size = "extra-small", placement = 'left'),
                    shinyBS::bsTooltip("bsa2sequenceStartTDS", lexi['bsa2sequenceStartTDS', choiceL()])
                )
            )
        }
    })
    
    output$panelTablesequenceDurationTDS <- renderUI({ # TDS entree parametre sequence duration pour Panelist Behavior
        if (input$actionTDS){
            div(
                div(style="width:80%; display:inline-block; vertical-align: middle;",
                    numericInput('a2sequenceDurationTDS', label = lexi['a2sequenceDurationTDS', choiceL()],value = 6, min = 0, max = NA, step = 1)
                ),
                div(style="display:inline-block; vertical-align: middle;",
                    bsButton("bsa2sequenceDurationTDS", label = "", icon = icon("question"), style = "info", size = "extra-small", placement = 'left'),
                    shinyBS::bsTooltip("bsa2sequenceDurationTDS", lexi['bsa2sequenceDurationTDS', choiceL()])
                )
            )
        }
    })
    
    output$panelTablenbDescriptorsTDS <- renderUI({ # TDS entree parametre nb descriptor pour Panelist Behavior
        if (input$actionTDS){
            div(
                div(style="width:80%; display:inline-block; vertical-align: middle;",
                    numericInput('a2nbDescriptorsTDS', label = lexi['a2nbDescriptorsTDS', choiceL()],value = 1, min = 0, max = NA, step = 1)
                ),
                div(style="display:inline-block; vertical-align: middle;",
                    bsButton("bsa2nbDescriptorsTDS", label = "", icon = icon("question"), style = "info", size = "extra-small", placement = 'left'),
                    shinyBS::bsTooltip("bsa2nbDescriptorsTDS", lexi['bsa2nbDescriptorsTDS', choiceL()])
                )
            )
        }
    })
    
    output$panelTablenbClicksTDS <- renderUI({ # TDS entree parametre nb clicks start pour Panelist Behavior
        if (input$actionTDS){
            div(
                div(style="width:80%; display:inline-block; vertical-align: middle;",
                    numericInput('a2nbClicksTDS', label = lexi['a2nbClicksTDS', choiceL()],value = 1.5, min = 0, max = NA, step = 0.1)
                ),
                div(style="display:inline-block; vertical-align: middle;",
                    bsButton("bsa2nbClicksTDS", label = "", icon = icon("question"), style = "info", size = "extra-small", placement = 'left'),
                    shinyBS::bsTooltip("bsa2nbClicksTDS", lexi['bsa2nbClicksTDS', choiceL()])
                )
            )
        }
    })
    
    output$panelTabledescriptorDurationTDS <- renderUI({ # TDS entree parametre nb descriptor duration pour Panelist Behavior
        if (input$actionTDS){
            div(
                div(style="width:80%; display:inline-block; vertical-align: middle;",
                    numericInput('a2descriptorDurationTDS', label = lexi['a2descriptorDurationTDS', choiceL()],value = 2, min = 0, max = NA, step = 1)
                ),
                div(style="display:inline-block; vertical-align: middle;",
                    bsButton("bsa2descriptorDurationTDS", label = "", icon = icon("question"), style = "info", size = "extra-small", placement = 'left'),
                    shinyBS::bsTooltip("bsa2descriptorDurationTDS", lexi['bsa2descriptorDurationTDS', choiceL()])
                )
            )
        }
    })
    
    output$actionAna2TDS <- renderUI({ # TDS entree pour lancer les analyses de Product Temporality
        if (input$actionTDS){
            actionButton(inputId = 'actAna2TDS', 'start', style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
        }
    })
    
    output$actionAna3TDS <- renderUI({ # TDS entree pour lancer l analyse de Dominance Durations
        if (input$actionTDS){
            actionButton(inputId = 'actAna3TDS', 'start', style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
        }
    })
    
    # tables de donnees des donnees TDS pretraitees
    
    output$dfTDS <- renderDataTable({
        ActTDS()$df
    })
    
    output$dominancesTDS <- renderDataTable({
        ActTDS()$dominances
    })
    
    output$stdDominancesTDS <- renderDataTable({
        ActTDS()$stdDominances
    })
    
    output$durationsTDS <- renderDataTable({
        ActTDS()$durations
    })
    
    output$citationsTDS <- renderDataTable({
        ActTDS()$citations
    })
    
    output$behavioursTDS <- renderDataTable({
        ActTDS()$behaviours
    })
    
    output$BehaviourDistTDS <- renderPlot({ # TDS sortie Custom Analysis
        a1TDS()[[1]]$output$panelBehaviourDistribution
    })
    outputOptions(output, 'BehaviourDistTDS', suspendWhenHidden = FALSE)
    
    output$ClustDurationTDS <- renderPlot({ # TDS sortie Custom Analysis
        a4TDS()[[1]]$output$durationsClusters
    })
    outputOptions(output, 'ClustDurationTDS', suspendWhenHidden = FALSE)
    
    output$DiffDomCurvTDS <- renderPlot({ # TDS sortie Custom Analysis
        b6TDS()[[1]]$output$differenceCurves
    })
    outputOptions(output, 'DiffDomCurvTDS', suspendWhenHidden = FALSE)
    
    output$BarCitTDS <- renderPlot({ # TDS sortie Custom Analysis
        c2TDS()[[1]]$output$barplot
    })
    outputOptions(output, 'BarCitTDS', suspendWhenHidden = FALSE)
    
    output$DurDistTDS <- renderPlot({ # TDS sortie Custom Analysis
        d1TDS()[[1]]$output$durationsDistribution
    })
    outputOptions(output, 'DurDistTDS', suspendWhenHidden = FALSE)
    
    output$BarDurTDS <- renderPlot({ # TDS sortie Custom Analysis
        d2TDS()[[1]]$output$barplot
    })
    outputOptions(output, 'BarDurTDS', suspendWhenHidden = FALSE)
    
    output$BehaviourPanelTDS <- renderText({ # TDS sortie Panelist Table Panelist Behavior
        kable_styling(a2TDS()[[1]]$output$panelBehaviour)
    })
    outputOptions(output, 'BehaviourPanelTDS', suspendWhenHidden = FALSE)
    
    output$downloadpanelBehaviourTDS <- downloadHandler( # TDS telechargement html Panelist Table Panelist Behavior
        filename = function() {
            paste("panelBehaviour", ".html", sep = '')
        },
        content = function(file) {
            save_kable(kable_styling(a2TDS()[[1]]$output$panelBehaviour), file = file)
        }
    )
    
    output$downloadpanelBehaviourTDScsv <- downloadHandler( # TDS telechargement csv Panelist Table Panelist Behavior
        filename = function() {
            paste("panelBehaviour", ".csv", sep = '')
        },
        content = function(file) {
            write.csv2(a2TDS()[[1]]$df, file, row.names = FALSE)
        }
    )
    
    output$downloadpanelBehaviourAnovaTDS <- downloadHandler( # TDS telechargement html anova of behaviours Panelist Behavior
        filename = function() {
            paste("BehaviourAnova", ".html", sep = '')
        },
        content = function(file) {
            save_kable(kable_styling(a3TDS()[[1]]$output$anova), file = file)
        }
    )
    
    output$downloadpanelBehaviourAnovaTDScsv <- downloadHandler( # TDS telechargement csv anova of behaviours Panelist Behavior
        filename = function() {
            paste("BehaviourAnova", ".csv", sep = '')
        },
        content = function(file) {
            write.csv2(a3TDS()[[1]]$output$result, file, row.names = FALSE)
        }
    )
    
    output$TDStriTable <- renderText({ # inutile
        kable_styling(a2TDStri()[[1]]$output$panelBehaviour)
    })
    
    output$BehaviourAnovaTDS <- renderText({ # TDS sortie anova of behaviours Panelist Behavior
        kable_styling(a3TDS()[[1]]$output$anova)
    })
    outputOptions(output, 'BehaviourAnovaTDS', suspendWhenHidden = FALSE)
    
    output$AnovaDurationsTDS <- renderText({ # TDS sortie anova of durations Dominance Durations
        kable_styling(d3TDS()[[1]]$output$anova)
    })
    
    output$AnovaDurationsTDShtml <- downloadHandler( # TDS telechargement html anova of durations Dominance Durations
        filename = function() {
            paste("AnovaDurations", ".html", sep = '')
        },
        content = function(file) {
            save_kable(kable_styling(d3TDS()[[1]]$output$anova), file = file)
        }
    )
    
    output$AnovaDurationsTDScsv <- downloadHandler( # TDS telechargement csv anova of durations Dominance Durations
        filename = function() {
            paste("AnovaDurations", ".csv", sep = '')
        },
        content = function(file) {
            write.csv2(d3TDS()[[1]]$output$result, file, row.names = FALSE)
        }
    )
    
    output$AnovaDurationsTDSrep <- renderText({ # TDS sortie anova of durations rep Custom Analysis 
        kable_styling(d34TDS()[[as.numeric(input$d34TDSrepChoice)]]$output$anova)
    })
    
    output$AnovaDurationsTDShtmlrep <- downloadHandler( # TDS telechargement html anova of durations rep Custom Analysis 
        filename = function() {
            paste("AnovaDurations", ".html", sep = '')
        },
        content = function(file) {
            save_kable(kable_styling(d34TDS()[[as.numeric(input$d34TDSrepChoice)]]$output$anova), file = file)
        }
    )
    
    output$AnovaDurationsTDScsvrep <- downloadHandler( # TDS telechargement csv anova of durations rep Custom Analysis 
        filename = function() {
            paste("AnovaDurations", ".csv", sep = '')
        },
        content = function(file) {
            write.csv2(d34TDS()[[as.numeric(input$d34TDSrepChoice)]]$output$result, file, row.names = FALSE)
        }
    )
    
    output$indSeqTDS <- renderPlot({ # TDS sortie Individual Sequences Product Temporality
        b1TDS()[[1]]$output$sequences
    })
    outputOptions(output, 'indSeqTDS', suspendWhenHidden = FALSE)
    
    output$domCurTDS <- renderPlot({ # TDS sortie Dominances Curves Product Temporality
        b23TDS()[[1]]$output$dominanceCurves
    })
    outputOptions(output, 'domCurTDS', suspendWhenHidden = FALSE)
    
    output$PanelSequencesTDS <- renderPlot({ # TDS sortie Panel Bandplot Product Temporality
        b89TDS()[[1]]$output$panelSequences
    })
    outputOptions(output, 'PanelSequencesTDS', suspendWhenHidden = FALSE)
    
    output$PCATDS <- renderPlot({ # TDS sortie PCA of durations Dominance Durations
        p = d56TDS()
        return(p[[1]]$output$biplot)
    })
    outputOptions(output, 'PCATDS', suspendWhenHidden = FALSE)
    
    output$CVATDS <- renderPlot({ # TDS sortie CVA of durations Dominance Durations
        d7TDS()[[1]]$output$biplot
    })
    outputOptions(output, 'CVATDS', suspendWhenHidden = FALSE)
    
    output$PCATDSpng <- downloadHandler( # TDS telechargement png PCA of durations Dominance Durations
        filename = function() {
            paste("PCA", ".png", sep = '')
        },
        content = function(file) {
            device <- function(..., width, height) {
                grDevices::png(..., width = width, height = height,
                               res = 300, units = "in")
            }
            ggsave(file, plot = d56TDS()[[1]]$output$biplot, device = device)
        }
    )
    
    output$PCATDSsvg <- downloadHandler( # TDS telechargement svg PCA of durations Dominance Durations
        filename = function() {
            paste("PCA", ".svg", sep = '')
        },
        content = function(file) {
            device <- function(..., width, height) {
                grDevices::svg(..., width = width, height = height)
            }
            ggsave(file, plot = d56TDS()[[1]]$output$biplot, device = device)
        }
    )
    
    output$CVATDSpng <- downloadHandler( # TDS telechargement png CVA of durations Dominance Durations
        filename = function() {
            paste("CVA", ".png", sep = '')
        },
        content = function(file) {
            device <- function(..., width, height) {
                grDevices::png(..., width = width, height = height,
                               res = 300, units = "in")
            }
            ggsave(file, plot = d7TDS()[[1]]$output$biplot, device = device)
        }
    )
    
    output$CVATDSsvg <- downloadHandler( # TDS telechargement svg CVA of durations Dominance Durations
        filename = function() {
            paste("CVA", ".svg", sep = '')
        },
        content = function(file) {
            device <- function(..., width, height) {
                grDevices::svg(..., width = width, height = height)
            }
            ggsave(file, plot = d7TDS()[[1]]$output$biplot, device = device)
        }
    )
    
    output$PCATDSRep <- renderPlot({ # TDS sortie PCA of durations Rep Custom Analysis
        d56TDSRep()[[as.numeric(input$d56TDSrepChoice)]]$output$biplot
    })
    
    output$CVATDSRep <- renderPlot({ # TDS sortie CVA of durations Dominance Durations
        d7TDSRep()[[as.numeric(input$d7TDSrepChoice)]]$output$biplot
    })
    
    output$PCATDSpngRep <- downloadHandler( # TDS telechargement png PCA of durations Rep Custom Analysis
        filename = function() {
            paste("PCA", ".png", sep = '')
        },
        content = function(file) {
            device <- function(..., width, height) {
                grDevices::png(..., width = width, height = height,
                               res = 300, units = "in")
            }
            ggsave(file, plot = d56TDSRep()[[as.numeric(input$d56TDSrepChoice)]]$output$biplot, device = device)
        }
    )
    
    output$CVATDSpngRep <- downloadHandler( # TDS telechargement png CVA of durations Rep Custom Analysis
        filename = function() {
            paste("CVA", ".png", sep = '')
        },
        content = function(file) {
            device <- function(..., width, height) {
                grDevices::png(..., width = width, height = height,
                               res = 300, units = "in")
            }
            ggsave(file, plot = d7TDSRep()[[as.numeric(input$d7TDSrepChoice)]]$output$biplot, device = device)
        }
    )
    
    output$PCATDSsvgRep <- downloadHandler( # TDS telechargement svg PCA of durations Rep Custom Analysis
        filename = function() {
            paste("PCA", ".svg", sep = '')
        },
        content = function(file) {
            device <- function(..., width, height) {
                grDevices::svg(..., width = width, height = height)
            }
            ggsave(file, plot = d56TDSRep()[[as.numeric(input$d56TDSrepChoice)]]$output$biplot, device = device)
        }
    )
    
    output$CVATDSsvgRep <- downloadHandler( # TDS telechargement png CVA of durations Rep Custom Analysis
        filename = function() {
            paste("CVA", ".svg", sep = '')
        },
        content = function(file) {
            device <- function(..., width, height) {
                grDevices::svg(..., width = width, height = height)
            }
            ggsave(file, plot = d56TDSRep()[[as.numeric(input$d7TDSrepChoice)]]$output$biplot, device = device)
        }
    )
    
    output$PCAtrajectoriesTDS <- renderPlot({ # TDS sortie PCA des trajectoires Product Temporality
        d9TDS()[[1]]$output$biplot
    })
    
    output$maxDomTDS <- renderPlot({ # inutile
        if (input$actAna2TDS){
            b5TDS()[[1]]$output$dominances
        }
    })
    
    output$choiceTDS <- renderUI({ # TDS entree choix pour Custom Analysis
        div(
            div(style="width:80%; display:inline-block; vertical-align: middle;",
                selectInput('chTDS', label = 'choice :', choices = c('Citation distribution','Panel behaviour distribution','Clusters of subjects durations','Differences of dominance curves','Barplot of citations','Duration distribution','Barplot of durations', 'PCA of durations by rep', 'CVA of durations by rep', 'ANOVA of durations by rep'))
            ),
            div(style="display:inline-block; vertical-align: middle;",
                bsButton("bschTDS", label = "", icon = icon("question"), style = "info", size = "extra-small", placement = 'left'),
                shinyBS::bsTooltip("bschTDS", lexi['bsChoiceTDS', choiceL()])
            )
        )
    })
    
    output$c1boutonchTDS <- renderUI({ # TDS entree parametre pour Custom analysis
        if (input$actionTDS & (input$chTDS == 'Citation distribution')){
            actionButton(inputId = 'c1actionchTDS', 'start', style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
        }
    })
    
    output$a1boutonchTDS <- renderUI({ # TDS entree parametre pour Custom analysis
        if (input$actionTDS & (input$chTDS == 'Panel behaviour distribution')){
            actionButton(inputId = 'a1actionchTDS', 'start', style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
        }
    })
    
    output$a4boutonchTDS <- renderUI({ # TDS entree parametre pour Custom analysis
        if (input$actionTDS & (input$chTDS == 'Clusters of subjects durations')){
            actionButton(inputId = 'a4actionchTDS', 'start', style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
        }
    })
    
    output$b6boutonchTDS <- renderUI({ # TDS entree parametre pour Custom analysis
        if (input$actionTDS & (input$chTDS == 'Differences of dominance curves')){
            actionButton(inputId = 'b6actionchTDS', 'start', style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
        }
    })
    
    output$b6alphachTDS <- renderUI({ # TDS entree parametre pour Custom analysis
        if (input$actionTDS & (input$chTDS == 'Differences of dominance curves')){
            div(
                div(style="width:80%; display:inline-block; vertical-align: middle;",
                    numericInput('b6alpha', label = 'alpha :',value = 0.05, min = 0, max = 1, step = 0.01)
                ),
                div(style="display:inline-block; vertical-align: middle;",
                    bsButton("bsb6alpha", label = "", icon = icon("question"), style = "info", size = "extra-small", placement = 'left'),
                    shinyBS::bsTooltip("bsb6alpha", 'info')
                )
            )
        }
    })
    
    output$b6AnaDiffDomCurvStTDS <- renderUI({ # TDS entree parametre pour Custom analysis
        if (input$actionTDS & (input$chTDS == 'Differences of dominance curves')){
            div(
                div(style="width:80%; display:inline-block; vertical-align: middle;",
                    checkboxInput('clAnaDiffDomCurvSt', label = lexi['anaDiffDomCurvSt', choiceL()], value = FALSE)
                ),
                div(style="display:inline-block; vertical-align: middle;",
                    bsButton("bsclAnaDiffDomCurvStst", label = "", icon = icon("question"), style = "info", size = "extra-small", placement = 'left'),
                    shinyBS::bsTooltip("bsclAnaDiffDomCurvStst", lexi['bsanaDiffDomCurvStst', choiceL()])
                )
            )
        }
    })
    
    output$c2boutonchTDS <- renderUI({ # TDS entree parametre pour Custom analysis
        if (input$actionTDS & (input$chTDS == 'Barplot of citations')){
            actionButton(inputId = 'c2actionchTDS', 'start', style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
        }
    })
    
    output$c2confintTDS <- renderUI({ # TDS entree parametre pour Custom analysis
        if (input$actionTDS & (input$chTDS == 'Barplot of citations')){
            div(
                div(style="width:80%; display:inline-block; vertical-align: middle;",
                    numericInput('TDSconfintc2', label = 'confInt :',value = 0.95, min = 0, max = 1, step = 0.01)
                ),
                div(style="display:inline-block; vertical-align: middle;",
                    bsButton("bsTDSconfintc2", label = "", icon = icon("question"), style = "info", size = "extra-small", placement = 'left'),
                    shinyBS::bsTooltip("bsTDSconfintc2", 'info')
                )
            )
        }
    })
    
    output$d2confintTDS <- renderUI({ # TDS entree parametre pour Custom analysis
        if (input$actionTDS & (input$chTDS == 'Barplot of durations')){
            div(
                div(style="width:80%; display:inline-block; vertical-align: middle;",
                    numericInput('TDSconfintd2', label = 'confInt :',value = 0.95, min = 0, max = 1, step = 0.01)
                ),
                div(style="display:inline-block; vertical-align: middle;",
                    bsButton("bsTDSconfintd2", label = "", icon = icon("question"), style = "info", size = "extra-small", placement = 'left'),
                    shinyBS::bsTooltip("bsTDSconfintd2", 'info')
                )
            )
        }
    })
    
    output$d1boutonchTDS <- renderUI({ # TDS entree parametre pour Custom analysis
        if (input$actionTDS & (input$chTDS == 'Duration distribution')){
            actionButton(inputId = 'd1actionchTDS', 'start', style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
        }
    })
    
    output$d2boutonchTDS <- renderUI({ # TDS entree parametre pour Custom analysis
        if (input$actionTDS & (input$chTDS == 'Barplot of durations')){
            actionButton(inputId = 'd2actionchTDS', 'start', style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
        }
    })
    
    output$d56TDSrepch <- renderUI({ # TDS entree parametre pour Custom analysis
        if (input$actionTDS & (input$chTDS == 'PCA of durations by rep')){
            div(
                div(style="width:80%; display:inline-block; vertical-align: middle;",
                    selectInput('d56TDSrepChoice', label = 'Rep :', choices = 1:nlevels(ActTDS()$df$rep), multiple = FALSE)
                ),
                div(style="display:inline-block; vertical-align: middle;",
                    bsButton("bsd56TDSrepChoice", label = "", icon = icon("question"), style = "info", size = "extra-small", placement = 'left'),
                    shinyBS::bsTooltip("bsd56TDSrepChoice", 'info')
                )
            )
        }
    })
    
    output$d7TDSrepch <- renderUI({ # TDS entree parametre pour Custom analysis
        if (input$actionTDS & (input$chTDS == 'CVA of durations by rep')){
            div(
                div(style="width:80%; display:inline-block; vertical-align: middle;",
                    selectInput('d7TDSrepChoice', label = 'Rep :', choices = 1:nlevels(ActTDS()$df$rep), multiple = FALSE)
                ),
                div(style="display:inline-block; vertical-align: middle;",
                    bsButton("bsd7TDSrepChoice", label = "", icon = icon("question"), style = "info", size = "extra-small", placement = 'left'),
                    shinyBS::bsTooltip("bsd7TDSrepChoice", 'info')
                )
            )
        }
    })
    
    output$d34TDSrepch <- renderUI({ # TDS entree parametre pour Custom analysis
        if (input$actionTDS & (input$chTDS == 'ANOVA of durations by rep')){
            div(
                div(style="width:80%; display:inline-block; vertical-align: middle;",
                    selectInput('d34TDSrepChoice', label = 'Rep :', choices = 1:nlevels(ActTDS()$df$rep), multiple = FALSE)
                ),
                div(style="display:inline-block; vertical-align: middle;",
                    bsButton("bsd34TDSrepChoice", label = "", icon = icon("question"), style = "info", size = "extra-small", placement = 'left'),
                    shinyBS::bsTooltip("bsd34TDSrepChoice", 'info')
                )
            )
        }
    })
    
    output$alphaAnovaTDSrep <- renderUI({ # TDS entree parametre pour Custom analysis
        if (input$actionTDS & (input$chTDS == 'ANOVA of durations by rep')){
            div(
                div(style="width:80%; display:inline-block; vertical-align: middle;",
                    numericInput('alAnovaTDSrep', label = lexi['TDSana3alpha', choiceL()],value = 0.05, min = 0, max = 1, step = 0.01)
                ),
                div(style="display:inline-block; vertical-align: middle;",
                    bsButton("bsalAnovaTDSrep", label = "", icon = icon("question"), style = "info", size = "extra-small", placement = 'left'),
                    shinyBS::bsTooltip("bsalAnovaTDSrep", lexi['bsTDSana3alpha', choiceL()])
                )
            )
        }
    })
    
    output$d56boutonchTDSrep <- renderUI({ # TDS entree parametre pour Custom analysis
        if (input$actionTDS & (input$chTDS == 'PCA of durations by rep')){
            actionButton(inputId = 'd56actionchTDSrep', 'start', style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
        }
    })
    
    output$d7boutonchTDSrep <- renderUI({ # TDS entree parametre pour Custom analysis
        if (input$actionTDS & (input$chTDS == 'CVA of durations by rep')){
            actionButton(inputId = 'd7actionchTDSrep', 'start', style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
        }
    })
    
    output$d34boutonchTDSrep <- renderUI({ # TDS entree parametre pour Custom analysis
        if (input$actionTDS & (input$chTDS == 'ANOVA of durations by rep')){
            actionButton(inputId = 'd34actionchTDSrep', 'start', style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
        }
    })
    
    output$citDistTDS <- renderPlot({ # TDS entree parametre pour Custom analysis
        if (input$c1actionchTDS & input$chTDS == 'Citation distribution'){
            c1TDS()[[1]]$output$citationsDistribution
        }
    })
    
    output$downloadtdsDFcsv <- downloadHandler( # TDS entree telechargement csv summary
        filename = function() {
            paste("DF", ".csv", sep = '')
        },
        content = function(file) {
            write.csv2(ActTDS()$df, file, row.names = FALSE)
        }
    )
    
    output$downloadtdsDFxlsx <- downloadHandler( # TDS entree telechargement xlsx summary
        filename = function() {
            paste("DF", ".xlsx", sep = '')
        },
        content = function(file) {
            write.xlsx(ActTDS()$df, file, row.names = FALSE)
        }
    )
    
    output$downloadDominanceTDScsv <- downloadHandler( # TDS entree telechargement csv summary
        filename = function() {
            paste("Dominance", ".csv", sep = '')
        },
        content = function(file) {
            write.csv2(ActTDS()$dominances, file, row.names = FALSE)
        }
    )
    
    output$downloadDominanceTDSxlsx <- downloadHandler( # TDS entree telechargement xlsx summary
        filename = function() {
            paste("Dominance", ".xlsx", sep = '')
        },
        content = function(file) {
            write.xlsx(ActTDS()$dominances, file, row.names = FALSE)
        }
    )
    
    output$downloadDominanceStTDScsv <- downloadHandler( # TDS entree telechargement csv summary
        filename = function() {
            paste("DominanceSt", ".csv", sep = '')
        },
        content = function(file) {
            write.csv2(ActTDS()$stdDominances, file, row.names = FALSE)
        }
    )
    
    output$downloadDominanceStTDSxlsx <- downloadHandler( # TDS entree telechargement xlsx summary
        filename = function() {
            paste("DominanceSt", ".xlsx", sep = '')
        },
        content = function(file) {
            write.xlsx(ActTDS()$stdDominances, file, row.names = FALSE)
        }
    )
    
    output$downloadDurationTDScsv <- downloadHandler( # TDS entree telechargement csv summary
        filename = function() {
            paste("Duration", ".csv", sep = '')
        },
        content = function(file) {
            write.csv2(ActTDS()$durations, file, row.names = FALSE)
        }
    )
    
    output$downloadDurationTDSxlsx <- downloadHandler( # TDS entree telechargement xlsx summary
        filename = function() {
            paste("Duration", ".xlsx", sep = '')
        },
        content = function(file) {
            write.xlsx(ActTDS()$durations, file, row.names = FALSE)
        }
    )
    
    output$downloadCitationTDScsv <- downloadHandler( # TDS entree telechargement csv summary
        filename = function() {
            paste("Citation", ".csv", sep = '')
        },
        content = function(file) {
            write.csv2(ActTDS()$citations, file, row.names = FALSE)
        }
    )
    
    output$downloadCitationTDSxlsx <- downloadHandler( # TDS entree telechargement xlsx summary
        filename = function() {
            paste("Citation", ".xlsx", sep = '')
        },
        content = function(file) {
            write.xlsx(ActTDS()$citations, file, row.names = FALSE)
        }
    )
    
    output$downloadbehavioursTDScsv <- downloadHandler( # TDS entree telechargement csv summary
        filename = function() {
            paste("behaviours", ".csv", sep = '')
        },
        content = function(file) {
            write.csv2(ActTDS()$behaviours, file, row.names = FALSE)
        }
    )
    
    output$downloadbehavioursTDSxlsx <- downloadHandler( # TDS entree telechargement xlsx summary
        filename = function() {
            paste("behaviours", ".xlsx", sep = '')
        },
        content = function(file) {
            write.xlsx(ActTDS()$behaviours, file, row.names = FALSE)
        }
    )
    
    output$tabProductTDS <- renderDataTable({ # liste des produits TDS
        dat = data.frame(levels(ActTDS()$behaviours$product))
        names(dat) = 'Products'
        dat
    })
    
    output$tabSubjectTDS <- renderDataTable({ # liste des sujets TDS
        dat = data.frame(levels(ActTDS()$behaviours$subject))
        names(dat) = 'Subjects'
        dat
    })
    
    output$affNbSubTDS <- renderText({ # TDS entree nombre sujet
        as.character(nlevels(ActTDS()$behaviours$subject))
    })
    
    output$affNbProductTDS <- renderText({ # TDS entree nombre produit
        as.character(nlevels(ActTDS()$behaviours$product))
    })
    
    output$affNbRepTDS <- renderText({ # TDS entree nombre replicate
        as.character(nlevels(ActTDS()$behaviours$rep))
    })
    
    output$presentationTDS <- renderText({ # TDS sortie phrase de resume
        if (choiceL() == 'en'){
            sub = 'subject'
            prod = 'product'
            rep = 'replicate'
            andet = 'and'
        }
        else{
            sub = 'sujet'
            prod = 'produit'
            rep = 'replique'
            andet = 'et'
        }
        if (nlevels(ActTDS()$behaviours$subject) > 1){
            presSubTDS <- paste(sub,'s',sep='')
        }
        else{
            presSubTDS <- sub
        }
        if (nlevels(ActTDS()$behaviours$product) > 1){
            presProdTDS <- paste(prod,'s',sep='')
        }
        else{
            presProdTDS <- prod
        }
        if (nlevels(ActTDS()$behaviours$rep) > 1){
            presRepTDS <- paste(rep,'s',sep='')
        }
        else{
            presRepTDS <- rep
        }
        paste(as.character(nlevels(ActTDS()$behaviours$subject)), presSubTDS, ',', as.character(nlevels(ActTDS()$behaviours$product)), presProdTDS, andet, as.character(nlevels(ActTDS()$behaviours$rep)), presRepTDS)
    })
    
    output$titleRapportTDS <- renderUI({ # TDS nom du rapport html
        textInput('titleTDS', lexi['titleRapportTDS', choiceL()], value = 'Report TDS')
    })
    
    output$htmlTDS <- downloadHandler( # TDS entree rapport html pour analyses TDS 
        filename = function() {
            "report.html"
        },
        content = function(file) {
            
            tempReport <- file.path("www/report.Rmd")
            file.copy("report.Rmd", tempReport, overwrite = TRUE)
            
            if (isTruthy(input$actAna1TDS)){
                a2rapport = kable_styling(a2TDS()[[1]]$output$panelBehaviour)
                a3rapport = kable_styling(a3TDS()[[1]]$output$anova)
            }
            else{
                a2r = analysis(ActTDS(),type="Panel behaviour table", title="Panel behaviour table",sequenceStart=15,sequenceDuration=6,nbDescriptors=1,nbClicks=1.5,descriptorDuration=2)
                graphics.off()
                a3r = analysis(ActTDS(),type="ANOVA of behaviours", title="ANOVA of behaviours", columns=c("G_Mean","F_Product","P_Product","SE_Product","R2","P_ShapiroWilks","P_Levene_Product","Mean_Product","Group_Product"), alpha=0.05)
                graphics.off()
                a2rapport = kable_styling(a2r[[1]]$output$panelBehaviour)
                a3rapport = kable_styling(a3r[[1]]$output$anova)
            }
            
            if (isTruthy(input$actAna2TDS)){
                b1rapport = b1TDS()[[1]]$output$sequences
                b23rapport = b23TDS()[[1]]$output$dominanceCurves
                b89rapport = b89TDS()[[1]]$output$panelSequences
                d9rapport = d9TDS()[[1]]$output$biplot
            }
            else {
                b1r = analysis(ActTDS(),type="Individual sequences", title="Individual sequences")
                graphics.off()
                b23r = analysis(ActTDS(),type="Standardized dominance curves", title="Dominance curves", alpha=0.05, repAsIndividual=TRUE, rows="product", cols="rep", color="descriptor", smooth=TRUE, draw=c("significance","hasard","graymask"))
                graphics.off()
                b89r = analysis(ActTDS(),type="Standardized panel sequences", title="Panel bandplot", alpha=0.05, repAsIndividual=TRUE)
                graphics.off()
                d9r = analysis(ActTDS(),type="PCA of trajectories", title="PCA of trajectories", axes=list(c(1,2)), periods=7, fontSizeCex = 2)
                graphics.off()
                b1rapport = b1r[[1]]$output$sequences
                b23rapport = b23r[[1]]$output$dominanceCurves
                b89rapport = b89r[[1]]$output$panelSequences
                d9rapport = d9r[[1]]$output$biplot
            }
            
            if (isTruthy(input$actAna3TDS)){
                d3rapport = kable_styling(d3TDS()[[1]]$output$anova)
                d56rapport = d56TDS()[[1]]$output$biplot
                d7rapport = d7TDS()[[1]]$output$biplot
            }
            else {
                d3r = analysis(ActTDS(),type="ANOVA of durations", title="ANOVA of durations", columns=c("G_Mean","F_Product","P_Product","F_Product_Significance","SE_Product","R2","P_ShapiroWilks","P_Levene_Product","Mean_Product","Group_Product","P_Subject"), alpha=0.05)
                graphics.off()
                d56r = analysis(ActTDS(),type="PCA of durations", title="PCA", fontSizeCex = 2)
                graphics.off()
                d7r = analysis(ActTDS(),type="CVA of durations", title="CVA", fontSizeCex = 2)
                graphics.off()
                d3rapport = kable_styling(d3r[[1]]$output$anova)
                d56rapport = d56r[[1]]$output$biplot
                d7rapport = d7r[[1]]$output$biplot
            }
            
            params <- list(
                titleRTDS = input$titleTDS,
                printa2 = a2rapport,
                printa3 = a3rapport,
                plotb1 = b1rapport,
                plotb23 = b23rapport,
                plotb89 = b89rapport,
                plotd9 = d9rapport,
                printd3 = d3rapport,
                plotd56 = d56rapport,
                plotd7 = d7rapport
            )
            
            rmarkdown::render(tempReport, output_file = file,
                              params = params,
                              envir = new.env(parent = globalenv())
            )
            
    })
    
    output$zipTDS <- downloadHandler( # TDS entree rapport zip pour analyses TDS 
        filename = function() {
            "TDS.zip"
        },
        content = function(fname) {
            
            fs <- 1:9
            fs[1] <- paste0(tempdir(),"\\panelBehaviour.html")
            fs[2] <- paste0(tempdir(),"\\BehaviourAnova.html")
            fs[3] <- paste0(tempdir(),"\\individualSequences.png")
            fs[4] <- paste0(tempdir(),"\\dominanceCurves.png")
            fs[5] <- paste0(tempdir(),"\\panelSequences.png")
            fs[6] <- paste0(tempdir(),"\\PCAtrajectories.png")
            fs[7] <- paste0(tempdir(),"\\anovaDurations.html")
            fs[8] <- paste0(tempdir(),"\\PCAdurations.png")
            fs[9] <- paste0(tempdir(),"\\CVAdurations.png")
            fs[10] <- paste0(tempdir(),"\\panelBehaviour.csv")
            fs[11] <- paste0(tempdir(),"\\BehaviourAnova.csv")
            fs[12] <- paste0(tempdir(),"\\anovaDurations.csv")
            
            if (isTruthy(input$actAna1TDS)){
                a2rapport = kable_styling(a2TDS()[[1]]$output$panelBehaviour)
                a2rapportDF = a2TDS()[[1]]$df
                a3rapport = kable_styling(a3TDS()[[1]]$output$anova)
                a3rapportDF = a3TDS()[[1]]$output$result
            }
            else{
                a2r = analysis(ActTDS(),type="Panel behaviour table", title="Panel behaviour table",sequenceStart=15,sequenceDuration=6,nbDescriptors=1,nbClicks=1.5,descriptorDuration=2)
                graphics.off()
                a3r = analysis(ActTDS(),type="ANOVA of behaviours", title="ANOVA of behaviours", columns=c("G_Mean","F_Product","P_Product","SE_Product","R2","P_ShapiroWilks","P_Levene_Product","Mean_Product","Group_Product"), alpha=0.05)
                graphics.off()
                a2rapport = kable_styling(a2r[[1]]$output$panelBehaviour)
                a2rapportDF = a2r[[1]]$df
                a3rapport = kable_styling(a3r[[1]]$output$anova)
                a3rapportDF = a3r[[1]]$output$result
            }
            
            if (isTruthy(input$actAna2TDS)){
                b1rapport = b1TDS()[[1]]$output$sequences
                b23rapport = b23TDS()[[1]]$output$dominanceCurves
                b89rapport = b89TDS()[[1]]$output$panelSequences
                d9rapport = d9TDS()[[1]]$output$biplot
            }
            else {
                b1r = analysis(ActTDS(),type="Individual sequences", title="Individual sequences")
                graphics.off()
                b23r = analysis(ActTDS(),type="Standardized dominance curves", title="Dominance curves", alpha=0.05, repAsIndividual=TRUE, rows="product", cols="rep", color="descriptor", smooth=TRUE, draw=c("significance","hasard","graymask"))
                graphics.off()
                b89r = analysis(ActTDS(),type="Standardized panel sequences", title="Panel bandplot", alpha=0.05, repAsIndividual=TRUE)
                graphics.off()
                d9r = analysis(ActTDS(),type="PCA of trajectories", title="PCA of trajectories", axes=list(c(1,2)), periods=7, fontSizeCex = 2)
                graphics.off()
                b1rapport = b1r[[1]]$output$sequences
                b23rapport = b23r[[1]]$output$dominanceCurves
                b89rapport = b89r[[1]]$output$panelSequences
                d9rapport = d9r[[1]]$output$biplot
            }
            
            if (isTruthy(input$actAna3TDS)){
                d3rapport = kable_styling(d3TDS()[[1]]$output$anova)
                d3rapportDF = d3TDS()[[1]]$output$result
                d56rapport = d56TDS()[[1]]$output$biplot
                d7rapport = d7TDS()[[1]]$output$biplot
            }
            else {
                d3r = analysis(ActTDS(),type="ANOVA of durations", title="ANOVA of durations", columns=c("G_Mean","F_Product","P_Product","F_Product_Significance","SE_Product","R2","P_ShapiroWilks","P_Levene_Product","Mean_Product","Group_Product","P_Subject"), alpha=0.05)
                graphics.off()
                d56r = analysis(ActTDS(),type="PCA of durations", title="PCA", fontSizeCex = 2)
                graphics.off()
                d7r = analysis(ActTDS(),type="CVA of durations", title="CVA", fontSizeCex = 2)
                graphics.off()
                d3rapport = kable_styling(d3r[[1]]$output$anova)
                d3rapportDF = d3r[[1]]$output$result
                d56rapport = d56r[[1]]$output$biplot
                d7rapport = d7r[[1]]$output$biplot
            }
            
            save_html(file = fs[1], html = a2rapport)
            write.csv2(a2rapportDF, file = fs[10], row.names = FALSE)
            save_html(file = fs[2], html = a3rapport)
            write.csv2(a3rapportDF, file = fs[11], row.names = FALSE)
            ggsave(fs[3], b1rapport)
            ggsave(fs[4], b23rapport)
            ggsave(fs[5], b89rapport)
            ggsave(fs[6], d9rapport)
            save_html(file = fs[7], html = d3rapport)
            write.csv2(d3rapportDF, file = fs[12], row.names = FALSE)
            ggsave(fs[8], d56rapport)
            ggsave(fs[9], d7rapport)
            zip::zipr(zipfile=fname, files=fs)
    }, contentType = "application/zip")
    
    output$individualSequencesTDSpng <- downloadHandler( # TDS entree telechargement png Individual Sequences Product Temporality
        filename = function() {
            paste("individualSequences", ".png", sep = '')
        },
        content = function(file) {
            device <- function(..., width, height) {
                grDevices::png(..., width = width, height = height,
                               res = 300, units = "in")
            }
            ggsave(file, plot = b1TDS()[[1]]$output$sequences, device = device)
    })
    
    output$individualSequencesTDSsvg <- downloadHandler( # TDS entree telechargement svg Individual Sequences Product Temporality
        filename = function() {
            paste("individualSequences", ".svg", sep = '')
        },
        content = function(file) {
            device <- function(..., width, height) {
                grDevices::svg(..., width = width, height = height)
            }
            ggsave(file, plot = b1TDS()[[1]]$output$sequences, device = device)
        }
    )
    
    output$PanelSequencesTDSpng <- downloadHandler( # TDS entree telechargement png Panel Bandplot Product Temporality
        filename = function() {
            paste("PanelSequences", ".png", sep = '')
        },
        content = function(file) {
            device <- function(..., width, height) {
                grDevices::png(..., width = width, height = height,
                               res = 300, units = "in")
            }
            ggsave(file, plot = b89TDS()[[1]]$output$panelSequences, device = device)
        }
    )
    
    output$PanelSequencesTDSsvg <- downloadHandler( # TDS entree telechargement svg Panel Bandplot Product Temporality
        filename = function() {
            paste("PanelSequences", ".svg", sep = '')
        },
        content = function(file) {
            device <- function(..., width, height) {
                grDevices::svg(..., width = width, height = height)
            }
            ggsave(file, plot = b89TDS()[[1]]$output$panelSequences, device = device)
        }
    )
    
    output$PCAtrajectoriesTDSpng <- downloadHandler( # TDS entree telechargement png PCA des trajectoires Product Temporality
        filename = function() {
            paste("PCAtrajectories", ".png", sep = '')
        },
        content = function(file) {
            device <- function(..., width, height) {
                grDevices::png(..., width = width, height = height,
                               res = 300, units = "in")
            }
            ggsave(file, plot = d9TDS()[[1]]$output$biplot, device = device)
        }
    )
    
    output$PCAtrajectoriesTDSsvg <- downloadHandler( # TDS entree telechargement svg PCA des trajectoires Product Temporality
        filename = function() {
            paste("PCAtrajectories", ".svg", sep = '')
        },
        content = function(file) {
            device <- function(..., width, height) {
                grDevices::svg(..., width = width, height = height)
            }
            ggsave(file, plot = d9TDS()[[1]]$output$biplot, device = device)
        }
    )
    
    output$DominanceCurvesTDSpng <- downloadHandler( # TDS entree telechargement png Dominances Curves Product Temporality
        filename = function() {
            paste("DominanceCurves", ".png", sep = '')
        },
        content = function(file) {
            device <- function(..., width, height) {
                grDevices::png(..., width = width, height = height,
                               res = 300, units = "in")
            }
            ggsave(file, plot = b23TDS()[[1]]$output$dominanceCurves, device = device)
        }
    )
    
    output$DominanceCurvesTDSsvg <- downloadHandler( # TDS entree telechargement svg Dominances Curves Product Temporality
        filename = function() {
            paste("DominanceCurves", ".svg", sep = '')
        },
        content = function(file) {
            device <- function(..., width, height) {
                grDevices::svg(..., width = width, height = height)
            }
            ggsave(file, plot = b23TDS()[[1]]$output$dominanceCurves, device = device)
        }
    )
    
    output$citDistTDSpng <- downloadHandler( # TDS telechargement png custom analysis
        filename = function() {
            paste("citationsDistribution", ".png", sep = '')
        },
        content = function(file) {
            device <- function(..., width, height) {
                grDevices::png(..., width = width, height = height,
                               res = 300, units = "in")
            }
            ggsave(file, plot = c1TDS()[[1]]$output$citationsDistribution, device = device)
        }
    )
    
    output$citDistTDSsvg <- downloadHandler( # TDS telechargement svg custom analysis
        filename = function() {
            paste("citationsDistribution", ".svg", sep = '')
        },
        content = function(file) {
            device <- function(..., width, height) {
                grDevices::svg(..., width = width, height = height)
            }
            ggsave(file, plot = c1TDS()[[1]]$output$citationsDistribution, device = device)
        }
    )
    
    output$BehaviourDistTDSpng <- downloadHandler( # TDS telechargement png custom analysis
        filename = function() {
            paste("panelBehaviourDistribution", ".png", sep = '')
        },
        content = function(file) {
            device <- function(..., width, height) {
                grDevices::png(..., width = width, height = height,
                               res = 300, units = "in")
            }
            ggsave(file, plot = a1TDS()[[1]]$output$panelBehaviourDistribution, device = device)
        }
    )
    
    output$BehaviourDistTDSsvg <- downloadHandler( # TDS telechargement svg custom analysis
        filename = function() {
            paste("panelBehaviourDistribution", ".svg", sep = '')
        },
        content = function(file) {
            device <- function(..., width, height) {
                grDevices::svg(..., width = width, height = height)
            }
            ggsave(file, plot = a1TDS()[[1]]$output$panelBehaviourDistribution, device = device)
        }
    )
    
    output$ClustDurationTDSpng <- downloadHandler( # TDS telechargement png custom analysis
        filename = function() {
            paste("durationsClusters", ".png", sep = '')
        },
        content = function(file) {
            device <- function(..., width, height) {
                grDevices::png(..., width = width, height = height,
                               res = 300, units = "in")
            }
            ggsave(file, plot = a4TDS()[[1]]$output$durationsClusters, device = device)
        }
    )
    
    output$ClustDurationTDSsvg <- downloadHandler( # TDS telechargement svg custom analysis
        filename = function() {
            paste("durationsClusters", ".svg", sep = '')
        },
        content = function(file) {
            device <- function(..., width, height) {
                grDevices::svg(..., width = width, height = height)
            }
            ggsave(file, plot = a4TDS()[[1]]$output$durationsClusters, device = device)
        }
    )
    
    output$DiffDomCurvTDSpng <- downloadHandler( # TDS telechargement png custom analysis
        filename = function() {
            paste("differenceCurves", ".png", sep = '')
        },
        content = function(file) {
            device <- function(..., width, height) {
                grDevices::png(..., width = width, height = height,
                               res = 300, units = "in")
            }
            ggsave(file, plot = b6TDS()[[1]]$output$differenceCurves, device = device)
        }
    )
    
    output$DiffDomCurvTDSsvg <- downloadHandler( # TDS telechargement svg custom analysis
        filename = function() {
            paste("differenceCurves", ".svg", sep = '')
        },
        content = function(file) {
            device <- function(..., width, height) {
                grDevices::svg(..., width = width, height = height)
            }
            ggsave(file, plot = b6TDS()[[1]]$output$differenceCurves, device = device)
        }
    )
    
    output$BarCitTDSpng <- downloadHandler( # TDS telechargement png custom analysis
        filename = function() {
            paste("barplotCitation", ".png", sep = '')
        },
        content = function(file) {
            device <- function(..., width, height) {
                grDevices::png(..., width = width, height = height,
                               res = 300, units = "in")
            }
            ggsave(file, plot = c2TDS()[[1]]$output$barplot, device = device)
        }
    )
    
    output$BarCitTDSsvg <- downloadHandler( # TDS telechargement svg custom analysis
        filename = function() {
            paste("barplotCitation", ".svg", sep = '')
        },
        content = function(file) {
            device <- function(..., width, height) {
                grDevices::svg(..., width = width, height = height)
            }
            ggsave(file, plot = c2TDS()[[1]]$output$barplot, device = device)
        }
    )
    
    output$DurDistTDSpng <- downloadHandler( # TDS telechargement png custom analysis
        filename = function() {
            paste("durationsDistribution", ".png", sep = '')
        },
        content = function(file) {
            device <- function(..., width, height) {
                grDevices::png(..., width = width, height = height,
                               res = 300, units = "in")
            }
            ggsave(file, plot = d1TDS()[[1]]$output$durationsDistribution, device = device)
        }
    )
    
    output$DurDistTDSsvg <- downloadHandler( # TDS telechargement svg custom analysis
        filename = function() {
            paste("durationsDistribution", ".svg", sep = '')
        },
        content = function(file) {
            device <- function(..., width, height) {
                grDevices::svg(..., width = width, height = height)
            }
            ggsave(file, plot = d1TDS()[[1]]$output$durationsDistribution, device = device)
        }
    )
    
    output$BarDurTDSpng <- downloadHandler( # TDS telechargement png custom analysis
        filename = function() {
            paste("barplotDuration", ".png", sep = '')
        },
        content = function(file) {
            device <- function(..., width, height) {
                grDevices::png(..., width = width, height = height,
                               res = 300, units = "in")
            }
            ggsave(file, plot = d2TDS()[[1]]$output$barplot, device = device)
        }
    )
    
    output$BarDurTDSsvg <- downloadHandler( # TDS telechargement svg custom analysis
        filename = function() {
            paste("barplotDuration", ".svg", sep = '')
        },
        content = function(file) {
            device <- function(..., width, height) {
                grDevices::svg(..., width = width, height = height)
            }
            ggsave(file, plot = d2TDS()[[1]]$output$barplot, device = device)
        }
    )
    
    output$bodyTDS <- renderUI({
        
        if (input$actionTDS){
            navbarPage("", id = "tabselected",
                       tabPanel(lexi['tdsSummary', choiceL()], value = 1,
                                tabsetPanel(
                                    tabPanel(lexi['tdsUnit', choiceL()],
                                             fluidRow(
                                                 textOutput('presentationTDS')
                                             ),
                                             fluidRow(
                                                 box(title = 'products', width = 6, solidHeader = TRUE, dataTableOutput('tabProductTDS')),
                                                 box(title = 'subjects', width = 6, solidHeader = TRUE, dataTableOutput('tabSubjectTDS'))
                                             ),
                                    ),
                                    tabPanel(lexi['tdsDF', choiceL()],
                                             dataTableOutput('dfTDS'),
                                             downloadButton("downloadtdsDFcsv", "csv"),
                                             downloadButton("downloadtdsDFxlsx", "xlsx")),
                                    tabPanel(lexi['tdsDominances', choiceL()],
                                             dataTableOutput('dominancesTDS'),
                                             downloadButton("downloadDominanceTDScsv", "csv"),
                                             downloadButton("downloadDominanceTDSxlsx", "xlsx")),
                                    tabPanel(lexi['tdsDominancesSTD', choiceL()],
                                             dataTableOutput('stdDominancesTDS'),
                                             downloadButton("downloadDominanceStTDScsv", "csv"),
                                             downloadButton("downloadDominanceStTDSxlsx", "xlsx")),
                                    tabPanel(lexi['tdsDurations', choiceL()],
                                             dataTableOutput('durationsTDS'),
                                             downloadButton("downloadDurationTDScsv", "csv"),
                                             downloadButton("downloadDurationTDSxlsx", "xlsx")),
                                    tabPanel(lexi['tdsCitations', choiceL()],
                                             dataTableOutput('citationsTDS'),
                                             downloadButton("downloadCitationTDScsv", "csv"),
                                             downloadButton("downloadCitationTDSxlsx", "xlsx")),
                                    tabPanel(lexi['tdsBehaviours', choiceL()],
                                             dataTableOutput('behavioursTDS'),
                                             downloadButton("downloadbehavioursTDScsv", "csv"),
                                             downloadButton("downloadbehavioursTDSxlsx", "xlsx"))
                                )
                       ),
                       
                       tabPanel(lexi['analysis1TDS', choiceL()], value = 2,
                                tabsetPanel(
                                    tabPanel(lexi['tdsPanelistTable', choiceL()],
                                             htmlOutput('BehaviourPanelTDS'),
                                             downloadButton("downloadpanelBehaviourTDS", "html"),
                                             downloadButton("downloadpanelBehaviourTDScsv", "csv")),
                                    tabPanel(lexi['tdsAnovaBehaviour', choiceL()], value = 'ANOVA of behaviours',
                                             htmlOutput('BehaviourAnovaTDS'),
                                             downloadButton("downloadpanelBehaviourAnovaTDS", "html"),
                                             downloadButton("downloadpanelBehaviourAnovaTDScsv", "csv"))
                                    #tabPanel(lexi['tdsbandplotPanelist', calc$choiceL], value = 'Bandplot by panelist',
                                    #         plotOutput('bandplotPanelistTDS', height = "800px"))
                                    #tabPanel('Clusters of subjects durations',
                                    #         plotOutput('ClustDurationTDS', height = "800px"))
                                )
                       ),
                       
                       tabPanel(lexi['analysis2TDS', choiceL()], value=3,
                                tabsetPanel(
                                    tabPanel(lexi['individualSequencesTDS', choiceL()],
                                             plotOutput('indSeqTDS', height = "800px"),
                                             downloadButton("individualSequencesTDSpng", "png"),
                                             downloadButton("individualSequencesTDSsvg", "svg")
                                    ),
                                    tabPanel(lexi['TDScurveDominances', choiceL()],
                                             plotOutput('domCurTDS', height = "800px"),
                                             downloadButton("DominanceCurvesTDSpng", "png"),
                                             downloadButton("DominanceCurvesTDSsvg", "svg")
                                    ),
                                    tabPanel(lexi['panelBandplotTDS', choiceL()],
                                             plotOutput('PanelSequencesTDS', height = "800px"),
                                             downloadButton("PanelSequencesTDSpng", "png"),
                                             downloadButton("PanelSequencesTDSsvg", "svg")
                                    ),
                                    #tabPanel('PCA of trajectories',
                                    #         imageOutput('PCAtrajectoriesTDS'))
                                    #tabPanel('PCA of trajectories',
                                    #         div(style="display: block; margin-left: auto; margin-right: auto;",imageOutput('PCAtrajectoriesTDS'))
                                    #),
                                    tabPanel(lexi['TDSpcaTrajectories', choiceL()],
                                             plotOutput('PCAtrajectoriesTDS', height = "800px"),
                                             downloadButton("PCAtrajectoriesTDSpng", "png"),
                                             downloadButton("PCAtrajectoriesTDSsvg", "svg")
                                    )
                                    #tabPanel('Maximum dominance rates',
                                    #         plotOutput('maxDomTDS', height = "800px"))
                                )
                       ),
                       
                       tabPanel(lexi['analysis3TDS', choiceL()], value=6,
                                tabsetPanel(
                                    tabPanel(lexi['anovaDurationsTDS', choiceL()],
                                             htmlOutput('AnovaDurationsTDS'),
                                             downloadButton("AnovaDurationsTDShtml", "html"),
                                             downloadButton("AnovaDurationsTDScsv", "csv")
                                    ),
                                    tabPanel(lexi['pcaDurationsTDS', choiceL()],
                                             plotOutput('PCATDS', height = "800px"),
                                             downloadButton("PCATDSpng", "png"),
                                             downloadButton("PCATDSsvg", "svg")
                                    ),
                                    tabPanel(lexi['cvaDurationsTDS', choiceL()],
                                             plotOutput('CVATDS', height = "800px"),
                                             downloadButton("CVATDSpng", "png"),
                                             downloadButton("CVATDSsvg", "svg")
                                    )
                                )
                       ),
                       
                       tabPanel(lexi['downloadMenuTDS', choiceL()], value=7,
                                
                       ),
                       
                       tabPanel(lexi['customAnalysisTDS', choiceL()], value=4,
                                tabsetPanel(id = 'carteTDS',
                                            tabPanel('Citation distribution', value = 'Citation distribution',
                                                     plotOutput('citDistTDS', height = "800px"),
                                                     downloadButton("citDistTDSpng", "png"),
                                                     downloadButton("citDistTDSsvg", "svg")
                                            ),
                                            tabPanel('Panel behaviour distribution',
                                                     plotOutput('BehaviourDistTDS', height = "800px"),
                                                     downloadButton("BehaviourDistTDSpng", "png"),
                                                     downloadButton("BehaviourDistTDSsvg", "svg")
                                            ),
                                            tabPanel('Clusters of subjects durations',
                                                     plotOutput('ClustDurationTDS', height = "800px"),
                                                     downloadButton("ClustDurationTDSpng", "png"),
                                                     downloadButton("ClustDurationTDSsvg", "svg")
                                            ),
                                            tabPanel('Differences of dominance curves',
                                                     plotOutput('DiffDomCurvTDS', height = "800px"),
                                                     downloadButton("DiffDomCurvTDSpng", "png"),
                                                     downloadButton("DiffDomCurvTDSsvg", "svg")
                                            ),
                                            tabPanel('Barplot of citations',
                                                     plotOutput('BarCitTDS', height = "800px"),
                                                     downloadButton("BarCitTDSpng", "png"),
                                                     downloadButton("BarCitTDSsvg", "svg")
                                            ),
                                            tabPanel('Duration distribution',
                                                     plotOutput('DurDistTDS', height = "800px"),
                                                     downloadButton("DurDistTDSpng", "png"),
                                                     downloadButton("DurDistTDSsvg", "svg")
                                            ),
                                            tabPanel('Barplot of durations',
                                                     plotOutput('BarDurTDS', height = "800px"),
                                                     downloadButton("BarDurTDSpng", "png"),
                                                     downloadButton("BarDurTDSsvg", "svg")
                                            ),
                                            tabPanel('PCA of durations by rep',
                                                     plotOutput('PCATDSRep', height = "800px"),
                                                     downloadButton("PCATDSpngRep", "png"),
                                                     downloadButton("PCATDSsvgRep", "svg")
                                            ),
                                            tabPanel('CVA of durations by rep',
                                                     plotOutput('CVATDSRep', height = "800px"),
                                                     downloadButton("CVATDSpngRep", "png"),
                                                     downloadButton("CVATDSsvgRep", "svg")
                                            ),
                                            tabPanel('ANOVA of durations by rep',
                                                     htmlOutput('AnovaDurationsTDSrep'),
                                                     downloadButton("AnovaDurationsTDShtmlrep", "html"),
                                                     downloadButton("AnovaDurationsTDScsvrep", "csv")
                                            )
                                )
                       )
            )
        }
    })
    
})



