################################################################################
# serv calc
################################################################################

serv_calc <- list()

serv_calc[[5]] <- function(calc, sess){
  calc[["choiceL"]] <- 'en'
  observeEvent(calc$chL == 'francais' | calc$chL == 'english',{
    if (calc$chL == 'francais') {
      L = 'fr'
    }
    else if (calc$chL == 'english') {
      L = 'en'
    }
    calc[["choiceL"]] <- L
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
}


################################################################################
# serv calc FC 1-7
################################################################################

serv_calc[[1]] <- function(calc, sess){
  observeEvent(calc$actionFC,{
    shinyjs::hide('data')
    shinyjs::hide('lexique')
    shinyjs::hide('specificWordsTabNameFC')
    shinyjs::hide('subject')
    shinyjs::hide('product')
    shinyjs::hide('description')
    shinyjs::hide('citation')
    shinyjs::hide('separatorFC')
    shinyjs::hide('actionFC')
    shinyjs::hide('bsspecificWordsTabNameFC')
    shinyjs::hide('bssubject')
    shinyjs::hide('bsproduct')
    shinyjs::hide('bsdescription')
    shinyjs::hide('bscitation')
    shinyjs::hide('bsseparatorFC')
    shinyjs::hide('bsdata')
    shinyjs::hide('bslexique')
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
}

serv_calc[[2]] <- function(calc, sess){
  observeEvent(calc$actionFC,{
    fc=fcRead(file=calc$data$datapath, sep=calc$separatorFC, cols=list(subject=calc$subject, product=calc$product, text=calc$description),pathToLexicon=calc$lexique$datapath, specificWordsTabName=calc$specificWordsTabNameFC, minCitations=calc$citation)
    calc[["Act_table"]] <- fc
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
}

serv_calc[[3]] <- function(calc, sess){
  observeEvent(calc$actionLexiconFC,{
    fc=fcRead(file=calc$data$datapath, sep=calc$separatorFC, cols=list(subject=calc$subject, product=calc$product, text=calc$description),pathToLexicon=calc$lexique$datapath, specificWordsTabName=calc$specificWordsTabNameFC, minCitations=calc$citation)
    fc = addSpecificLexiconEntry(fc, calc$wordLexicon, calc$lemmaLexicon)
    calc[["Act_table"]] <- fc
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
}

serv_calc[[4]] <- function(calc, sess){
  observeEvent(calc$actionAnalysisFC,{
    fco=fcRead(file=calc$data$datapath, sep=calc$separatorFC, cols=list(subject=calc$subject, product=calc$product, text=calc$description),pathToLexicon=calc$lexique$datapath, specificWordsTabName=calc$specificWordsTabNameFC, minCitations=calc$citation)
    if (calc$axFC == 'signif'){
      r=analysis(fco,type="MRCA", title="MR CA", nBoot=calc$nbFC, alpha=calc$alFC, nbAxes=calc$axFC, twoSided=FALSE, choice=calc$chFC)
    }
    else {
      axes = as.numeric(calc$axFC)
      r=analysis(fco,type="MRCA", title="MR CA", nBoot=calc$nbFC, alpha=calc$alFC, nbAxes=axes, twoSided=FALSE, choice=calc$chFC)
    }
    graphics.off()
    calc[["Act_tableFCAnalyse"]] <- r
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
}

serv_calc[[6]] <- function(calc, sess){
  observeEvent(calc$actionFC,{
    nbSub = nlevels(calc$Act_table$contingencyTable[,1])
    calc[["nbSub"]] <- nbSub
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
}

serv_calc[[7]] <- function(calc, sess){
  observeEvent(calc$actionFC,{
    nbProd = nlevels(calc$Act_table$contingencyTable[,2])
    calc[["nbProd"]] <- nbProd
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
}

serv_calc[[35]] <- function(calc, sess){
  observeEvent(calc$data,{
    lectureFC = read.csv2(calc$data$datapath)
    choix_FC_table <- names(lectureFC)
    updateSelectInput(sess, 'subject',
                      choices = choix_FC_table
    )
    updateSelectInput(sess, 'product',
                      choices = choix_FC_table
    )
    updateSelectInput(sess, 'description',
                      choices = choix_FC_table
    )
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
}

################################################################################
# serv calc TDS
################################################################################

serv_calc[[8]] <- function(calc, sess){
  observeEvent(calc$actionTDS,{
    #shinyjs::hide('dataTDS')
    #shinyjs::hide('descriptorTDS')
    #shinyjs::hide('timeTDS')
    #shinyjs::hide('scoreTDS')
    #shinyjs::hide('subjectTDS')
    #shinyjs::hide('productTDS')
    #shinyjs::hide('replicateTDS')
    #shinyjs::hide('separatorTDS')
    #shinyjs::hide('startWithFirstCitationTDS')
    #shinyjs::hide('discretizationTDS')
    #shinyjs::hide('bsdataTDS')
    #shinyjs::hide('bsdescriptorTDS')
    #shinyjs::hide('bstimeTDS')
    #shinyjs::hide('bsscoreTDS')
    #shinyjs::hide('bssubjectTDS')
    #shinyjs::hide('bsproductTDS')
    #shinyjs::hide('bsreplicateTDS')
    #shinyjs::hide('bsseparatorTDS')
    #shinyjs::hide('bsstartWithFirstCitationTDS')
    #shinyjs::hide('bsdiscretizationTDS')
    #shinyjs::hide('listeDeroulanteActionTDS')
    #shinyjs::hide('bslisteDeroulanteActionTDS')
    #shinyjs::hide('actionTDS')
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
}

serv_calc[[9]] <- function(calc, sess){
  observeEvent(calc$actionTDS,{
    
    tryCatch({
      if (isTruthy(calc$sidebarItemExpanded == 'ListTDS')){
        tds = tdsRead(file=calc$dataTDS$datapath, cols=list(subject=calc$subjectTDS, product=calc$productTDS, descriptor=calc$descriptorTDS,time=calc$timeTDS,score=calc$scoreTDS,rep=calc$replicateTDS), supCols="", sep=calc$separatorTDS,startWithFirstCitation=calc$startWithFirstCitationTDS,discretization=calc$discretizationTDS,periods=1)
      }
      else{
        tds = tdsRead(file=calc$dataTDS$datapath, cols=list(subject="SubjectCode", product="ProductCode", descriptor="AttributeCode",time="Time",score="Score",rep="Replicate"), supCols="", sep=calc$separatorTDS,startWithFirstCitation=calc$startWithFirstCitationTDS,discretization=calc$discretizationTDS,periods=1)
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
      calc[["ActTDS"]] <- tds
    },
    warning = function(warn){
      showNotification(paste0(warn), type = 'warning')
    },
    error = function(err){
      showNotification(paste0(err), type = 'err')
    })
    
    
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
}

serv_calc[[10]] <- function(calc, sess){
  observeEvent(calc$a1actionchTDS,{
    a1=analysis(calc$ActTDS,type="Panel behaviour distribution", title="Panel behaviour distribution")
    graphics.off()
    calc[["a1TDS"]] <- a1
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
}

serv_calc[[11]] <- function(calc, sess){
  observeEvent(calc$a4actionchTDS,{
    a4=analysis(calc$ActTDS,type="Clusters of subjects durations", title="Clusters of subjects durations")
    graphics.off()
    calc[["a4TDS"]] <- a4
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
}

serv_calc[[12]] <- function(calc, sess){
  observeEvent(calc$actAna1TDS,{
    a2=analysis(calc$ActTDS,type="Panel behaviour table", title="Panel behaviour table",sequenceStart=calc$a2sequenceStartTDS,sequenceDuration=calc$a2sequenceDurationTDS,nbDescriptors=calc$a2nbDescriptorsTDS,nbClicks=calc$a2nbClicksTDS,descriptorDuration=calc$a2descriptorDurationTDS)
    graphics.off()
    calc[["a2TDS"]] <- a2
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
}

serv_calc[[13]] <- function(calc, sess){
  observeEvent(calc$actAna2TDS,{
    b1=analysis(calc$ActTDS,type="Individual sequences", title="Individual sequences")
    graphics.off()
    calc[["b1TDS"]] <- b1
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
}

serv_calc[[14]] <- function(calc, sess){
  observeEvent(calc$actAna2TDS,{
    if (calc$clAna2st == TRUE){
      b23 = analysis(calc$ActTDS,type="Dominance curves", title="Dominance curves", alpha=calc$alTDSana2, repAsIndividual=calc$repAsAna2, rows="product", cols="rep", color="descriptor", smooth=TRUE, draw=c("significance","hasard","graymask"))
    }
    else {
      b23 = analysis(calc$ActTDS,type="Standardized dominance curves", title="Dominance curves", alpha=calc$alTDSana2, repAsIndividual=calc$repAsAna2, rows="product", cols="rep", color="descriptor", smooth=TRUE, draw=c("significance","hasard","graymask"))
    }
    graphics.off()
    calc[["b23TDS"]] <- b23
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
}

serv_calc[[15]] <- function(calc, sess){
  observeEvent(calc$actAna2TDS,{
    if (calc$clAna2st == TRUE){
      b89=analysis(calc$ActTDS,type="Panel sequences", title="Panel bandplot", alpha=calc$alTDSana2, repAsIndividual=calc$repAsAna2)
    }
    else {
      b89=analysis(calc$ActTDS,type="Standardized panel sequences", title="Panel bandplot", alpha=calc$alTDSana2, repAsIndividual=calc$repAsAna2)
    }
    graphics.off()
    calc[["b89TDS"]] <- b89
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
}

serv_calc[[16]] <- function(calc, sess){
  observeEvent(calc$actAna2TDS,{
    #b5=analysis(calc$ActTDS,type="Maximum dominance rates", title="Maximum dominance rates", alpha=calc$alTDSana2, draw=c("significance","hasard","graymask"))
    #graphics.off()
    #calc[["b5TDS"]] <- b5
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
}

serv_calc[[17]] <- function(calc, sess){
  observeEvent(calc$actAna1TDS,{
    a3=analysis(calc$ActTDS,type="ANOVA of behaviours", title="ANOVA of behaviours", columns=c("G_Mean","F_Product","P_Product","SE_Product","R2","P_ShapiroWilks","P_Levene_Product","Mean_Product","Group_Product"), alpha=calc$alTDSana1)
    graphics.off()
    calc[["a3TDS"]] <- a3
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
}

serv_calc[[18]] <- function(calc, sess){
  observeEvent(calc$c1actionchTDS,{
    c1=analysis(calc$ActTDS,type="Citation distribution", title="Citation distribution")
    graphics.off()
    calc[["c1TDS"]] <- c1
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
}

serv_calc[[19]] <- function(calc, sess){
  observeEvent(calc$chTDS,{
    updateTabsetPanel(sess, "carteTDS",
                      selected = calc$chTDS)
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
}

serv_calc[[23]] <- function(calc, sess){
  observeEvent(calc$actAna3TDS,{
    d56=analysis(calc$ActTDS,type="PCA of durations", title="PCA", fontSizeCex = 2)
    graphics.off()
    calc[["d56TDS"]] <- d56
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
}

serv_calc[[24]] <- function(calc, sess){
  observeEvent(calc$actAna3TDS,{
    d3=analysis(calc$ActTDS,type="ANOVA of durations", title="ANOVA of durations", columns=c("G_Mean","F_Product","P_Product","F_Product_Significance","SE_Product","R2","P_ShapiroWilks","P_Levene_Product","Mean_Product","Group_Product","P_Subject"), alpha=calc$alTDSana3)
    graphics.off()
    calc[["d3TDS"]] <- d3
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
}

serv_calc[[25]] <- function(calc, sess){
  observeEvent(calc$actAna3TDS,{
    d7=analysis(calc$ActTDS,type="CVA of durations", title="CVA",runBy="rep")
    graphics.off()
    calc[["d7TDS"]] <- d7
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
}

serv_calc[[26]] <- function(calc, sess){
  observeEvent(calc$actAna2TDS,{
    d9=analysis(calc$ActTDS,type="PCA of trajectories", title="PCA of trajectories", axes=list(c(1,2)), periods=7, fontSizeCex = 2)
    graphics.off()
    calc[["d9TDS"]] <- d9
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
}

serv_calc[[27]] <- function(calc, sess){
  observeEvent(calc$actionTDS,{
    calc[["nbSubTDS"]] <- nlevels(calc$ActTDS$behaviours$subject)
    calc[["nbProductTDS"]] <- nlevels(calc$ActTDS$behaviours$product)
    calc[["nbRepTDS"]] <- nlevels(calc$ActTDS$behaviours$rep)
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
}

serv_calc[[28]] <- function(calc, sess){
  observeEvent(calc$b6actionchTDS,{
    if (calc$clAnaDiffDomCurvSt == TRUE){
      b6=analysis(calc$ActTDS,type="Differences of dominance curves", title="Difference curves", alpha=calc$b6alpha, repAsIndividual=TRUE, rows="product", cols="rep", color="descriptor", smooth=TRUE)
    }
    else {
      b6=analysis(calc$ActTDS,type="Standardized differences of dominance curves", title="Difference curves",  alpha=calc$b6alpha, repAsIndividual=TRUE, rows="product", cols="rep", color="descriptor", smooth=TRUE)
    }
    graphics.off()
    calc[["b6TDS"]] <- b6
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
}

serv_calc[[20]] <- function(calc, sess){
  observeEvent(calc$c2actionchTDS,{
    c2=analysis(calc$ActTDS,type="Barplot of citations", title="Barplot of citations", confInt=calc$TDSconfintc2, errorBars="CI")
    graphics.off()
    calc[["c2TDS"]] <- c2
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
}

serv_calc[[21]] <- function(calc, sess){
  observeEvent(calc$d1actionchTDS,{
    d1=analysis(calc$ActTDS,type="Duration distribution", title="Duration distribution",plot=c("violin","boxplot","jitter"))
    graphics.off()
    calc[["d1TDS"]] <- d1
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
}

serv_calc[[22]] <- function(calc, sess){
  observeEvent(calc$d2actionchTDS,{
    d2=analysis(calc$ActTDS,type="Barplot of durations", title="Barplot of durations", confInt=calc$TDSconfintd2, errorBars="CI")
    graphics.off()
    calc[["d2TDS"]] <- d2
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
}

serv_calc[[29]] <- function(calc, sess){
  observeEvent(calc$dataTDS,{
    lectureTDS = read.csv(calc$dataTDS$datapath, sep = calc$separatorTDS)
    choix_TDS_table <- names(lectureTDS)
    updateSelectInput(sess, 'subjectTDS',
                      choices = choix_TDS_table
    )
    updateSelectInput(sess, 'productTDS',
                      choices = choix_TDS_table
    )
    updateSelectInput(sess, 'descriptorTDS',
                      choices = choix_TDS_table
    )
    updateSelectInput(sess, 'timeTDS',
                      choices = choix_TDS_table
    )
    updateSelectInput(sess, 'scoreTDS',
                      choices = choix_TDS_table
    )
    updateSelectInput(sess, 'replicateTDS',
                      choices = choix_TDS_table
    )
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
}

serv_calc[[30]] <- function(calc, sess){
  observeEvent(calc$d56actionchTDSrep,{
    d56rep=analysis(tds,type="PCA of durations", title="PCA", runBy="rep", fontSizeCex = 2)
    graphics.off()
    calc[["d56TDSRep"]] <- d56rep
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
}

serv_calc[[31]] <- function(calc, sess){
  observeEvent(calc$d34actionchTDSrep,{
    d34=analysis(tds,type="ANOVA of durations", title="ANOVA of durations by rep", runBy="rep", columns=c("G_Mean","F_Product","P_Product","F_Product_Significance","SE_Product","R2","P_ShapiroWilks","P_Levene_Product","Mean_Product","Group_Product","P_Subject"), alpha=calc$alphaAnovaTDSrep)
    graphics.off()
    calc[["d34TDS"]] <- d34
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
}

serv_calc[[32]] <- function(calc, sess){
  observeEvent(calc$actAna3TDS,{
    d7=analysis(calc$ActTDS,type="CVA of durations", title="CVA", fontSizeCex = 2)
    graphics.off()
    calc[["d7TDS"]] <- d7
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
}

serv_calc[[33]] <- function(calc, sess){
  observeEvent(calc$d7actionchTDSrep,{
    d7rep=analysis(tds,type="CVA of durations", title="CVA", runBy="rep", fontSizeCex = 2)
    graphics.off()
    calc[["d7TDSRep"]] <- d7rep
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
}

serv_calc[[34]] <- function(calc, sess){
  observeEvent(isTruthy(calc$sidebarItemExpanded == 'ListTDS'),{
    if (isTruthy(calc$dataTDS$datapath)){
      lectureTDS = read.csv(calc$dataTDS$datapath, sep = calc$separatorTDS)
      choix_TDS_table <- names(lectureTDS)
      updateSelectInput(sess, 'subjectTDS',
                        choices = choix_TDS_table
      )
      updateSelectInput(sess, 'productTDS',
                        choices = choix_TDS_table
      )
      updateSelectInput(sess, 'descriptorTDS',
                        choices = choix_TDS_table
      )
      updateSelectInput(sess, 'timeTDS',
                        choices = choix_TDS_table
      )
      updateSelectInput(sess, 'scoreTDS',
                        choices = choix_TDS_table
      )
      updateSelectInput(sess, 'replicateTDS',
                        choices = choix_TDS_table
      )
    }
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
}






################################################################################
# serv out
################################################################################

serv_out <- list()

################################################################################
# serv out FC
################################################################################

serv_out[["FCmenuBouton"]] <- function(calc, sess){
  renderUI({
    actionButton(inputId = 'actionFC', lexi['fcstart', calc$choiceL])
  })
}

serv_out[["FCmenuSep"]] <- function(calc, sess){
  renderUI({
    div(
      div(style="width:80%; display:inline-block; vertical-align: middle;",
          textInput('separatorFC', lexi['fcseparator', calc$choiceL], value = ';')
      ),
      div(style="display:inline-block; vertical-align: middle;",
          bsButton("bsseparatorFC", label = "", icon = icon("question"), style = "info", size = "extra-small", placement = 'left'),
          shinyBS::bsTooltip("bsseparatorFC", lexi['bsfcseparator', calc$choiceL], placement =  "right", options=list(container="body"))
      )
    )
  })
}

serv_out[["FCmenuCitation"]] <- function(calc, sess){
  renderUI({
    div(
      div(style="width:80%; display:inline-block; vertical-align: middle;",
          numericInput('citation', lexi['fccitation', calc$choiceL], value = 0, min = 1)
      ),
      div(style="display:inline-block; vertical-align: middle;",
          bsButton("bscitation", label = "", icon = icon("question"), style = "info", size = "extra-small", placement = 'left'),
          shinyBS::bsTooltip("bscitation", lexi['bsfccitation', calc$choiceL], placement =  "right", options=list(container="body"))
      )
    )
  })
}

serv_out[["FCmenuDescription"]] <- function(calc, sess){
  renderUI({
    div(
      div(style="width:80%; display:inline-block; vertical-align: middle;",
          selectInput('description',
                      lexi['fcdescription', calc$choiceL],
                      choices = "",
                      multiple = FALSE)
      ),
      div(style="display:inline-block; vertical-align: middle;",
          bsButton("bsdescription", label = "", icon = icon("question"), style = "info", size = "extra-small", placement = 'left'),
          shinyBS::bsTooltip("bsdescription", lexi['bsfcdescription', calc$choiceL], placement =  "right", options=list(container="body"))
      )
    )
  })
}

serv_out[["FCmenuProduit"]] <- function(calc, sess){
  renderUI({
    div(
      div(style="width:80%; display:inline-block; vertical-align: middle;",
          selectInput('product',
                      lexi['fcproduct', calc$choiceL],
                      choices = "",
                      multiple = FALSE)
          
      ),
      div(style="display:inline-block; vertical-align: middle;",
          bsButton("bsproduct", label = "", icon = icon("question"), style = "info", size = "extra-small", placement = 'left'),
          shinyBS::bsTooltip("bsproduct", lexi['bsfcproduct', calc$choiceL], placement =  "right", options=list(container="body"))
      )
    )
  })
}

serv_out[["FCmenuSujet"]] <- function(calc, sess){
  renderUI({
    div(
      div(style="width:80%; display:inline-block; vertical-align: middle;",
          selectInput('subject',
                      lexi['fcsubject', calc$choiceL],
                      choices = "",
                      multiple = FALSE)
      ),
      div(style="display:inline-block; vertical-align: middle;",
          bsButton("bssubject", label = "", icon = icon("question"), style = "info", size = "extra-small", placement = 'left'),
          shinyBS::bsTooltip("bssubject", lexi['bsfcsubject', calc$choiceL], placement =  "right", options=list(container="body"))
      )
    )
  })
}

serv_out[["FCmenuSpecific"]] <- function(calc, sess){
  renderUI({
    div(
      div(style="width:80%; display:inline-block; vertical-align: middle;",
          textInput('specificWordsTabNameFC', label = lexi['fcspecific', calc$choiceL], value = "")
      ),
      div(style="display:inline-block; vertical-align: middle;",
          bsButton("bsspecificWordsTabNameFC", label = "", icon = icon("question"), style = "info", size = "extra-small", placement = 'left'),
          shinyBS::bsTooltip("bsspecificWordsTabNameFC", lexi['bsfcspecific', calc$choiceL], placement =  "right", options=list(container="body"))
      )
    )
  })
}

serv_out[["FCmenuLexique"]] <- function(calc, sess){
  renderUI({
    div(
      div(style="width:80%; display:inline-block; vertical-align: middle;",
          fileInput('lexique', label = lexi['fclexic', calc$choiceL], buttonLabel = "Browse...", placeholder = "No file selected")
      ),
      div(style="display:inline-block; vertical-align: middle;",
          bsButton("bslexique", label = "", icon = icon("question"), style = "info", size = "extra-small", placement = 'left'),
          shinyBS::bsTooltip("bslexique", lexi['bsfclexic', calc$choiceL], placement =  "right", options=list(container="body"))
      )
    )
  })
}

serv_out[["FCmenuData"]] <- function(calc, sess){
  renderUI({
    div(
      div(style="width:80%; display:inline-block; vertical-align: middle;",
          fileInput('data', label = lexi['fcdata', calc$choiceL], buttonLabel = "Browse...", placeholder = "No file selected")
      ),
      div(style="display:inline-block; vertical-align: middle;",
          bsButton(inputId ="bsdata", label = "", icon = icon("question"), style = "info", size = "extra-small", placement = 'left'),
          shinyBS::bsTooltip(id = "bsdata", lexi['bsfcdata', calc$choiceL], placement =  "right", options=list(container="body"))
      )
    )
  })
}

serv_out[["addLexicon"]] <- function(calc, sess){
  renderUI({
    if (calc$actionFC){
      h5(lexi['fcaddLex1', calc$choiceL],textInput('wordLexicon', label = lexi['fcaddLex2', calc$choiceL], value = ""))
    }
  })
}

serv_out[["addLexicon2"]] <- function(calc, sess){
  renderUI({
    if (calc$actionFC){
      textInput('lemmaLexicon', label = lexi['fcaddLex3', calc$choiceL], value = "")
    }
  })
}

serv_out[["boutonLexiconFC"]] <- function(calc, sess){
  renderUI({
    if (calc$actionFC){
      div(
        div(style="width:80%; display:inline-block; vertical-align: middle;",
            actionButton(inputId = 'actionLexiconFC', lexi['fcaddLex4', calc$choiceL])
        ),
        div(style="display:inline-block; vertical-align: middle;",
            bsButton("bsactionLexiconFC", label = "", icon = icon("question"), style = "info", size = "extra-small", placement = 'left'),
            shinyBS::bsTooltip("bsactionLexiconFC", lexi['fcaddLexHelp', calc$choiceL], placement =  "right", options=list(container="body"))
        )
      )
    }
  })
}

serv_out[["boutonAnalysisFC"]] <- function(calc, sess){
  renderUI({
    if (calc$actionFC){
      actionButton(inputId = 'actionAnalysisFC', lexi['fcAn', calc$choiceL],style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
    }
  })
}

serv_out[["productchoice"]] <- function(calc, sess){
  renderUI({
    if (calc$actionFC){
      selectInput("prodchoice",
                  'lol',
                  choices = sort(unique(levels(calc$Act_table$contingencyTable[,2]))),
                  multiple = TRUE)
    }
  })
} # A REGARDER

serv_out[["alphaFC"]] <- function(calc, sess){
  renderUI({
    if (calc$actionFC){
      div(
        div(style="width:80%; display:inline-block; vertical-align: middle;",
            numericInput('alFC', label = lexi['fcalpha', calc$choiceL],value = 0.05, min = 0, max = 1, step = 0.01)
        ),
        div(style="display:inline-block; vertical-align: middle;",
            bsButton("bsalFC", label = "", icon = icon("question"), style = "info", size = "extra-small", placement = 'left'),
            shinyBS::bsTooltip("bsalFC", lexi['bsfcalpha', calc$choiceL], placement =  "right", options=list(container="body"))
        )
      )
    }
  })
}

serv_out[["nbootFC"]] <- function(calc, sess){
  renderUI({
    if (calc$actionFC){
      div(
        div(style="width:80%; display:inline-block; vertical-align: middle;",
            numericInput('nbFC', label = lexi['fcnboot', calc$choiceL],value = 100, min = 0)
        ),
        div(style="display:inline-block; vertical-align: middle;",
            bsButton("bsnbFC", label = "", icon = icon("question"), style = "info", size = "extra-small", placement = 'left'),
            shinyBS::bsTooltip("bsnbFC", lexi['bsfcnboot', calc$choiceL], placement =  "right", options=list(container="body"))
        )
      )
    }
  })
}

serv_out[["axesFC"]] <- function(calc, sess){
  renderUI({
    if (calc$actionFC){
      div(
        div(style="width:80%; display:inline-block; vertical-align: middle;",
            selectInput('axFC', label = lexi['fcchAxes', calc$choiceL], choices = c('signif',2:(nlevels(calc$Act_table$contingencyTable[,2])-1)))
        ),
        div(style="display:inline-block; vertical-align: middle;",
            bsButton("bsaxFC", label = "", icon = icon("question"), style = "info", size = "extra-small", placement = 'left'),
            shinyBS::bsTooltip("bsaxFC", lexi['bsfcchAxes', calc$choiceL], placement =  "right", options=list(container="body"))
        )
      )
    } 
  })
}

serv_out[["choiceFC"]] <- function(calc, sess){
  renderUI({
    if (calc$actionFC){
      div(
        div(style="width:80%; display:inline-block; vertical-align: middle;",
            selectInput('chFC', label = lexi['fcchoice', calc$choiceL], choices = c('percent.cont','original.cont','null.cont','p.values','derived.cont','percent.derived.cont'))
        ),
        div(style="display:inline-block; vertical-align: middle;",
            bsButton("bschFC", label = "", icon = icon("question"), style = "info", size = "extra-small", placement = 'left'),
            shinyBS::bsTooltip("bschFC", title = lexi['fcchoiceHelp', calc$choiceL], placement =  "right", options=list(container="body"))
        )
      )
    }
  })
}

serv_out[["tabContingence"]] <- function(calc, sess){
  renderDataTable({
    if (calc$actionFC & is.null(calc$lexique) == FALSE){
      calc$Act_table$contingencyTable
    }
  })
}

serv_out[["keptWords"]] <- function(calc, sess){
  renderDataTable({
    if (calc$actionFC & is.null(calc$lexique) == FALSE){
      data.frame(calc$Act_table$keptWords)
    }
  })
}

serv_out[["removedWords"]] <- function(calc, sess){
  renderDataTable({
    if (calc$actionFC & is.null(calc$lexique) == FALSE){
      data.frame(calc$Act_table$removedWords)
    }
  })
}

serv_out[["groupsOfWords"]] <- function(calc, sess){
  renderDataTable({
    if (calc$actionFC & is.null(calc$lexique) == FALSE){
      calc$Act_table$groupsOfWords
      convertDataFC = matrix(0,nrow = length(names(calc$Act_table$groupsOfWords)), ncol = 2)
      convertDataFC[,1] = names(calc$Act_table$groupsOfWords)
      convertDataFC[,2] = as.numeric(as.vector(calc$Act_table$groupsOfWords))
      colnames(convertDataFC) <- c('Words','Class')
      convertDataFC
    }
  })
}

serv_out[["dendogram"]] <- function(calc, sess){
  renderPlot({
    if (calc$actionFC & is.null(calc$lexique) == FALSE){
      calc$Act_table$dendogram
    }
  })
}

serv_out[["plotAnalyse"]] <- function(calc, sess){
  renderPlot({
    if (calc$actionFC){
      if (calc$actionAnalysisFC){
        calc$Act_tableFCAnalyse[[1]]$output$biplot
      }
    }
  })
}

serv_out[["plotAnalyse2"]] <- function(calc, sess){
  renderUI({
    if (calc$actionFC){
      if (calc$actionAnalysisFC){
        print(calc$Act_tableFCAnalyse[[1]]$output$testPerCell)
      }
    }
  })
}

serv_out[["downloadanaFC"]] <- function(calc, sess){
  downloadHandler(
    filename = function() {
      paste("analysis", ".png", sep = '')
    },
    content = function(file) {
      save_kable(calc$Act_tableFCAnalyse[[1]]$output$testPerCell, file = file, zoom = 1.5)
    }
  )
}

serv_out[["downloadanaFCsvg"]] <- function(calc, sess){
  downloadHandler(
    filename = function() {
      paste("analysis", ".svg", sep = '')
    },
    content = function(file) {
      save_kable(calc$Act_tableFCAnalyse[[1]]$output$testPerCell, file = file, zoom = 1.5)
    }
  )
}

serv_out[["downloadaxeFC"]] <- function(calc, sess){
  downloadHandler(
    filename = function() {
      paste("axes", ".png", sep = '')
    },
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = width, height = height,
                       res = 300, units = "in")
      }
      ggsave(file, plot = calc$Act_tableFCAnalyse[[1]]$output$biplot, device = device)
    }
  )
}

serv_out[["downloadaxeFCsvg"]] <- function(calc, sess){
  downloadHandler(
    filename = function() {
      paste("axes", ".svg", sep = '')
    },
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::svg(..., width = width, height = height)
      }
      ggsave(file, plot = calc$Act_tableFCAnalyse[[1]]$output$biplot, device = device)
    }
  )
}

serv_out[["downloadDendogramFC"]] <- function(calc, sess){
  downloadHandler(
    filename = function() {
      paste("dendogram", ".png", sep = '')
    },
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = width, height = height,
                       res = 300, units = "in")
      }
      ggsave(file, plot = calc$Act_table$dendogram, device = device)
    }
  )
}

serv_out[["downloadDendogramFCsvg"]] <- function(calc, sess){
  downloadHandler(
    filename = function() {
      paste("dendogram", ".svg", sep = '')
    },
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::svg(..., width = width, height = height)
      }
      ggsave(file, plot = calc$Act_table$dendogram, device = device)
    }
  )
}

serv_out[["downloadContingenceFCcsv"]] <- function(calc, sess){
  downloadHandler(
    filename = function() {
      paste("contingence", ".csv", sep = '')
    },
    content = function(file) {
      write.csv2(calc$Act_table$contingencyTable, file, row.names = FALSE)
    }
  )
}

serv_out[["downloadContingenceFCxlsx"]] <- function(calc, sess){
  downloadHandler(
    filename = function() {
      paste("contingence", ".xlsx", sep = '')
    },
    content = function(file) {
      write.xlsx(calc$Act_table$contingencyTable, file, row.names = FALSE)
    }
  )
}

serv_out[["downloadKeptwordsFCcsv"]] <- function(calc, sess){
  downloadHandler(
    filename = function() {
      paste("keptwords", ".csv", sep = '')
    },
    content = function(file) {
      write.csv2(data.frame(calc$Act_table$keptWords), file, row.names = FALSE)
    }
  )
}

serv_out[["downloadKeptwordsFCxlsx"]] <- function(calc, sess){
  downloadHandler(
    filename = function() {
      paste("keptwords", ".xlsx", sep = '')
    },
    content = function(file) {
      write.xlsx(data.frame(calc$Act_table$keptWords), file, row.names = FALSE)
    }
  )
}

serv_out[["downloadRemovedwordsFCcsv"]] <- function(calc, sess){
  downloadHandler(
    filename = function() {
      paste("removedwords", ".csv", sep = '')
    },
    content = function(file) {
      write.csv2(data.frame(calc$Act_table$removedWords), file, row.names = FALSE)
    }
  )
}

serv_out[["downloadRemovedwordsFCxlsx"]] <- function(calc, sess){
  downloadHandler(
    filename = function() {
      paste("removedwords", ".xlsx", sep = '')
    },
    content = function(file) {
      write.xlsx(data.frame(calc$Act_table$removedWords), file, row.names = FALSE)
    }
  )
}

serv_out[["downloadGroupsFCcsv"]] <- function(calc, sess){
  downloadHandler(
    filename = function() {
      paste("groupofwords", ".csv", sep = '')
    },
    content = function(file) {
      convertDataFC = matrix(0,nrow = length(names(calc$Act_table$groupsOfWords)), ncol = 2)
      convertDataFC[,1] = names(calc$Act_table$groupsOfWords)
      convertDataFC[,2] = as.numeric(as.vector(calc$Act_table$groupsOfWords))
      colnames(convertDataFC) <- c('Words','Class')
      write.csv2(convertDataFC, file, row.names = FALSE)
    }
  )
}

serv_out[["downloadGroupsFCxlsx"]] <- function(calc, sess){
  downloadHandler(
    filename = function() {
      paste("groupofwords", ".xlsx", sep = '')
    },
    content = function(file) {
      convertDataFC = matrix(0,nrow = length(names(calc$Act_table$groupsOfWords)), ncol = 2)
      convertDataFC[,1] = names(calc$Act_table$groupsOfWords)
      convertDataFC[,2] = as.numeric(as.vector(calc$Act_table$groupsOfWords))
      colnames(convertDataFC) <- c('Words','Class')
      write.xlsx(data.frame(convertDataFC), file, row.names = FALSE)
    }
  )
}

serv_out[["TabLexiconFC"]] <- function(calc, sess){
  renderDataTable({
    if (calc$actionFC & is.null(calc$lexique) == FALSE){
      calc$lexFC
    }
  })
}

serv_out[["TabLexiconFCcsv"]] <- function(calc, sess){
  downloadHandler(
    filename = function() {
      paste("lexicon", ".csv", sep = '')
    },
    content = function(file) {
      write.csv2(calc$lexFC, file, row.names = FALSE)
    }
  )
}

serv_out[["TabLexiconFCxlsx"]] <- function(calc, sess){
  downloadHandler(
    filename = function() {
      paste("lexicon", ".xlsx", sep = '')
    },
    content = function(file) {
      write.xlsx(calc$lexFC, file, row.names = FALSE)
    }
  )
}

serv_out[["productFC"]] <- function(calc, sess){
  renderDataTable({
    if (calc$actionFC & is.null(calc$lexique) == FALSE){
      data.frame(levels(calc$Act_table$contingencyTable[,2]))
    }
  })
}

serv_out[["nbSubFC"]] <- function(calc, sess){
  renderText({
    if (calc$actionFC & is.null(calc$lexique) == FALSE){
      as.character(calc$nbSub)
    }
  })
}

serv_out[["nbProdFC"]] <- function(calc, sess){
  renderText({
    if (calc$actionFC & is.null(calc$lexique) == FALSE){
      as.character(calc$nbProd)
    }
  })
}

serv_out[["bodyFC"]] <- function(calc, sess){
  renderUI({
    if (calc$actionFC){
      navbarPage("", id = "tabselected",
                 tabPanel(lexi['fctabResume', calc$choiceL], value = 2,
                          tabsetPanel(
                            tabPanel(lexi['fctabContingence', calc$choiceL],
                                     dataTableOutput('tabContingence'),
                                     downloadButton("downloadContingenceFCcsv", "csv"),
                                     downloadButton("downloadContingenceFCxlsx", "xlsx")),
                            tabPanel(lexi['fctabKeptWord', calc$choiceL],
                                     dataTableOutput('keptWords'),
                                     downloadButton("downloadKeptwordsFCcsv", "csv"),
                                     downloadButton("downloadKeptwordsFCxlsx", "xlsx")),
                            tabPanel(lexi['fctabRemovedWord', calc$choiceL],
                                     dataTableOutput('removedWords'),
                                     downloadButton("downloadRemovedwordsFCcsv", "csv"),
                                     downloadButton("downloadRemovedwordsFCxlsx", "xlsx")),
                            tabPanel(lexi['fctabGroup', calc$choiceL],
                                     dataTableOutput('groupsOfWords'),
                                     downloadButton("downloadGroupsFCcsv", "csv"),
                                     downloadButton("downloadGroupsFCxlsx", "xlsx")),
                            tabPanel(lexi['fctabDendogram', calc$choiceL],
                                     plotOutput('dendogram', height = "800px"),
                                     downloadButton("downloadDendogramFC", "png"),
                                     downloadButton("downloadDendogramFCsvg", "svg")),
                            tabPanel(lexi['fctabLexicon', calc$choiceL],
                                     dataTableOutput('TabLexiconFC'),
                                     downloadButton("TabLexiconFCcsv", "csv"),
                                     downloadButton("TabLexiconFCxlsx", "xlsx"))
                          )
                 ),
                 tabPanel(lexi['fctabAnalysis', calc$choiceL], value = 1,
                          tabsetPanel(
                            tabPanel(lexi['fctabAnalysis1', calc$choiceL],
                                     htmlOutput('plotAnalyse2'),
                                     downloadButton("downloadanaFC", "png"),
                                     downloadButton("downloadanaFCsvg", "svg")),
                            tabPanel(lexi['fctabAnalysis2', calc$choiceL],
                                     plotOutput('plotAnalyse', height = "800px"),
                                     downloadButton("downloadaxeFC", "png"),
                                     downloadButton("downloadaxeFCsvg", "svg"))
                            
                          )
                 ),
                 tabPanel(lexi['fctabUnit', calc$choiceL], value=3,
                          tabsetPanel(
                            tabPanel(lexi['fctabUnit1', calc$choiceL],
                                     fluidRow(
                                       valueBox(value = textOutput('nbSubFC'),
                                                subtitle = 'nb subject'
                                                , icon = NULL, color = "aqua", width = 3,
                                                href = NULL),
                                       valueBox(value = textOutput('nbProdFC'),
                                                subtitle = 'nb product'
                                                , icon = NULL, color = "aqua", width = 3,
                                                href = NULL)
                                     ),
                                     fluidRow(
                                       box(title = "product", width = 6, solidHeader = TRUE, dataTableOutput('productFC'))
                                     ))
                          )
                 )
      )
    }
  })
  
}

################################################################################
# serv out TDS
################################################################################

serv_out[["TDSmenuData"]] <- function(calc, sess){
  renderUI({
    div(
      div(style="width:80%; display:inline-block; vertical-align: middle;",
          fileInput('dataTDS', label = lexi['tdsData', calc$choiceL], buttonLabel = lexi['TDSmenuBrowse', calc$choiceL], placeholder = lexi['TDSmenuFileSelected', calc$choiceL])
      ),
      div(style="display:inline-block; vertical-align: middle;",
          bsButton("bsdataTDS", label = "", icon = icon("question"), style = "info", size = "extra-small", placement = 'left'),
          shinyBS::bsTooltip("bsdataTDS", lexi['bstdsData', calc$choiceL])
      )
    )
  })
}

serv_out[["TDSmenuSeparator"]] <- function(calc, sess){
  renderUI({
    div(
      div(style="width:80%; display:inline-block; vertical-align: middle;",
          textInput('separatorTDS', label = lexi['tdsSeparator', calc$choiceL], value = ';')
      ),
      div(style="display:inline-block; vertical-align: middle;",
          bsButton("bsseparatorTDS", label = "", icon = icon("question"), style = "info", size = "extra-small", placement = 'left'),
          shinyBS::bsTooltip("bsseparatorTDS", lexi['bstdsSeparator', calc$choiceL])
      )
    )
  })
}

serv_out[["TDSmenuStart"]] <- function(calc, sess){
  renderUI({
    div(
      div(style="width:80%; display:inline-block; vertical-align: middle;",
          checkboxInput('startWithFirstCitationTDS', label = lexi['tdsFirst', calc$choiceL], value = FALSE)
      ),
      div(style="display:inline-block; vertical-align: middle;",
          bsButton("bsstartWithFirstCitationTDS", label = "", icon = icon("question"), style = "info", size = "extra-small", placement = 'left'),
          shinyBS::bsTooltip("bsstartWithFirstCitationTDS", lexi['bstdsFirst', calc$choiceL])
      )
    )
  })
}

serv_out[["TDSmenuDiscretization"]] <- function(calc, sess){
  renderUI({
    div(
      div(style="width:80%; display:inline-block; vertical-align: middle;",
          numericInput('discretizationTDS', label = lexi['tdsDiscretization', calc$choiceL],value = 0.2, min = 0, max = 1, step = 0.1)
      ),
      div(style="display:inline-block; vertical-align: middle;",
          bsButton("bsdiscretizationTDS", label = "", icon = icon("question"), style = "info", size = "extra-small", placement = 'left'),
          shinyBS::bsTooltip("bsdiscretizationTDS", lexi['bstdsDiscretization', calc$choiceL])
      )
    )
  })
}

serv_out[["repAsAna2"]] <- function(calc, sess){
  renderUI({
    div(
      div(style="width:80%; display:inline-block; vertical-align: middle;",
          checkboxInput('clrepAsAna2', label = lexi['ana2repAs', calc$choiceL], value = FALSE)
      ),
      div(style="display:inline-block; vertical-align: middle;",
          bsButton("bsclrepAsAna2", label = "", icon = icon("question"), style = "info", size = "extra-small", placement = 'left'),
          shinyBS::bsTooltip("bsclrepAsAna2", lexi['bsana2repAs', calc$choiceL])
      )
    )
  })
}

serv_out[["Ana2st"]] <- function(calc, sess){
  renderUI({
    div(
      div(style="width:80%; display:inline-block; vertical-align: middle;",
          checkboxInput('clAna2st', label = lexi['ana2st', calc$choiceL], value = FALSE)
      ),
      div(style="display:inline-block; vertical-align: middle;",
          bsButton("bsclAna2st", label = "", icon = icon("question"), style = "info", size = "extra-small", placement = 'left'),
          shinyBS::bsTooltip("bsclAna2st", lexi['bsana2st', calc$choiceL])
      )
    )
  })
}

serv_out[["menuListTDS"]] <- function(calc, sess){
  renderUI({
    if (calc$actionTDS == FALSE | isTruthy(calc$ActTDS) == FALSE){
      lectureTDS = read.csv(calc$dataTDS$datapath, sep = calc$separatorTDS)
      if ((isTruthy(names(lectureTDS) == "SubjectCode") & isTruthy(names(lectureTDS) == "ProductCode") & isTruthy(names(lectureTDS) == "AttributeCode") & isTruthy(names(lectureTDS) == "Time") & isTruthy(names(lectureTDS) == "Score") & isTruthy(names(lectureTDS) == "Replicate")) == FALSE){
      sidebarMenu(id = 'ListMenuTDS',
        menuItem(text = lexi['ListeDeroulanteTDS', calc$choiceL], expandedName = 'ListTDS', startExpanded = TRUE,
                 div(
                   div(style="width:80%; display:inline-block; vertical-align: middle;",
                       selectInput('subjectTDS',
                                   lexi['tdsSubject', calc$choiceL],
                                   choices = "",
                                   multiple = FALSE)
                   ),
                   div(style="display:inline-block; vertical-align: middle;",
                       bsButton("bssubjectTDS", label = "", icon = icon("question"), style = "info", size = "extra-small", placement = 'left'),
                       shinyBS::bsTooltip("bssubjectTDS", lexi['bssubjectTDS', calc$choiceL])
                   )
                 ),
                 div(
                   div(style="width:80%; display:inline-block; vertical-align: middle;",
                       selectInput('productTDS',
                                   lexi['tdsProduct', calc$choiceL],
                                   choices = "",
                                   multiple = FALSE)
                   ),
                   div(style="display:inline-block; vertical-align: middle;",
                       bsButton("bsproductTDS", label = "", icon = icon("question"), style = "info", size = "extra-small", placement = 'left'),
                       shinyBS::bsTooltip("bsproductTDS", lexi['bsproductTDS', calc$choiceL])
                   )
                 ),
                 div(
                   div(style="width:80%; display:inline-block; vertical-align: middle;",
                       selectInput('descriptorTDS',
                                   lexi['tdsDescriptor', calc$choiceL],
                                   choices = "",
                                   multiple = FALSE)
                   ),
                   div(style="display:inline-block; vertical-align: middle;",
                       bsButton("bsdescriptorTDS", label = "", icon = icon("question"), style = "info", size = "extra-small", placement = 'left'),
                       shinyBS::bsTooltip("bsproductTDS", lexi['bsdescriptorTDS', calc$choiceL])
                   )
                 ),
                 div(
                   div(style="width:80%; display:inline-block; vertical-align: middle;",
                       selectInput('timeTDS',
                                   lexi['tdsTime', calc$choiceL],
                                   choices = "",
                                   multiple = FALSE)
                   ),
                   div(style="display:inline-block; vertical-align: middle;",
                       bsButton("bstimeTDS", label = "", icon = icon("question"), style = "info", size = "extra-small", placement = 'left'),
                       shinyBS::bsTooltip("bstimeTDS", lexi['bstimeTDS', calc$choiceL])
                   )
                 ),
                 div(
                   div(style="width:80%; display:inline-block; vertical-align: middle;",
                       selectInput('scoreTDS',
                                   lexi['tdsScore', calc$choiceL],
                                   choices = "",
                                   multiple = FALSE)
                   ),
                   div(style="display:inline-block; vertical-align: middle;",
                       bsButton("bsscoreTDS", label = "", icon = icon("question"), style = "info", size = "extra-small", placement = 'left'),
                       shinyBS::bsTooltip("bsscoreTDS", lexi['bsscoreTDS', calc$choiceL])
                   )
                 ),
                 div(
                   div(style="width:80%; display:inline-block; vertical-align: middle;",
                       selectInput('replicateTDS',
                                   lexi['tdsReplicate', calc$choiceL],
                                   choices = "",
                                   multiple = FALSE) 
                   ),
                   div(style="display:inline-block; vertical-align: middle;",
                       bsButton("bsreplicateTDS", label = "", icon = icon("question"), style = "info", size = "extra-small", placement = 'left'),
                       shinyBS::bsTooltip("bsreplicateTDS", lexi['bsreplicateTDS', calc$choiceL])
                   )
                 )
      ))
      }
    }
  })
}

serv_out[["TDSmenuBouton"]] <- function(calc, sess){
  renderUI({
    actionButton(inputId = 'actionTDS', lexi['tdsStart', calc$choiceL])
  })
}

serv_out[["alphaAnaTDS"]] <- function(calc, sess){
  renderUI({
    if (calc$actionTDS){
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
}

serv_out[["alphaAna1TDS"]] <- function(calc, sess){
  renderUI({
    if (calc$actionTDS){
      div(
        div(style="width:80%; display:inline-block; vertical-align: middle;",
            numericInput('alTDSana1', label = lexi['TDSana1alpha', calc$choiceL],value = 0.05, min = 0, max = 1, step = 0.01)
        ),
        div(style="display:inline-block; vertical-align: middle;",
            bsButton("bsalTDSana1", label = "", icon = icon("question"), style = "info", size = "extra-small", placement = 'left'),
            shinyBS::bsTooltip("bsalTDSana1", lexi['bsTDSana1alpha', calc$choiceL])
        )
      )
    }
  })
}

serv_out[["alphaAna3TDS"]] <- function(calc, sess){
  renderUI({
    if (calc$actionTDS){
      div(
        div(style="width:80%; display:inline-block; vertical-align: middle;",
            numericInput('alTDSana3', label = lexi['TDSana3alpha', calc$choiceL],value = 0.05, min = 0, max = 1, step = 0.01)
        ),
        div(style="display:inline-block; vertical-align: middle;",
            bsButton("bsalTDSana3", label = "", icon = icon("question"), style = "info", size = "extra-small", placement = 'left'),
            shinyBS::bsTooltip("bsalTDSana3", lexi['bsTDSana3alpha', calc$choiceL])
        )
      )
    }
  })
}

serv_out[["actionAna1TDS"]] <- function(calc, sess){
  renderUI({
    if (calc$actionTDS){
      actionButton(inputId = 'actAna1TDS', 'start', style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
    }
  })
}

serv_out[["panelTablesequenceStartTDS"]] <- function(calc, sess){
  renderUI({
    if (calc$actionTDS){
      div(
        div(style="width:80%; display:inline-block; vertical-align: middle;",
            numericInput('a2sequenceStartTDS', label = lexi['a2sequenceStartTDS', calc$choiceL],value = 15, min = 0, max = NA, step = 1)
        ),
        div(style="display:inline-block; vertical-align: middle;",
            bsButton("bsa2sequenceStartTDS", label = "", icon = icon("question"), style = "info", size = "extra-small", placement = 'left'),
            shinyBS::bsTooltip("bsa2sequenceStartTDS", lexi['bsa2sequenceStartTDS', calc$choiceL])
        )
      )
    }
  })
}

serv_out[["panelTablesequenceDurationTDS"]] <- function(calc, sess){
  renderUI({
    if (calc$actionTDS){
      div(
        div(style="width:80%; display:inline-block; vertical-align: middle;",
            numericInput('a2sequenceDurationTDS', label = lexi['a2sequenceDurationTDS', calc$choiceL],value = 6, min = 0, max = NA, step = 1)
        ),
        div(style="display:inline-block; vertical-align: middle;",
            bsButton("bsa2sequenceDurationTDS", label = "", icon = icon("question"), style = "info", size = "extra-small", placement = 'left'),
            shinyBS::bsTooltip("bsa2sequenceDurationTDS", lexi['bsa2sequenceDurationTDS', calc$choiceL])
        )
      )
    }
  })
}

serv_out[["panelTablenbDescriptorsTDS"]] <- function(calc, sess){
  renderUI({
    if (calc$actionTDS){
      div(
        div(style="width:80%; display:inline-block; vertical-align: middle;",
            numericInput('a2nbDescriptorsTDS', label = lexi['a2nbDescriptorsTDS', calc$choiceL],value = 1, min = 0, max = NA, step = 1)
        ),
        div(style="display:inline-block; vertical-align: middle;",
            bsButton("bsa2nbDescriptorsTDS", label = "", icon = icon("question"), style = "info", size = "extra-small", placement = 'left'),
            shinyBS::bsTooltip("bsa2nbDescriptorsTDS", lexi['bsa2nbDescriptorsTDS', calc$choiceL])
        )
      )
    }
  })
}

serv_out[["panelTablenbClicksTDS"]] <- function(calc, sess){
  renderUI({
    if (calc$actionTDS){
      div(
        div(style="width:80%; display:inline-block; vertical-align: middle;",
            numericInput('a2nbClicksTDS', label = lexi['a2nbClicksTDS', calc$choiceL],value = 1.5, min = 0, max = NA, step = 0.1)
        ),
        div(style="display:inline-block; vertical-align: middle;",
            bsButton("bsa2nbClicksTDS", label = "", icon = icon("question"), style = "info", size = "extra-small", placement = 'left'),
            shinyBS::bsTooltip("bsa2nbClicksTDS", lexi['bsa2nbClicksTDS', calc$choiceL])
        )
      )
    }
  })
}

serv_out[["panelTabledescriptorDurationTDS"]] <- function(calc, sess){
  renderUI({
    if (calc$actionTDS){
      div(
        div(style="width:80%; display:inline-block; vertical-align: middle;",
            numericInput('a2descriptorDurationTDS', label = lexi['a2descriptorDurationTDS', calc$choiceL],value = 2, min = 0, max = NA, step = 1)
        ),
        div(style="display:inline-block; vertical-align: middle;",
            bsButton("bsa2descriptorDurationTDS", label = "", icon = icon("question"), style = "info", size = "extra-small", placement = 'left'),
            shinyBS::bsTooltip("bsa2descriptorDurationTDS", lexi['bsa2descriptorDurationTDS', calc$choiceL])
        )
      )
    }
  })
}

serv_out[["actionAna2TDS"]] <- function(calc, sess){
  renderUI({
    if (calc$actionTDS){
      actionButton(inputId = 'actAna2TDS', 'start', style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
    }
  })
}

serv_out[["actionAna3TDS"]] <- function(calc, sess){
  renderUI({
    if (calc$actionTDS){
      actionButton(inputId = 'actAna3TDS', 'start', style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
    }
  })
}

serv_out[["dfTDS"]] <- function(calc, sess){
  renderDataTable({
    if (calc$actionTDS){
      calc$ActTDS$df
    }
  })
}

serv_out[["dominancesTDS"]] <- function(calc, sess){
  renderDataTable({
    if (calc$actionTDS){
      calc$ActTDS$dominances
    }
  })
}

serv_out[["stdDominancesTDS"]] <- function(calc, sess){
  renderDataTable({
    if (calc$actionTDS){
      calc$ActTDS$stdDominances
    }
  })
}

serv_out[["durationsTDS"]] <- function(calc, sess){
  renderDataTable({
    if (calc$actionTDS){
      calc$ActTDS$durations
    }
  })
}

serv_out[["citationsTDS"]] <- function(calc, sess){
  renderDataTable({
    if (calc$actionTDS){
      calc$ActTDS$citations
    }
  })
}

serv_out[["behavioursTDS"]] <- function(calc, sess){
  renderDataTable({
    if (calc$actionTDS){
      calc$ActTDS$behaviours
    }
  })
}

serv_out[["BehaviourDistTDS"]] <- function(calc, sess){
  renderPlot({
    if (calc$a1actionchTDS){
      calc$a1TDS[[1]]$output$panelBehaviourDistribution
    }
  })
}

serv_out[["ClustDurationTDS"]] <- function(calc, sess){
  renderPlot({
    if (calc$a4actionchTDS){
      calc$a4TDS[[1]]$output$durationsClusters
    }
  })
}

serv_out[["DiffDomCurvTDS"]] <- function(calc, sess){
  renderPlot({
    if (calc$b6actionchTDS){
      calc$b6TDS[[1]]$output$differenceCurves
    }
  })
}

serv_out[["BarCitTDS"]] <- function(calc, sess){
  renderPlot({
    if (calc$c2actionchTDS){
      calc$c2TDS[[1]]$output$barplot
    }
  })
}

serv_out[["DurDistTDS"]] <- function(calc, sess){
  renderPlot({
    if (calc$d1actionchTDS){
      calc$d1TDS[[1]]$output$durationsDistribution
    }
  })
}

serv_out[["BarDurTDS"]] <- function(calc, sess){
  renderPlot({
    if (calc$d2actionchTDS){
      calc$d2TDS[[1]]$output$barplot
    }
  })
}

serv_out[["BehaviourPanelTDS"]] <- function(calc, sess){
  renderUI({
    if (calc$actAna1TDS){
      print(calc$a2TDS[[1]]$output$panelBehaviour)
    }
  })
}

serv_out[["downloadpanelBehaviourTDS"]] <- function(calc, sess){
  downloadHandler(
    filename = function() {
      paste("panelBehaviour", ".html", sep = '')
    },
    content = function(file) {
      save_html(print(calc$a2TDS[[1]]$output$panelBehaviour), file = file)
    }
  )
}

serv_out[["downloadpanelBehaviourTDScsv"]] <- function(calc, sess){
  downloadHandler(
    filename = function() {
      paste("panelBehaviour", ".csv", sep = '')
    },
    content = function(file) {
      write.csv2(calc$a2TDS[[1]]$df, file, row.names = FALSE)
    }
  )
}

serv_out[["downloadpanelBehaviourAnovaTDS"]] <- function(calc, sess){
  downloadHandler(
    filename = function() {
      paste("BehaviourAnova", ".html", sep = '')
    },
    content = function(file) {
      save_html(print(calc$a3TDS[[1]]$output$anova), file = file)
    }
  )
}

serv_out[["downloadpanelBehaviourAnovaTDScsv"]] <- function(calc, sess){
  downloadHandler(
    filename = function() {
      paste("BehaviourAnova", ".csv", sep = '')
    },
    content = function(file) {
      write.csv2(calc$a3TDS[[1]]$output$result, file, row.names = FALSE)
    }
  )
}

serv_out[["TDStriTable"]] <- function(calc, sess){
  renderUI({
    print(calc$a2TDStri[[1]]$output$panelBehaviour)
  })
}

serv_out[["BehaviourAnovaTDS"]] <- function(calc, sess){
  renderUI({
    print(calc$a3TDS[[1]]$output$anova)
  })
}

serv_out[["AnovaDurationsTDS"]] <- function(calc, sess){
  renderUI({
    print(calc$d3TDS[[1]]$output$anova)
  })
}

serv_out[["AnovaDurationsTDShtml"]] <- function(calc, sess){
  downloadHandler(
    filename = function() {
      paste("AnovaDurations", ".html", sep = '')
    },
    content = function(file) {
      save_html(print(calc$d3TDS[[1]]$output$anova), file = file)
    }
  )
}

serv_out[["AnovaDurationsTDScsv"]] <- function(calc, sess){
  downloadHandler(
    filename = function() {
      paste("AnovaDurations", ".csv", sep = '')
    },
    content = function(file) {
      write.csv2(calc$d3TDS[[1]]$output$result, file, row.names = FALSE)
    }
  )
}

serv_out[["AnovaDurationsTDSrep"]] <- function(calc, sess){
  renderUI({
    print(calc$d34TDS[[as.numeric(calc$d34TDSrepChoice)]]$output$anova)
  })
}

serv_out[["AnovaDurationsTDShtmlrep"]] <- function(calc, sess){
  downloadHandler(
    filename = function() {
      paste("AnovaDurations", ".html", sep = '')
    },
    content = function(file) {
      save_html(print(calc$d34TDS[[as.numeric(calc$d34TDSrepChoice)]]$output$anova), file = file)
    }
  )
}

serv_out[["AnovaDurationsTDScsvrep"]] <- function(calc, sess){
  downloadHandler(
    filename = function() {
      paste("AnovaDurations", ".csv", sep = '')
    },
    content = function(file) {
      write.csv2(calc$d34TDS[[as.numeric(calc$d34TDSrepChoice)]]$output$result, file, row.names = FALSE)
    }
  )
}

serv_out[["indSeqTDS"]] <- function(calc, sess){
  renderPlot({
    if (calc$actAna2TDS){
      calc$b1TDS[[1]]$output$sequences
    }
  })
}

serv_out[["domCurTDS"]] <- function(calc, sess){
  renderPlot({
    if (calc$actAna2TDS){
      calc$b23TDS[[1]]$output$dominanceCurves
    }
  })
}

serv_out[["PanelSequencesTDS"]] <- function(calc, sess){
  renderPlot({
    if (calc$actAna2TDS){
      calc$b89TDS[[1]]$output$panelSequences
    }
  })
}

#serv_out[["PCATDS"]] <- function(calc, sess){
#    renderUI({
#        plot_output_list <- lapply(1:length(calc$d56TDS), function(i) {
#            plotname <- paste("plotPCA", i, sep="")
#            plotOutput(plotname, height = "800px")
#        })
#        do.call(tagList, plot_output_list)
#    })
#}

serv_out[["PCATDS"]] <- function(calc, sess){
  renderPlot({
    calc$d56TDS[[1]]$output$biplot
  })
}

serv_out[["CVATDS"]] <- function(calc, sess){
  renderPlot({
    calc$d7TDS[[1]]$output$biplot
  })
}

serv_out[["PCATDSpng"]] <- function(calc, sess){
  downloadHandler(
    filename = function() {
      paste("PCA", ".png", sep = '')
    },
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = width, height = height,
                       res = 300, units = "in")
      }
      ggsave(file, plot = calc$d56TDS[[1]]$output$biplot, device = device)
    }
  )
}

serv_out[["PCATDSsvg"]] <- function(calc, sess){
  downloadHandler(
    filename = function() {
      paste("PCA", ".svg", sep = '')
    },
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::svg(..., width = width, height = height)
      }
      ggsave(file, plot = calc$d56TDS[[1]]$output$biplot, device = device)
    }
  )
}

serv_out[["CVATDSpng"]] <- function(calc, sess){
  downloadHandler(
    filename = function() {
      paste("CVA", ".png", sep = '')
    },
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = width, height = height,
                       res = 300, units = "in")
      }
      ggsave(file, plot = calc$d7TDS[[1]]$output$biplot, device = device)
    }
  )
}

serv_out[["CVATDSsvg"]] <- function(calc, sess){
  downloadHandler(
    filename = function() {
      paste("CVA", ".svg", sep = '')
    },
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::svg(..., width = width, height = height)
      }
      ggsave(file, plot = calc$d7TDS[[1]]$output$biplot, device = device)
    }
  )
}

serv_out[["PCATDSRep"]] <- function(calc, sess){
  renderPlot({
    calc$d56TDSRep[[as.numeric(calc$d56TDSrepChoice)]]$output$biplot
  })
}

serv_out[["CVATDSRep"]] <- function(calc, sess){
  renderPlot({
    calc$d7TDSRep[[as.numeric(calc$d7TDSrepChoice)]]$output$biplot
  })
}

serv_out[["PCATDSpngRep"]] <- function(calc, sess){
  downloadHandler(
    filename = function() {
      paste("PCA", ".png", sep = '')
    },
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = width, height = height,
                       res = 300, units = "in")
      }
      ggsave(file, plot = calc$d56TDSRep[[as.numeric(calc$d56TDSrepChoice)]]$output$biplot, device = device)
    }
  )
}

serv_out[["CVATDSpngRep"]] <- function(calc, sess){
  downloadHandler(
    filename = function() {
      paste("PCA", ".png", sep = '')
    },
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = width, height = height,
                       res = 300, units = "in")
      }
      ggsave(file, plot = calc$d7TDSRep[[as.numeric(calc$d7TDSrepChoice)]]$output$biplot, device = device)
    }
  )
}

serv_out[["PCATDSsvgRep"]] <- function(calc, sess){
  downloadHandler(
    filename = function() {
      paste("PCA", ".svg", sep = '')
    },
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::svg(..., width = width, height = height)
      }
      ggsave(file, plot = calc$d56TDSRep[[as.numeric(calc$d56TDSrepChoice)]]$output$biplot, device = device)
    }
  )
}

serv_out[["CVATDSsvgRep"]] <- function(calc, sess){
  downloadHandler(
    filename = function() {
      paste("PCA", ".svg", sep = '')
    },
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::svg(..., width = width, height = height)
      }
      ggsave(file, plot = calc$d56TDSRep[[as.numeric(calc$d7TDSrepChoice)]]$output$biplot, device = device)
    }
  )
}

#serv_out[["PCAtrajectoriesTDS"]] <- function(calc, sess){
#    renderImage({
#        outfile <- tempfile(fileext = '.png')
#        png(outfile, 
#            width = 400*8, 
#            height = 400*8,
#            res = 72*8)
#        print(calc$d9TDS[[1]]$output$biplot)
#        dev.off()
#        list(src = outfile,
#             contentType = 'image/png',
#             width = 800,
#             height = 800,
#             alt = "This is alternate text")
#        
#    }, deleteFile = TRUE)
#}

serv_out[["PCAtrajectoriesTDS"]] <- function(calc, sess){
  renderPlot({
    calc$d9TDS[[1]]$output$biplot
  })
}

serv_out[["maxDomTDS"]] <- function(calc, sess){
  renderPlot({
    if (calc$actAna2TDS){
      calc$b5TDS[[1]]$output$dominances
    }
  })
}

serv_out[["choiceTDS"]] <- function(calc, sess){
  renderUI({
    div(
      div(style="width:80%; display:inline-block; vertical-align: middle;",
          selectInput('chTDS', label = 'choice :', choices = c('Citation distribution','Panel behaviour distribution','Clusters of subjects durations','Differences of dominance curves','Barplot of citations','Duration distribution','Barplot of durations', 'PCA of durations by rep', 'CVA of durations by rep', 'ANOVA of durations by rep'))
      ),
      div(style="display:inline-block; vertical-align: middle;",
          bsButton("bschTDS", label = "", icon = icon("question"), style = "info", size = "extra-small", placement = 'left'),
          shinyBS::bsTooltip("bschTDS", lexi['bsChoiceTDS', calc$choiceL])
      )
    )
    
  })
}

serv_out[["c1boutonchTDS"]] <- function(calc, sess){
  renderUI({
    if (calc$actionTDS & (calc$chTDS == 'Citation distribution')){
      actionButton(inputId = 'c1actionchTDS', 'start', style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
    }
  })
}

serv_out[["a1boutonchTDS"]] <- function(calc, sess){
  renderUI({
    if (calc$actionTDS & (calc$chTDS == 'Panel behaviour distribution')){
      actionButton(inputId = 'a1actionchTDS', 'start', style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
    }
  })
}

serv_out[["a4boutonchTDS"]] <- function(calc, sess){
  renderUI({
    if (calc$actionTDS & (calc$chTDS == 'Clusters of subjects durations')){
      actionButton(inputId = 'a4actionchTDS', 'start', style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
    }
  })
}

serv_out[["b6boutonchTDS"]] <- function(calc, sess){
  renderUI({
    if (calc$actionTDS & (calc$chTDS == 'Differences of dominance curves')){
      actionButton(inputId = 'b6actionchTDS', 'start', style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
    }
  })
}

serv_out[["b6alphachTDS"]] <- function(calc, sess){
  renderUI({
    if (calc$actionTDS & (calc$chTDS == 'Differences of dominance curves')){
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
}

serv_out[["b6AnaDiffDomCurvStTDS"]] <- function(calc, sess){
  renderUI({
    if (calc$actionTDS & (calc$chTDS == 'Differences of dominance curves')){
      div(
        div(style="width:80%; display:inline-block; vertical-align: middle;",
            checkboxInput('clAnaDiffDomCurvSt', label = lexi['anaDiffDomCurvSt', calc$choiceL], value = FALSE)
        ),
        div(style="display:inline-block; vertical-align: middle;",
            bsButton("bsclAnaDiffDomCurvStst", label = "", icon = icon("question"), style = "info", size = "extra-small", placement = 'left'),
            shinyBS::bsTooltip("bsclAnaDiffDomCurvStst", lexi['bsanaDiffDomCurvStst', calc$choiceL])
        )
      )
    }
  })
}

serv_out[["c2boutonchTDS"]] <- function(calc, sess){
  renderUI({
    if (calc$actionTDS & (calc$chTDS == 'Barplot of citations')){
      actionButton(inputId = 'c2actionchTDS', 'start', style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
    }
  })
}

serv_out[["c2confintTDS"]] <- function(calc, sess){
  renderUI({
    if (calc$actionTDS & (calc$chTDS == 'Barplot of citations')){
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
}

serv_out[["d2confintTDS"]] <- function(calc, sess){
  renderUI({
    if (calc$actionTDS & (calc$chTDS == 'Barplot of durations')){
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
}

serv_out[["d1boutonchTDS"]] <- function(calc, sess){
  renderUI({
    if (calc$actionTDS & (calc$chTDS == 'Duration distribution')){
      actionButton(inputId = 'd1actionchTDS', 'start', style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
    }
  })
}

serv_out[["d2boutonchTDS"]] <- function(calc, sess){
  renderUI({
    if (calc$actionTDS & (calc$chTDS == 'Barplot of durations')){
      actionButton(inputId = 'd2actionchTDS', 'start', style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
    }
  })
}

serv_out[["d56TDSrepch"]] <- function(calc, sess){
  renderUI({
    if (calc$actionTDS & (calc$chTDS == 'PCA of durations by rep')){
      div(
        div(style="width:80%; display:inline-block; vertical-align: middle;",
            selectInput('d56TDSrepChoice', label = 'Rep :', choices = 1:nlevels(calc$ActTDS$df$rep), multiple = FALSE)
        ),
        div(style="display:inline-block; vertical-align: middle;",
            bsButton("bsd56TDSrepChoice", label = "", icon = icon("question"), style = "info", size = "extra-small", placement = 'left'),
            shinyBS::bsTooltip("bsd56TDSrepChoice", 'info')
        )
      )
    }
  })
}

serv_out[["d7TDSrepch"]] <- function(calc, sess){
  renderUI({
    if (calc$actionTDS & (calc$chTDS == 'CVA of durations by rep')){
      div(
        div(style="width:80%; display:inline-block; vertical-align: middle;",
            selectInput('d7TDSrepChoice', label = 'Rep :', choices = 1:nlevels(calc$ActTDS$df$rep), multiple = FALSE)
        ),
        div(style="display:inline-block; vertical-align: middle;",
            bsButton("bsd7TDSrepChoice", label = "", icon = icon("question"), style = "info", size = "extra-small", placement = 'left'),
            shinyBS::bsTooltip("bsd7TDSrepChoice", 'info')
        )
      )
    }
  })
}

serv_out[["d34TDSrepch"]] <- function(calc, sess){
  renderUI({
    if (calc$actionTDS & (calc$chTDS == 'ANOVA of durations by rep')){
      div(
        div(style="width:80%; display:inline-block; vertical-align: middle;",
            selectInput('d34TDSrepChoice', label = 'Rep :', choices = 1:nlevels(calc$ActTDS$df$rep), multiple = FALSE)
        ),
        div(style="display:inline-block; vertical-align: middle;",
            bsButton("bsd34TDSrepChoice", label = "", icon = icon("question"), style = "info", size = "extra-small", placement = 'left'),
            shinyBS::bsTooltip("bsd34TDSrepChoice", 'info')
        )
      )
    }
  })
}

serv_out[["alphaAnovaTDSrep"]] <- function(calc, sess){
  renderUI({
    if (calc$actionTDS & (calc$chTDS == 'ANOVA of durations by rep')){
      div(
        div(style="width:80%; display:inline-block; vertical-align: middle;",
            numericInput('alAnovaTDSrep', label = lexi['TDSana3alpha', calc$choiceL],value = 0.05, min = 0, max = 1, step = 0.01)
        ),
        div(style="display:inline-block; vertical-align: middle;",
            bsButton("bsalAnovaTDSrep", label = "", icon = icon("question"), style = "info", size = "extra-small", placement = 'left'),
            shinyBS::bsTooltip("bsalAnovaTDSrep", lexi['bsTDSana3alpha', calc$choiceL])
        )
      )
    }
  })
}

serv_out[["d56boutonchTDSrep"]] <- function(calc, sess){
  renderUI({
    if (calc$actionTDS & (calc$chTDS == 'PCA of durations by rep')){
      actionButton(inputId = 'd56actionchTDSrep', 'start', style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
    }
  })
}

serv_out[["d7boutonchTDSrep"]] <- function(calc, sess){
  renderUI({
    if (calc$actionTDS & (calc$chTDS == 'CVA of durations by rep')){
      actionButton(inputId = 'd7actionchTDSrep', 'start', style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
    }
  })
}

serv_out[["d34boutonchTDSrep"]] <- function(calc, sess){
  renderUI({
    if (calc$actionTDS & (calc$chTDS == 'ANOVA of durations by rep')){
      actionButton(inputId = 'd34actionchTDSrep', 'start', style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
    }
  })
}

serv_out[["citDistTDS"]] <- function(calc, sess){
  renderPlot({
    if (calc$c1actionchTDS & calc$chTDS == 'Citation distribution'){
      calc$c1TDS[[1]]$output$citationsDistribution
    }
  })
}

serv_out[["downloadtdsDFcsv"]] <- function(calc, sess){
  downloadHandler(
    filename = function() {
      paste("DF", ".csv", sep = '')
    },
    content = function(file) {
      write.csv2(calc$ActTDS$df, file, row.names = FALSE)
    }
  )
}

serv_out[["downloadtdsDFxlsx"]] <- function(calc, sess){
  downloadHandler(
    filename = function() {
      paste("DF", ".xlsx", sep = '')
    },
    content = function(file) {
      write.xlsx(calc$ActTDS$df, file, row.names = FALSE)
    }
  )
}

serv_out[["downloadDominanceTDScsv"]] <- function(calc, sess){
  downloadHandler(
    filename = function() {
      paste("Dominance", ".csv", sep = '')
    },
    content = function(file) {
      write.csv2(calc$ActTDS$dominances, file, row.names = FALSE)
    }
  )
}

serv_out[["downloadDominanceTDSxlsx"]] <- function(calc, sess){
  downloadHandler(
    filename = function() {
      paste("Dominance", ".xlsx", sep = '')
    },
    content = function(file) {
      write.xlsx(calc$ActTDS$dominances, file, row.names = FALSE)
    }
  )
}

serv_out[["downloadDominanceStTDScsv"]] <- function(calc, sess){
  downloadHandler(
    filename = function() {
      paste("DominanceSt", ".csv", sep = '')
    },
    content = function(file) {
      write.csv2(calc$ActTDS$stdDominances, file, row.names = FALSE)
    }
  )
}

serv_out[["downloadDominanceStTDSxlsx"]] <- function(calc, sess){
  downloadHandler(
    filename = function() {
      paste("DominanceSt", ".xlsx", sep = '')
    },
    content = function(file) {
      write.xlsx(calc$ActTDS$stdDominances, file, row.names = FALSE)
    }
  )
}

serv_out[["downloadDurationTDScsv"]] <- function(calc, sess){
  downloadHandler(
    filename = function() {
      paste("Duration", ".csv", sep = '')
    },
    content = function(file) {
      write.csv2(calc$ActTDS$durations, file, row.names = FALSE)
    }
  )
}

serv_out[["downloadDurationTDSxlsx"]] <- function(calc, sess){
  downloadHandler(
    filename = function() {
      paste("Duration", ".xlsx", sep = '')
    },
    content = function(file) {
      write.xlsx(calc$ActTDS$durations, file, row.names = FALSE)
    }
  )
}

serv_out[["downloadCitationTDScsv"]] <- function(calc, sess){
  downloadHandler(
    filename = function() {
      paste("Citation", ".csv", sep = '')
    },
    content = function(file) {
      write.csv2(calc$ActTDS$citations, file, row.names = FALSE)
    }
  )
}

serv_out[["downloadCitationTDSxlsx"]] <- function(calc, sess){
  downloadHandler(
    filename = function() {
      paste("Citation", ".xlsx", sep = '')
    },
    content = function(file) {
      write.xlsx(calc$ActTDS$citations, file, row.names = FALSE)
    }
  )
}

serv_out[["downloadbehavioursTDScsv"]] <- function(calc, sess){
  downloadHandler(
    filename = function() {
      paste("behaviours", ".csv", sep = '')
    },
    content = function(file) {
      write.csv2(calc$ActTDS$behaviours, file, row.names = FALSE)
    }
  )
}

serv_out[["downloadbehavioursTDSxlsx"]] <- function(calc, sess){
  downloadHandler(
    filename = function() {
      paste("behaviours", ".xlsx", sep = '')
    },
    content = function(file) {
      write.xlsx(calc$ActTDS$behaviours, file, row.names = FALSE)
    }
  )
}

serv_out[["tabProductTDS"]] <- function(calc, sess){
  renderDataTable({
    dat = data.frame(levels(calc$ActTDS$behaviours$product))
    names(dat) = 'Products'
    dat
  })
}

serv_out[["tabSubjectTDS"]] <- function(calc, sess){
  renderDataTable({
    dat = data.frame(levels(calc$ActTDS$behaviours$subject))
    names(dat) = 'Subjects'
    dat
  })
}

serv_out[["affNbSubTDS"]] <- function(calc, sess){
  renderText({
    as.character(calc$nbSubTDS)
  })
}

serv_out[["affNbProductTDS"]] <- function(calc, sess){
  renderText({
    as.character(calc$nbProductTDS)
  })
}

serv_out[["affNbRepTDS"]] <- function(calc, sess){
  renderText({
    as.character(calc$nbRepTDS)
  })
}

serv_out[["presentationTDS"]] <- function(calc, sess){
  renderText({
    if (calc$choiceL == 'en'){
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
    if (calc$nbSubTDS > 1){
      presSubTDS <- paste(sub,'s',sep='')
    }
    else{
      presSubTDS <- sub
    }
    if (calc$nbProductTDS > 1){
      presProdTDS <- paste(prod,'s',sep='')
    }
    else{
      presProdTDS <- prod
    }
    if (calc$nbRepTDS > 1){
      presRepTDS <- paste(rep,'s',sep='')
    }
    else{
      presRepTDS <- rep
    }
    paste(as.character(calc$nbSubTDS), presSubTDS, ',', as.character(calc$nbProductTDS), presProdTDS, andet, as.character(calc$nbRepTDS), presRepTDS)
  })
}

serv_out[['titleRapportTDS']] <- function(calc, sess){
  renderUI({
    textInput('titleTDS', lexi['titleRapportTDS', calc$choiceL], value = 'Report TDS')
  })
}

serv_out[['htmlTDS']] <- function(calc, sess){
  downloadHandler(
    filename = function() {
      "report.html"
    },
    content = function(file) {
      #pdf(file)
      #    if (calc$actAna1TDS){
      #        print(calc$a2TDS[[1]]$output$panelBehaviour)
      #        print(calc$a3TDS[[1]]$output$anova)
      #    }
      #    if (calc$actAna2TDS){
      #        print(calc$b1TDS[[1]]$output$sequences)
      #    }
      
      #dev.off()C:\Users\alexa\OneDrive\Documents\Cours\stage\ChemoSensPrivate\inst\extdata\shiny
      
      tempReport <- file.path(getwd(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
      
      if (isTruthy(calc$actAna1TDS)){
        a2rapport = print(calc$a2TDS[[1]]$output$panelBehaviour)
        a3rapport = print(calc$a3TDS[[1]]$output$anova)
      }
      else{
        a2r = analysis(calc$ActTDS,type="Panel behaviour table", title="Panel behaviour table",sequenceStart=15,sequenceDuration=6,nbDescriptors=1,nbClicks=1.5,descriptorDuration=2)
        graphics.off()
        a3r = analysis(calc$ActTDS,type="ANOVA of behaviours", title="ANOVA of behaviours", columns=c("G_Mean","F_Product","P_Product","SE_Product","R2","P_ShapiroWilks","P_Levene_Product","Mean_Product","Group_Product"), alpha=0.05)
        graphics.off()
        a2rapport = print(a2r[[1]]$output$panelBehaviour)
        a3rapport = print(a3r[[1]]$output$anova)
      }
      
      if (isTruthy(calc$actAna2TDS)){
        b1rapport = calc$b1TDS[[1]]$output$sequences
        b23rapport = calc$b23TDS[[1]]$output$dominanceCurves
        b89rapport = calc$b89TDS[[1]]$output$panelSequences
        d9rapport = calc$d9TDS[[1]]$output$biplot
      }
      else {
        b1r = analysis(calc$ActTDS,type="Individual sequences", title="Individual sequences")
        graphics.off()
        b23r = analysis(calc$ActTDS,type="Standardized dominance curves", title="Dominance curves", alpha=0.05, repAsIndividual=TRUE, rows="product", cols="rep", color="descriptor", smooth=TRUE, draw=c("significance","hasard","graymask"))
        graphics.off()
        b89r = analysis(calc$ActTDS,type="Standardized panel sequences", title="Panel bandplot", alpha=0.05, repAsIndividual=TRUE)
        graphics.off()
        d9r = analysis(calc$ActTDS,type="PCA of trajectories", title="PCA of trajectories", axes=list(c(1,2)), periods=7, fontSizeCex = 2)
        graphics.off()
        b1rapport = b1r[[1]]$output$sequences
        b23rapport = b23r[[1]]$output$dominanceCurves
        b89rapport = b89r[[1]]$output$panelSequences
        d9rapport = d9r[[1]]$output$biplot
      }
      
      if (isTruthy(calc$actAna3TDS)){
        d3rapport = print(calc$d3TDS[[1]]$output$anova)
        d56rapport = calc$d56TDS[[1]]$output$biplot
        d7rapport = calc$d7TDS[[1]]$output$biplot
      }
      else {
        d3r = analysis(calc$ActTDS,type="ANOVA of durations", title="ANOVA of durations", columns=c("G_Mean","F_Product","P_Product","F_Product_Significance","SE_Product","R2","P_ShapiroWilks","P_Levene_Product","Mean_Product","Group_Product","P_Subject"), alpha=0.05)
        graphics.off()
        d56r = analysis(calc$ActTDS,type="PCA of durations", title="PCA", fontSizeCex = 2)
        graphics.off()
        d7r = analysis(calc$ActTDS,type="CVA of durations", title="CVA", fontSizeCex = 2)
        graphics.off()
        d3rapport = print(d3r[[1]]$output$anova)
        d56rapport = d56r[[1]]$output$biplot
        d7rapport = d7r[[1]]$output$biplot
      }
      
      params <- list(
        titleRTDS = calc$titleTDS,
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
      
    }
  )
}


serv_out[['pdfTDS']] <- function(calc, sess){
  downloadHandler(
    filename = function() {
      "report.pdf"
    },
    content = function(file) {
      
      tempReport <- file.path("C:/Users/alexa/OneDrive/Documents/Cours/stage/ChemoSensPrivate/inst/extdata/shiny", "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
      
      params <- list(
        printa2 = print(calc$a2TDS[[1]]$output$panelBehaviour),
        printa3 = print(calc$a3TDS[[1]]$output$anova),
        plotb1 = calc$b1TDS[[1]]$output$sequences,
        plotb23 = calc$b23TDS[[1]]$output$dominanceCurves,
        plotb89 = calc$b89TDS[[1]]$output$panelSequences,
        plotd9 = calc$d9TDS[[1]]$output$biplot,
        printd3 = print(calc$d3TDS[[1]]$output$anova),
        plotd56 = calc$d56TDS[[1]]$output$biplot,
        plotd7 = calc$d7TDS[[1]]$output$biplot
      )
      
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
      
    }
  )
}

serv_out[['zipTDS']] <- function(calc, sess){
  downloadHandler(
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
      
      if (isTruthy(calc$actAna1TDS)){
        a2rapport = print(calc$a2TDS[[1]]$output$panelBehaviour)
        a2rapportDF = calc$a2TDS[[1]]$df
        a3rapport = print(calc$a3TDS[[1]]$output$anova)
        a3rapportDF = calc$a3TDS[[1]]$output$result
      }
      else{
        a2r = analysis(calc$ActTDS,type="Panel behaviour table", title="Panel behaviour table",sequenceStart=15,sequenceDuration=6,nbDescriptors=1,nbClicks=1.5,descriptorDuration=2)
        graphics.off()
        a3r = analysis(calc$ActTDS,type="ANOVA of behaviours", title="ANOVA of behaviours", columns=c("G_Mean","F_Product","P_Product","SE_Product","R2","P_ShapiroWilks","P_Levene_Product","Mean_Product","Group_Product"), alpha=0.05)
        graphics.off()
        a2rapport = print(a2r[[1]]$output$panelBehaviour)
        a2rapportDF = a2r[[1]]$df
        a3rapport = print(a3r[[1]]$output$anova)
        a3rapportDF = a3r[[1]]$output$result
      }
      
      if (isTruthy(calc$actAna2TDS)){
        b1rapport = calc$b1TDS[[1]]$output$sequences
        b23rapport = calc$b23TDS[[1]]$output$dominanceCurves
        b89rapport = calc$b89TDS[[1]]$output$panelSequences
        d9rapport = calc$d9TDS[[1]]$output$biplot
      }
      else {
        b1r = analysis(calc$ActTDS,type="Individual sequences", title="Individual sequences")
        graphics.off()
        b23r = analysis(calc$ActTDS,type="Standardized dominance curves", title="Dominance curves", alpha=0.05, repAsIndividual=TRUE, rows="product", cols="rep", color="descriptor", smooth=TRUE, draw=c("significance","hasard","graymask"))
        graphics.off()
        b89r = analysis(calc$ActTDS,type="Standardized panel sequences", title="Panel bandplot", alpha=0.05, repAsIndividual=TRUE)
        graphics.off()
        d9r = analysis(calc$ActTDS,type="PCA of trajectories", title="PCA of trajectories", axes=list(c(1,2)), periods=7, fontSizeCex = 2)
        graphics.off()
        b1rapport = b1r[[1]]$output$sequences
        b23rapport = b23r[[1]]$output$dominanceCurves
        b89rapport = b89r[[1]]$output$panelSequences
        d9rapport = d9r[[1]]$output$biplot
      }
      
      if (isTruthy(calc$actAna3TDS)){
        d3rapport = print(calc$d3TDS[[1]]$output$anova)
        d3rapportDF = calc$d3TDS[[1]]$output$result
        d56rapport = calc$d56TDS[[1]]$output$biplot
        d7rapport = calc$d7TDS[[1]]$output$biplot
      }
      else {
        d3r = analysis(calc$ActTDS,type="ANOVA of durations", title="ANOVA of durations", columns=c("G_Mean","F_Product","P_Product","F_Product_Significance","SE_Product","R2","P_ShapiroWilks","P_Levene_Product","Mean_Product","Group_Product","P_Subject"), alpha=0.05)
        graphics.off()
        d56r = analysis(calc$ActTDS,type="PCA of durations", title="PCA", fontSizeCex = 2)
        graphics.off()
        d7r = analysis(calc$ActTDS,type="CVA of durations", title="CVA", fontSizeCex = 2)
        graphics.off()
        d3rapport = print(d3r[[1]]$output$anova)
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
    },
    contentType = "application/zip")
}

serv_out[["individualSequencesTDSpng"]] <- function(calc, sess){
  
  downloadHandler(
    filename = function() {
      paste("individualSequences", ".png", sep = '')
    },
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = width, height = height,
                       res = 300, units = "in")
      }
      ggsave(file, plot = calc$b1TDS[[1]]$output$sequences, device = device)
    }
  )
}

serv_out[["individualSequencesTDSsvg"]] <- function(calc, sess){
  downloadHandler(
    filename = function() {
      paste("individualSequences", ".svg", sep = '')
    },
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::svg(..., width = width, height = height)
      }
      ggsave(file, plot = calc$b1TDS[[1]]$output$sequences, device = device)
    }
  )
}

serv_out[["PanelSequencesTDSpng"]] <- function(calc, sess){
  downloadHandler(
    filename = function() {
      paste("PanelSequences", ".png", sep = '')
    },
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = width, height = height,
                       res = 300, units = "in")
      }
      ggsave(file, plot = calc$b89TDS[[1]]$output$panelSequences, device = device)
    }
  )
}

serv_out[["PanelSequencesTDSsvg"]] <- function(calc, sess){
  downloadHandler(
    filename = function() {
      paste("PanelSequences", ".svg", sep = '')
    },
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::svg(..., width = width, height = height)
      }
      ggsave(file, plot = calc$b89TDS[[1]]$output$panelSequences, device = device)
    }
  )
}

serv_out[["PCAtrajectoriesTDSpng"]] <- function(calc, sess){
  downloadHandler(
    filename = function() {
      paste("PCAtrajectories", ".png", sep = '')
    },
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = width, height = height,
                       res = 300, units = "in")
      }
      ggsave(file, plot = calc$d9TDS[[1]]$output$biplot, device = device)
    }
  )
}

serv_out[["PCAtrajectoriesTDSsvg"]] <- function(calc, sess){
  downloadHandler(
    filename = function() {
      paste("PCAtrajectories", ".svg", sep = '')
    },
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::svg(..., width = width, height = height)
      }
      ggsave(file, plot = calc$d9TDS[[1]]$output$biplot, device = device)
    }
  )
}

serv_out[["DominanceCurvesTDSpng"]] <- function(calc, sess){
  downloadHandler(
    filename = function() {
      paste("DominanceCurves", ".png", sep = '')
    },
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = width, height = height,
                       res = 300, units = "in")
      }
      ggsave(file, plot = calc$b23TDS[[1]]$output$dominanceCurves, device = device)
    }
  )
}

serv_out[["DominanceCurvesTDSsvg"]] <- function(calc, sess){
  downloadHandler(
    filename = function() {
      paste("DominanceCurves", ".svg", sep = '')
    },
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::svg(..., width = width, height = height)
      }
      ggsave(file, plot = calc$b23TDS[[1]]$output$dominanceCurves, device = device)
    }
  )
}

serv_out[["citDistTDSpng"]] <- function(calc, sess){
  downloadHandler(
    filename = function() {
      paste("citationsDistribution", ".png", sep = '')
    },
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = width, height = height,
                       res = 300, units = "in")
      }
      ggsave(file, plot = calc$c1TDS[[1]]$output$citationsDistribution, device = device)
    }
  )
}

serv_out[["citDistTDSsvg"]] <- function(calc, sess){
  downloadHandler(
    filename = function() {
      paste("citationsDistribution", ".svg", sep = '')
    },
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::svg(..., width = width, height = height)
      }
      ggsave(file, plot = calc$c1TDS[[1]]$output$citationsDistribution, device = device)
    }
  )
}

serv_out[["BehaviourDistTDSpng"]] <- function(calc, sess){
  downloadHandler(
    filename = function() {
      paste("panelBehaviourDistribution", ".png", sep = '')
    },
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = width, height = height,
                       res = 300, units = "in")
      }
      ggsave(file, plot = calc$a1TDS[[1]]$output$panelBehaviourDistribution, device = device)
    }
  )
}

serv_out[["BehaviourDistTDSsvg"]] <- function(calc, sess){
  downloadHandler(
    filename = function() {
      paste("panelBehaviourDistribution", ".svg", sep = '')
    },
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::svg(..., width = width, height = height)
      }
      ggsave(file, plot = calc$a1TDS[[1]]$output$panelBehaviourDistribution, device = device)
    }
  )
}

serv_out[["ClustDurationTDSpng"]] <- function(calc, sess){
  downloadHandler(
    filename = function() {
      paste("durationsClusters", ".png", sep = '')
    },
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = width, height = height,
                       res = 300, units = "in")
      }
      ggsave(file, plot = calc$a4TDS[[1]]$output$durationsClusters, device = device)
    }
  )
}

serv_out[["ClustDurationTDSsvg"]] <- function(calc, sess){
  downloadHandler(
    filename = function() {
      paste("durationsClusters", ".svg", sep = '')
    },
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::svg(..., width = width, height = height)
      }
      ggsave(file, plot = calc$a4TDS[[1]]$output$durationsClusters, device = device)
    }
  )
}

serv_out[["DiffDomCurvTDSpng"]] <- function(calc, sess){
  downloadHandler(
    filename = function() {
      paste("differenceCurves", ".png", sep = '')
    },
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = width, height = height,
                       res = 300, units = "in")
      }
      ggsave(file, plot = calc$b6TDS[[1]]$output$differenceCurves, device = device)
    }
  )
}

serv_out[["DiffDomCurvTDSsvg"]] <- function(calc, sess){
  downloadHandler(
    filename = function() {
      paste("differenceCurves", ".svg", sep = '')
    },
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::svg(..., width = width, height = height)
      }
      ggsave(file, plot = calc$b6TDS[[1]]$output$differenceCurves, device = device)
    }
  )
}

serv_out[["BarCitTDSpng"]] <- function(calc, sess){
  downloadHandler(
    filename = function() {
      paste("barplotCitation", ".png", sep = '')
    },
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = width, height = height,
                       res = 300, units = "in")
      }
      ggsave(file, plot = calc$c2TDS[[1]]$output$barplot, device = device)
    }
  )
}

serv_out[["BarCitTDSsvg"]] <- function(calc, sess){
  downloadHandler(
    filename = function() {
      paste("barplotCitation", ".svg", sep = '')
    },
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::svg(..., width = width, height = height)
      }
      ggsave(file, plot = calc$c2TDS[[1]]$output$barplot, device = device)
    }
  )
}

serv_out[["DurDistTDSpng"]] <- function(calc, sess){
  downloadHandler(
    filename = function() {
      paste("durationsDistribution", ".png", sep = '')
    },
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = width, height = height,
                       res = 300, units = "in")
      }
      ggsave(file, plot = calc$d1TDS[[1]]$output$durationsDistribution, device = device)
    }
  )
}

serv_out[["DurDistTDSsvg"]] <- function(calc, sess){
  downloadHandler(
    filename = function() {
      paste("durationsDistribution", ".svg", sep = '')
    },
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::svg(..., width = width, height = height)
      }
      ggsave(file, plot = calc$d1TDS[[1]]$output$durationsDistribution, device = device)
    }
  )
}

serv_out[["BarDurTDSpng"]] <- function(calc, sess){
  downloadHandler(
    filename = function() {
      paste("barplotDuration", ".png", sep = '')
    },
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = width, height = height,
                       res = 300, units = "in")
      }
      ggsave(file, plot = calc$d2TDS[[1]]$output$barplot, device = device)
    }
  )
}

serv_out[["BarDurTDSsvg"]] <- function(calc, sess){
  downloadHandler(
    filename = function() {
      paste("barplotDuration", ".svg", sep = '')
    },
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::svg(..., width = width, height = height)
      }
      ggsave(file, plot = calc$d2TDS[[1]]$output$barplot, device = device)
    }
  )
}

serv_out[["bodyTDS"]] <- function(calc, sess){
  renderUI({
    
    if (calc$actionTDS){
      navbarPage("", id = "tabselected",
                 tabPanel(lexi['tdsSummary', calc$choiceL], value = 1,
                          tabsetPanel(
                            tabPanel(lexi['tdsUnit', calc$choiceL],
                                     fluidRow(
                                       textOutput('presentationTDS')
                                     ),
                                     fluidRow(
                                       box(title = 'products', width = 6, solidHeader = TRUE, dataTableOutput('tabProductTDS')),
                                       box(title = 'subjects', width = 6, solidHeader = TRUE, dataTableOutput('tabSubjectTDS'))
                                     ),
                            ),
                            tabPanel(lexi['tdsDF', calc$choiceL],
                                     dataTableOutput('dfTDS'),
                                     downloadButton("downloadtdsDFcsv", "csv"),
                                     downloadButton("downloadtdsDFxlsx", "xlsx")),
                            tabPanel(lexi['tdsDominances', calc$choiceL],
                                     dataTableOutput('dominancesTDS'),
                                     downloadButton("downloadDominanceTDScsv", "csv"),
                                     downloadButton("downloadDominanceTDSxlsx", "xlsx")),
                            tabPanel(lexi['tdsDominancesSTD', calc$choiceL],
                                     dataTableOutput('stdDominancesTDS'),
                                     downloadButton("downloadDominanceStTDScsv", "csv"),
                                     downloadButton("downloadDominanceStTDSxlsx", "xlsx")),
                            tabPanel(lexi['tdsDurations', calc$choiceL],
                                     dataTableOutput('durationsTDS'),
                                     downloadButton("downloadDurationTDScsv", "csv"),
                                     downloadButton("downloadDurationTDSxlsx", "xlsx")),
                            tabPanel(lexi['tdsCitations', calc$choiceL],
                                     dataTableOutput('citationsTDS'),
                                     downloadButton("downloadCitationTDScsv", "csv"),
                                     downloadButton("downloadCitationTDSxlsx", "xlsx")),
                            tabPanel(lexi['tdsBehaviours', calc$choiceL],
                                     dataTableOutput('behavioursTDS'),
                                     downloadButton("downloadbehavioursTDScsv", "csv"),
                                     downloadButton("downloadbehavioursTDSxlsx", "xlsx"))
                          )
                 ),
                 
                 tabPanel(lexi['analysis1TDS', calc$choiceL], value = 2,
                          tabsetPanel(
                            tabPanel(lexi['tdsPanelistTable', calc$choiceL],
                                     htmlOutput('BehaviourPanelTDS'),
                                     downloadButton("downloadpanelBehaviourTDS", "html"),
                                     downloadButton("downloadpanelBehaviourTDScsv", "csv")),
                            tabPanel(lexi['tdsAnovaBehaviour', calc$choiceL], value = 'ANOVA of behaviours',
                                     htmlOutput('BehaviourAnovaTDS'),
                                     downloadButton("downloadpanelBehaviourAnovaTDS", "html"),
                                     downloadButton("downloadpanelBehaviourAnovaTDScsv", "csv"))
                            #tabPanel(lexi['tdsbandplotPanelist', calc$choiceL], value = 'Bandplot by panelist',
                            #         plotOutput('bandplotPanelistTDS', height = "800px"))
                            #tabPanel('Clusters of subjects durations',
                            #         plotOutput('ClustDurationTDS', height = "800px"))
                          )
                 ),
                 
                 tabPanel(lexi['analysis2TDS', calc$choiceL], value=3,
                          tabsetPanel(
                            tabPanel(lexi['individualSequencesTDS', calc$choiceL],
                                     plotOutput('indSeqTDS', height = "800px"),
                                     downloadButton("individualSequencesTDSpng", "png"),
                                     downloadButton("individualSequencesTDSsvg", "svg")
                            ),
                            tabPanel(lexi['TDScurveDominances', calc$choiceL],
                                     plotOutput('domCurTDS', height = "800px"),
                                     downloadButton("DominanceCurvesTDSpng", "png"),
                                     downloadButton("DominanceCurvesTDSsvg", "svg")
                            ),
                            tabPanel(lexi['panelBandplotTDS', calc$choiceL],
                                     plotOutput('PanelSequencesTDS', height = "800px"),
                                     downloadButton("PanelSequencesTDSpng", "png"),
                                     downloadButton("PanelSequencesTDSsvg", "svg")
                            ),
                            #tabPanel('PCA of trajectories',
                            #         imageOutput('PCAtrajectoriesTDS'))
                            #tabPanel('PCA of trajectories',
                            #         div(style="display: block; margin-left: auto; margin-right: auto;",imageOutput('PCAtrajectoriesTDS'))
                            #),
                            tabPanel(lexi['TDSpcaTrajectories', calc$choiceL],
                                     plotOutput('PCAtrajectoriesTDS', height = "800px"),
                                     downloadButton("PCAtrajectoriesTDSpng", "png"),
                                     downloadButton("PCAtrajectoriesTDSsvg", "svg")
                            )
                            #tabPanel('Maximum dominance rates',
                            #         plotOutput('maxDomTDS', height = "800px"))
                          )
                 ),
                 
                 tabPanel(lexi['analysis3TDS', calc$choiceL], value=6,
                          tabsetPanel(
                            tabPanel(lexi['anovaDurationsTDS', calc$choiceL],
                                     uiOutput('AnovaDurationsTDS'),
                                     downloadButton("AnovaDurationsTDShtml", "html"),
                                     downloadButton("AnovaDurationsTDScsv", "csv")
                            ),
                            tabPanel(lexi['pcaDurationsTDS', calc$choiceL],
                                     plotOutput('PCATDS', height = "800px"),
                                     downloadButton("PCATDSpng", "png"),
                                     downloadButton("PCATDSsvg", "svg")
                            ),
                            tabPanel(lexi['cvaDurationsTDS', calc$choiceL],
                                     plotOutput('CVATDS', height = "800px"),
                                     downloadButton("CVATDSpng", "png"),
                                     downloadButton("CVATDSsvg", "svg")
                            )
                          )
                 ),
                 
                 tabPanel(lexi['downloadMenuTDS', calc$choiceL], value=7,
                        
                 ),
                 
                 tabPanel(lexi['customAnalysisTDS', calc$choiceL], value=4,
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
                                               uiOutput('AnovaDurationsTDSrep'),
                                               downloadButton("AnovaDurationsTDShtmlrep", "html"),
                                               downloadButton("AnovaDurationsTDScsvrep", "csv")
                                      )
                          )
                 )
      )
    }
  })
  
}