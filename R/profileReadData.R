#' profileReadData
#' 
#' Read data from profile experiment
#' @param file default to NULL. File to open whose colnames are "ProductCode" "SubjectCode","AttributeCode", "Replicate" and "Score" and potentially "Session"
#' @param allData default to NULL. Allows to use this function also for data already loaded in R. In this case, file should be NULL and allData should be a data.frame whose colnames are "ProductCode" "SubjectCode","AttributeCode", "Replicate" and "Score" (potentially also "Session")
#' @param replaceNA default to "crossmean"
#' @param sep default to ";". Colnames separator for the file
#' @param transformSessionIntoReplicate default to TRUE. Should a session be transformed to replicate ?
#' @param percentageOfSubjectsByReplicate default to 0. Option to deal with missing values. 
#' @param percentageOfReplicatesBySubject default to 0. Option to deal with missing values.
#' @param percentageOfProductBySubject default to 0. Option to deal with missing values.
#' @param productColors default to NULL. Colors associated to each product (for radarplot)
#' @param additionalCalculations default to T. If TRUE, the anova calculations are calculated in the object and can be used in the graphical plot or further results.
#' @param model default to NULL. In this case, the model is "twoWayMultiplicative" for more than 1 replicate. 
#' @param randomEffects default to "Subject". 
#' @param anovaCalculationMode default to "Ols" (Ordinary Least Square). Could also be "ML" for maximum likelihood. 
#' @param lsMeansAdjustment default to "Tukey". Could also be "sidak"
#' @param lsMeansAlpha default to 0.05. Limit of significance for product group constitutions
#' @param onlySignificantAttributesInMultivariateAnalysis default to TRUE. Should only the significant attributes be kept in multivariate analyses ? 
#' @param alphaForSelectionOfSignificantAttributes default to T. Significance threshold for the attributes to be kept in the multivariate analyses.
#'@export
#'@importFrom utils read.csv
#'@importFrom grDevices rainbow
profileReadData = function(file=NULL,allData=NULL,outputsCsv=NULL, replaceNA="crossmean", 
                           language="en",sep=";",transformSessionIntoReplicate=TRUE, percentageOfSubjectsByReplicate=0, 
                           percentageOfReplicatesBySubject=0,percentageOfProductBySubject=0,productColors=NULL,
                           additionalCalculations=T,
                           model=NULL, randomEffects="Subject", anovaCalculationMode="Ols", lsMeansAdjustment="Sidak", lsMeansAlpha=0.05,
                           onlySignificantAttributesInMultivariateAnalysis=T,alphaForSelectionOfSignificantAttributes=T
)
{
# Anciens defauts :percentageOfSubjectsByReplicate=0.75, percentageOfReplicatesBySubject=0.5,percentageOfProductBySubject=0.5
	if (is.null(file)&&is.null(allData))
	{
		stop("Parameter required: file.")
	}
	
	# Variable globale pour les traductions
	assign("TimeSensLanguage", language, envir = .GlobalEnv)
	
	profileObject=list()
	
	# Lecture du fichier texte
	if(!is.null(file))
	{
		allData = read.csv(file, header=TRUE, sep=sep) 
	}
	# S?eection des donnees de type Profile, format canonique
	canonicalData = allData
	if(dim(canonicalData)[1]==0){stop("No profile type in the data")}
	# Suppression des doublons dans completeExtendedData
	duplicatedData=which(duplicated(canonicalData))
	if(length(duplicatedData)>0)
	{
		canonicalData= canonicalData[-duplicatedData,]
	}
	

	# V?rification des variables
	res.CheckCanonicalData = CheckCanonicalData(canonicalData,variables=c("Session","SubjectCode","ProductCode","AttributeCode","Replicate","Score"),transformSessionIntoReplicate=transformSessionIntoReplicate)
	canonicalData = res.CheckCanonicalData$CanonicalData
	
	# Format canonique complet, sans donn?es manquantes
	res.completeData = CompleteCanonicalData(canonicalData, percentageOfSubjectsByReplicate=percentageOfSubjectsByReplicate, percentageOfReplicatesBySubject=percentageOfReplicatesBySubject,percentageOfProductBySubject=percentageOfProductBySubject)
	completeCanonicalData = res.completeData$CompleteCanonicalData	
	completeCanonicalDataWithoutNA = InputMissingValuesInCanonicalData(canonicalData, replaceNA=replaceNA, percentageOfSubjectsByReplicate=percentageOfSubjectsByReplicate, percentageOfReplicatesBySubject=percentageOfReplicatesBySubject,percentageOfProductBySubject=percentageOfProductBySubject)
	
	# Format ?tendu
	extendedData = CanonicalDataToExtendedData(canonicalData)
	
	# Format ?tendu complet avec donn?es manquantes
	completeExtendedData = CanonicalDataToExtendedData(completeCanonicalData)
	
	
	# Format ?tendu complet, sans donn?es manquantes
	if(!is.null(completeCanonicalDataWithoutNA))
	{
		completeExtendedDataWithoutNA = CanonicalDataToExtendedData(completeCanonicalDataWithoutNA)
	}
	else
	{
		completeExtendedDataWithoutNA=NULL
	}
	# Description du jeu de donn?es
	# TS_LogEntry(paste("<h3>",TS_GetLabel("Current dataset"),": </h3>",sep=""))
	# TS_LogEntry(paste("<p>",TS_GetLabel("ReadRows"),": ",res.completeData$Rows,"</p>",sep=""))
	# TS_LogEntry(paste("<p>",TS_GetLabel("IsComplete"),": ",TS_GetLabel(as.character(res.completeData$IsComplete)),"</p>",sep=""))
	# TS_LogEntry(paste("<p>",TS_GetLabel("IsBalanced"),": ",TS_GetLabel(as.character(res.completeData$IsBalanced)),"</p>",sep=""))
	 if (res.completeData$DuplicatedRows > 0)
	 { 
	   warning("Duplicated rows: ",res.completeData$DuplicatedRows)
	# 	TS_LogEntry(paste("<p>",TS_GetLabel("DuplicatedRows"),": ",res.completeData$DuplicatedRows,"</p>",sep=""))
	 }
	 if (res.completeData$DeletedRows > 0)
	 {
	# 	TS_LogEntry(paste("<p>",TS_GetLabel("DeletedRows"),": ",res.completeData$DeletedRows,"</p>",sep=""))
	     warning("Deleted rows: ",res.completeData$DeletedRows)
	  }
	 if (res.completeData$MissingRows > 0)
	 {
	 	#TS_LogEntry(paste("<p>",TS_GetLabel("MissingRows"),": ",res.completeData$MissingRows,", (",round(res.completeData$PercentageOfMissingRows,2),"%) ",TS_GetLabel("CompleteWithMethod"),": ",TS_GetLabel(replaceNA),"</p>",sep=""))
	  warning("Missing rows:  ",res.completeData$MissingRows," (",round(res.completeData$PercentageOfMissingRows,2),"%), completed with ",replaceNA)
	 }
	# 
	# TS_LogEntry(paste("<p>",TS_GetLabel("Sessions"),": ",length(unique(completeCanonicalData$Session)),"</p>",sep=""))
	# TS_LogEntry(paste("<p>",TS_GetLabel("Subjects"),": ",length(unique(completeCanonicalData$SubjectCode)),"</p>",sep=""))
	# TS_LogEntry(paste("<p>",TS_GetLabel("Products"),": ",length(unique(completeCanonicalData$ProductCode)),"</p>",sep=""))
	# TS_LogEntry(paste("<p>",TS_GetLabel("Attributes"),": ",length(unique(completeCanonicalData$AttributeCode)),"</p>",sep=""))
	# TS_LogEntry(paste("<p>",TS_GetLabel("Replicates"),": ",length(unique(completeCanonicalData$Replicate)),"</p>",sep=""))
	# 
	# Objet profil 
	profileObject[["CompleteCanonicalDataWithoutNA"]] = completeCanonicalDataWithoutNA
	profileObject[["CompleteExtendedDataWithoutNA"]] = completeExtendedDataWithoutNA

	if(replaceNA=="None"||nrow(completeCanonicalDataWithoutNA)==0) # cas ou on ne remplace pas les valeurs manquantes
	{
		profileObject[["IsComplete"]]=res.completeData$IsComplete
		profileObject[["IsBalanced"]]=res.completeData$IsBalanced
	}
	else 
	{
	
			profileObject[["IsComplete"]]=TRUE
			profileObject[["IsBalanced"]]=TRUE
		
	}
	profileObject[["Attributes"]] = unique(completeCanonicalData$AttributeCode)
	profileObject[["Products"]] = unique(completeCanonicalData$ProductCode)
	profileObject[["Subjects"]] = unique(completeCanonicalData$SubjectCode)
	profileObject[["Replicates"]] = unique(completeCanonicalData$Replicate)
	profileObject[["Sessions"]] = levels(completeCanonicalData$Session)
	profileObject[["Language"]] = language
	if(!is.null(productColors))
	{
	  if(any(!names(productColors)%in%profileObject$Products)){warning("productColors name is not in Products")}
	  else{cols=productColors}
	}
	
	if(is.null(productColors))
	{
	  cols=rainbow(length(profileObject$Products))
	  names(cols)=profileObject$Products
	  
	}
	profileObject[["ProductColors"]]=cols
	# Tableau de donn?es au format ?tendu
	# if (!is.null(outputsCsv))
	# {
	# 	if ("ExtendedTable" %in% outputsCsv)
	# 	{
	# 		write.table(profileObject[["CompleteExtendedDataWithoutNA"]][,c(1:4,sort(colnames(completeExtendedDataWithoutNA)[-c(1:4)],index.return=TRUE)$ix+4),],"ExtendedData.csv",sep=",",row.names=FALSE)
	# 	}
	# }
	profileObject[["additionalCalculations"]]=F
	if(additionalCalculations)
	{
	  if(is.null(model)){if(length(profileObject$Replicates)==1){model="TwoWayAdditive"}else{model="TwoWayMultiplicative"}}
	  profileObject=profileSetUnivariateAnalysisParameters(profileObject, model=model, randomEffects=randomEffects, anovaCalculationMode=anovaCalculationMode, lsMeansAdjustment=lsMeansAdjustment, lsMeansAlpha=lsMeansAlpha)
	  profileObject[["OnlySignificantAttributesInMultivariateAnalysis"]]=onlySignificantAttributesInMultivariateAnalysis
	  profileObject[["AlphaForSelectionOfSignificantAttributes"]]=alphaForSelectionOfSignificantAttributes
	  profileObject[["additionalCalculations"]]=T
	}
	
	return (profileObject)
}

