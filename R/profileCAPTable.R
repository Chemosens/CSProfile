#'CAPTable
#'
#'This function build an html file (CAPTable.html) containing a panel and panelist performance table (CAP or MAM-CAP table) in current working directory.
#'@param profileObject resulted from \link{profileReadData}
#'@param model character. "TwoWayANOVA" or "MAM": model of performance tests. "TwoWayANOVA" refers to the first Schlich CAP table available on the SensoBase (www.sensobase.fr) and does not take the scaling into account."MAM" refers to the MAM-CAP table, using the performances tests of Mixed Assessor Model (2013) 
#'@param panelLimit limit of the statistical tests concerning panel performances
#'@param indivLimit limit of the statistical tests concerning individual performances. When the chosen model is CAP, limitIndiv is only the limit of discrimination tests (Kendall agreement test limit = 0.2 and repeatability test= 0.01)
#' @param output the name of the html file is created in the current working directory 
#' @param correction logical. If TRUE, limitIndiv is divided by the number of panelists
#' @param correlationTest if model=='CAP', test for the agreement between panelists 'kendall' by default, but can also be "spearman" or "pearson". See cor.test
#' @param indivRepLimCap if model =='CAP', limit for significance in repeatability tests. 0.01 by default
#' @param indivAgLimCap if model =='CAP', limit for significance in disagreement tests. 0.2 by default
#' @param repInIndModel if model =='CAP', repInIndModel indicates whether the replicate should be included in the individual model (using the model 'TwoWayAdditiveBySubject'). Default to FALSE (using the model 'OneWayBySubject')
#' @references
#'Brockhoff P. , Schlich P.; Skovgaard I, Taking individual scaling differences into account by analyzing profile data with the Mixed Assessor Model. (submitted in Food Quality and Preferences in July 2013)
#' Peltier C., Brockhoff, P., Visalli, M., Schlich, P. The MAM-CAP table: a new tool to monitor panel performances. (accepted in Food Quality and Preferences in July 2013)
#' @examples
#' data(cheeses)
#' profileCAPTable(cheeses)
#' profileCAPTable(cheeses,model="CAP")
#'@export
#'@import doBy
profileCAPTable = function(profileObject,model="MAM",panelLimit=0.05,indivLimit=0.05,output="CAPTable",correction=FALSE,correlationTest="Kendall", indivRepLimCap=0.01, indivAgLimCap=0.2, repInIndModel=FALSE)
{ 	
	correlationTest=tolower(correlationTest)
	language="en"
	if(!model%in% c("CAP","MAM","Overall"))
	{
		stop("Please choose model in 'CAP' or 'MAM' or 'Overall'")
	}
	if (model == "CAP")
	{
		#if(is.null(profileObject$ANOVAModel)){stop("Your univariate analyses were not calculated. They should be to select 'Default Model'")}
	  if(!profileObject$additionalCalculations)
	  {
	    if(length(profileObject$Replicates)==1){model="TwoWayAdditive"}else{model="TwoWayMultiplicative"}
	    profileObject=profileSetUnivariateAnalysisParameters(profileObject, model=model, randomEffects="Subject", anovaCalculationMode="Ols", lsMeansAdjustment="Tukey", lsMeansAlpha=.05)
	  }   
		resultingList=CAP(object=profileObject,panelLimit=panelLimit,indivRepeatabilityLimit=indivRepLimCap,indivAgreementLimit=indivAgLimCap,indivDiscriminationLimit=indivLimit,language=language,output=output,correction=correction,correlationTest=correlationTest,repInIndModel=repInIndModel)
	}
	if (model == "MAM")
	{
		resultingList=MAMCAP(object=profileObject,panelLimit=panelLimit,indivLimit=indivLimit,language=language,output=output,correction=correction,option="mam",correlationTest=correlationTest,levelOption=FALSE)
	}
	if (model == "Overall")
	{
		resultingList=MAMCAP(object=profileObject,panelLimit=panelLimit,indivLimit=indivLimit,language=language,output=output,correction=correction,option="overall",correlationTest=correlationTest,levelOption=FALSE)
	}
	# if (model == "Overall")
	# {
	#   resultingList=MAMCAP(object=profileObject,panelLimit=panelLimit,indivLimit=indivLimit,language=language,output=output,correction=correction,option="overall",correlationTest=correlationTest,levelOption=FALSE)
	# }
	
	print(paste("Your CAPTable was built in your working directory as ", output,".html in ",getwd(),". You can copy and paste it in Word.", sep=""))
	return(resultingList)
}