#'profileAnova
#'
#'Allows to get the ANOVA results (and relative posthoc tests) for all descriptors
#'@param profileObject a profileObject obtained with \link{profileReadData}
#'@param model default to "DefaultModel". In this case, when ANOVA was calculated in profileReadData (useful for displaying statistics and groups on boxplot for example), the same model is kept for this function. 
#'@param  randomEffects default to c("Subject")
#'@param  lsMeansAlpha default to 0.10. Alpha for the posthoc tests.
#'@param  lsMeansAdjustment default to "Tukey"
#'@param  anovaCalculationMode default to "Ols". Can also be "ML" for maximum likelihood
#'@param  show default to c("Groups","StatMANOVA","F", "PValues","RMSE")
#'@param fileName default to NULL. In this case, the output file is called "ANOVA of scores". Otherwise, the output file is named as fileName.
#'@param varianceTest default to "None". Can also be "Bartlett" or "Fligner"
#'@param  normalityTest default to "None". Can also be "ShapiroWilks"or "KolmogorovSmirnov"
#'@param orderByF default to TRUE. Indicates whether the attributes should be ordered by decreasing F or not.
#'@return A list containing a table with ANOVA results and post hoc tests. The attributes are in rows and the products are in columns.
#'@export
#'@examples
#' #data(cheeses)
#' #profileAnova(cheeses)
profileAnova=function(profileObject,model="DefaultModel", randomEffects=c("Subject"), lsMeansAlpha=0.10, lsMeansAdjustment="Tukey", anovaCalculationMode="Ols", show=c("Groups","StatMANOVA","F", "PValues","RMSE"),fileName=NULL,varianceTest="None", normalityTest="None",orderByF=TRUE)
{	

	if(model=="DefaultModel"&!is.null(profileObject$ANOVAModel))
	{ 
		model=profileObject$ANOVAModel;
		lsMeansAlpha=profileObject$LsMeansAlpha;
		lsMeansAdjustment=profileObject$LsMeansAdjustment;
		anovaCalculationMode=profileObject$AnovaCalculationMode
		randomSubject=profileObject$RandomSubject
		randomSession=profileObject$RandomSession
		defaultAnova=profileObject$Anova
	} else
	{
		randomSubject="Subject" %in% randomEffects
		randomSession="Session" %in% randomEffects
		defaultAnova=NULL
	}
	
	if(is.null(fileName))
	{
		fileName="ANOVA of scores"
	}
	
	# Donn?es au format ?tendu
	extendedData=profileObject[["CompleteExtendedDataWithoutNA"]]
	

	res=AnovaTable(extendedData=extendedData, defaultAnova=defaultAnova, model=model, randomSubject=randomSubject, correlationStructure="", testRep="", lsMeansAlpha=lsMeansAlpha, lsMeansAdjustment=lsMeansAdjustment, anovaCalculationMode=anovaCalculationMode, show=show, fileName=fileName,varianceTest=varianceTest, normalityTest=normalityTest,orderByF=orderByF)$Table
	call=list(model=model, randomEffects=randomEffects, lsMeansAlpha=lsMeansAlpha, lsMeansAdjustment=lsMeansAdjustment, anovaCalculationMode=anovaCalculationMode, show=show,fileName=fileName,varianceTest=varianceTest, normalityTest=normalityTest,orderByF=orderByF)
	return(list(res=res[[1]],call=call))
}