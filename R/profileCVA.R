#' CVA
#' 
#' Return Canonical Variate Analysis
#'@param profileObject profile object from \link{profileReadData}
#'@param test default to "Hotelling-Lawley". 
#'@param option default to "TwoWayANOVA". Can also be "OneWayANOVA","TwoWayANOVA","MAM" or "MultiMAM".
#'@param itemsToPlot default to c("ProductEllipses","ProductSegments","Panellists")
#'@param representation default to "distanceBiplot"
#'@param axes default to 1:2
#'@param nbAxes default to "Auto",
#'@param outputs default to NULL.
#'@param confInt default to 0.9.
#'@param ellipsesType default to "barycentric". can also be "individual".
#'@param returnX default to FALSE. If the X axis should be reverted, can be TRUE 
#'@param returnY default to FALSE. If the Y axis should be reverted, can be TRUE 
#'@param productColors default to FALSE
#'@param fileName default to NULL
#'@param ellipsesCalculation default to "Chi"
#'@param onlySignificantAttributesInMultivariateAnalysis default to TRUE. If FALSE, all attributes are considered.
#'@param alphaForSelectionOfSignificantAttributes default to 0.05. Significance threshold for the product effect for each attribute
#' @importFrom CSUtils CVA turnToCVAgg plotCVAgg
#' @return{
#' a list containing CVA results 
#'\itemize{
#'\item{IndivCoord }{coordinates of the products}
#'\item{VarCoord }{coordinates of the attributes}
#'\item{NbDimSig }{number of significant dimensions}
#'\item{HotellingTable }{table containing the results of T2 Hotelling tests for all product pairs}
#'\item{ConditioningOfW }{condition index of W. If ConditioningOfW >30, the matrix is ill-conditionned and CVA can be unstable}
#'\item{B }{covariance matrix of product effect}
#'\item{W}{covariance matrix of interaction}
#'\item{EigenVectors}{W-orthonormalized eigenvectors of W^(-1)B}
#'\item{EigenValues}{eigenvalues of W^(-1)B}
#'\item{Stats}{list containing the MANOVA statistics (statistics, approximated F and pvalue)}
#'\item{decomposition}{table with the decomposition of each score according to the different effects}
#'\item{CenteredProductSubjectTable}{centered product by subject table}
#'\item{nbAxes}{number of axes, useful if nbAxes="auto" was chosen}
#'\item{wDemi}{matrix of weigths used in the biplot}
#'\item{option}{returns the chosen model}
#'\item{representation}{if "biplot", the plot related to this CVA is a biplot. If else, it will be two graphs: a product map and an attribute map}
#'}
#'
#' }
#'@export
#'@examples
#' data(cheeses)
#' profileCVA(cheeses)
profileCVA=function(profileObject,axes=1:2, test="Hotelling-Lawley",option="TwoWayANOVA", itemsToPlot=c("ProductEllipses","ProductSegments"), representation="distanceBiplot", nbAxes="Auto", outputs=NULL,confInt=0.9,ellipsesType="barycentric",returnX=F,returnY=F,productColors=FALSE,fileName=NULL,ellipsesCalculation="Chi",onlySignificantAttributesInMultivariateAnalysis=TRUE,alphaForSelectionOfSignificantAttributes=0.05)
{
  
  if(!profileObject$additionalCalculations)
  {
    if(length(profileObject$Replicates)==1){model="TwoWayAdditive"}else{model="TwoWayMultiplicative"}
    profileObject=profileSetUnivariateAnalysisParameters(profileObject, model=model, randomEffects="Subject", anovaCalculationMode="Ols", lsMeansAdjustment="Tukey", lsMeansAlpha=.05)
    profileObject=TS_SetProfileMultivariateAnalysisParameters(profileObject, onlySignificantAttributesInMultivariateAnalysis,alphaForSelectionOfSignificantAttributes)
  }
  indSup=c()
   if("ProductEllipses" %in% itemsToPlot)
 	{ 
 		indSup=c(indSup,"ell")
 	}
	if("Panellists"%in%itemsToPlot)
	{
	  indSup=c(indSup,"points")
	}
	# if("PanellistLabel"%in%itemsToPlot)
	# {	
	# 	panellists="Labels"
	# }
	if("ProductSegments"%in%itemsToPlot)
	{
		linkBetweenProducts=TRUE
	} else
	{
		linkBetweenProducts=FALSE
	}	
	 if("AttributeLabels"%in%itemsToPlot){variablesLabels=TRUE}else{variablesLabels=FALSE}
	if("ProductLabels"%in%itemsToPlot){individualLabels=TRUE}else{individualLabels=FALSE}
		
	if(!profileObject$IsComplete)
	{
		stop("Complete dataset required")
	}

	if (productColors==TRUE)
	{
		productColors=profileObject$ProductColors
	} else
	{
		productColors=NULL
	}
	

	
	#title=paste("CVA of scores", sep="")
	
	if(is.null(fileName))
	{
		fileName=title
	}
	
	extendedData=profileObject$CompleteExtendedDataWithoutNA
	if(profileObject$OnlySignificantAttributesInMultivariateAnalysis)
	{
		removedAttributes=att=c()
		for(attribute in levels(profileObject$Attributes))
		{
			if((profileObject$Anova[[attribute]][[1]]$PProd)>(profileObject$AlphaForSelectionOfSignificantAttributes))
			{
				extendedData=extendedData[,-which(colnames(extendedData)==attribute)]
				removedAttributes=c(removedAttributes,attribute)
			}
		  else{att=c(att,attribute)}
		}	
		
		if (length(removedAttributes)>0)
		{
			warning(paste("<p>","Removed Attributes in CVA",": ",paste(removedAttributes,collapse=", "),"</p>",sep=""))
			leftAttributes=length(profileObject$Attributes)-length(removedAttributes)
			if(leftAttributes<2)
			{
				stop("Not enough significant attributes in CVA.")		
			}
		}
	}else{att=profileObject$Attributes}
	colnames(extendedData)[which(colnames(extendedData)=="SubjectCode")]="subject"
	colnames(extendedData)[which(colnames(extendedData)=="ProductCode")]="product"
	colnames(extendedData)[which(colnames(extendedData)=="Replicate")]="rep"
	extendedData=extendedData[,c("subject","product","rep",att)]
	res.CVA=CVA(extendedData=extendedData, test=test,nbDimHotelling=NULL,option=option,representation=representation)
	cvag=turnToCVAgg(res.CVA,axes=list(axes))
	res.PlotCVA=plotCVAgg(cvag,type=representation,revertX=returnX,revertY=returnY,segmentsHotelling=linkBetweenProducts,indSup=indSup)
	#plotCVAgg=function(respcagg,type="ind",text=TRUE,n=10,colorInd="all",substrVec=c(1,2),axes=c(1,2),indSup=c("ell"),repel=FALSE,revertX=FALSE,revertY=FALSE,sizeText=NULL,segmentsHotelling=TRUE)
	# if(test != "None")
	# {
	# 	 MultidimensionalDifferencesTable(res.CVA$HotellingTable, confInt,output="Hotelling table")
	# }
	# 
	# if ("IndividualCoordinates" %in% outputs)
	# {
	# 	write.table(cbind(rownames(res.CVA$IndivCoord),res.CVA$IndivCoord),paste("IndividualCoordinates",".csv",sep=""),sep=",",row.names=F)
	# }
	# if ("VariableCoordinates" %in% outputs)
	# {
	# 	write.table(cbind(rownames(res.CVA$VarCoord),res.CVA$VarCoord),paste("VariableCoordinates",".csv",sep=""),sep=",",row.names=F)
	# }
	call=list(test=test,option=option, itemsToPlot=itemsToPlot, representation=representation, nbAxes=nbAxes,confInt=confInt,ellipsesType=ellipsesType,returnX=returnX,returnY=returnY,productColors=productColors,ellipsesCalculation=ellipsesCalculation,onlySignificantAttributesInMultivariateAnalysis=onlySignificantAttributesInMultivariateAnalysis,alphaForSelectionOfSignificantAttributes=alphaForSelectionOfSignificantAttributes)
	res=list(resCVA=res.CVA,gg=res.PlotCVA,call=call)
	plot(res.PlotCVA)
	return(res)
}
