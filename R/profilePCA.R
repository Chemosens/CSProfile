#' profilePCA
#'
#' Returns PCA.
#'
#' @param profileObject  profile object stemming from \link{profileReadData}
#' @param option default to "Covariance". Could also be "Correlation"
#' @param representation default to "DistanceBiplot". Could also be "twoMaps"
#' @param confInt default to 0.9. Threshold for ellipses
#' @param nbAxes default to "Auto". Number of axes. When default, the number of axis is defined by...
#' @param ellipsesType default to "Barycentric". Can also be "Individual"
#' @param productColors default to FALSE
#' @param ellipsesCalculation default to "Chi"
#' @param bootstrap default to FALSE: Warning, the ellipses are NOT obtained by bootstrap but with the covariance matrix of the individuals.
#' @param nSamples default to 100. If bootstrap is TRUE, number of bootstraps.
#' @param twoDiffFiles default to TRUE
#' @param itemsToPlot default to c("ProductEllipses","Panellists")
#' @param onlySignificantAttributesInMultivariateAnalysis default to TRUE
#' @param alphaForSelectionOfSignificantAttributes default to 0.05
#' @param sizeText number allowing to change the size of the text
#' @param axes default to 1:2
#' @param repel if TRUE, ggrepel is computed. FALSE by default
#' @param returnX default to FALSE. If the X axis should be reverted, can be TRUE
#' @param returnY default to FALSE. If the Y axis should be reverted, can be TRUE
#' @param expandBiplot default to 1.
#' @param colorAttributeForBiplot "black" by default
#'
#' @export
#' @importFrom CSUtils PCA turnToPCAgg plotPCAgg
#' @examples
#' data(cheeses)
#' profilePCA(cheeses)
#'
profilePCA=function(profileObject,axes=1:2,option="Covariance",representation="DistanceBiplot",confInt=0.9,repel=FALSE,nbAxes="Auto",ellipsesType="Barycentric",returnX=F,returnY=F,productColors=FALSE,ellipsesCalculation="Chi",bootstrap=FALSE,nSamples=100,twoDiffFiles=TRUE,itemsToPlot=c("ProductEllipses"),onlySignificantAttributesInMultivariateAnalysis=TRUE,alphaForSelectionOfSignificantAttributes=0.05,sizeText=NULL,expandBiplot=1,colorAttributeForBiplot="black")
{
	#if(option!="Covariance"&& representation=="DistanceBiplot"){stop(TS_GetLabel("[TS] Biplots should be run only with the covariance option. Please change either the mapping representation or the option"))}
	if(!profileObject$additionalCalculations)
	{
		if(length(profileObject$Replicates)==1){model="TwoWayAdditive"}else{model="TwoWayMultiplicative"}
	  profileObject=profileSetUnivariateAnalysisParameters(profileObject, model=model, randomEffects="Subject", anovaCalculationMode="Ols", lsMeansAdjustment="Tukey", lsMeansAlpha=.05)
	  profileObject=TS_SetProfileMultivariateAnalysisParameters(profileObject, onlySignificantAttributesInMultivariateAnalysis,alphaForSelectionOfSignificantAttributes)
	}

  title=paste("PCA ","of"," Score", sep="")
  indSup=c()

  if("ProductEllipses" %in% itemsToPlot)
 	{ #itemsToPlot=c("ProductEllipses","Panellists")

    indSup=c(indSup,"ell")

   	#	ellipsesType="None"
 	}
 	if("Panellists"%in%itemsToPlot)
 	{
 		indSup=c(indSup,"points")
 	}
#  else
# 	{
# 		panellists="None"
# 	}
# 	if("PanellistLabel"%in%itemsToPlot)
# 	{
# 		panellists="Labels"
# 	}
#  if("AttributeLabels"%in%itemsToPlot){variablesLabels=TRUE}else{variablesLabels=FALSE}
# 	if("ProductLabels"%in%itemsToPlot){text=TRUE}else{text=FALSE}

	revertX=returnX
	revertY=returnY

	extendedData=profileObject$CompleteExtendedDataWithoutNA
	removeNonSignificantAttributes=profileObject$OnlySignificantAttributesInMultivariateAnalysis
	att=c()
	# Selection eventuelle des attributs significatifs uniquement
	if(removeNonSignificantAttributes)
	{
		removedAttributes=c()
		for(attribute in levels(profileObject$Attributes))
		{
			if((profileObject$Anova[[attribute]][[1]]$PProd)>(profileObject$AlphaForSelectionOfSignificantAttributes))
			{
				extendedData=extendedData[,-which(colnames(extendedData)==attribute)]
				removedAttributes=c(removedAttributes,attribute)
			}
		  else
		  {
		    att=c(att,attribute)
		  }
		}

		if (length(removedAttributes)>0)
		{
			stop(paste("Removed Attributes in PCA",": ",paste(removedAttributes,collapse=", "),"</p>",sep=""))
			leftAttributes=length(profileObject$Attributes)-length(removedAttributes)
			if(leftAttributes<=3)
			{
				stop(" Not enough significant attributes in PCA.")
			}
		}
	}
  else{att=profileObject$Attributes}
	if (productColors==TRUE)
	{
		productColors=profileObject$ProductColors
	} else
	{
		productColors=NULL
	}

	colnames(extendedData)[which(colnames(extendedData)=="SubjectCode")]="subject"
	colnames(extendedData)[which(colnames(extendedData)=="ProductCode")]="product"
	colnames(extendedData)[which(colnames(extendedData)=="Replicate")]="rep"
	extendedData=extendedData[,c("product","subject","rep",att)]
	res.PCA=PCA(extendedData, option=option,representation=representation)
	pcag=turnToPCAgg(res.PCA,axes=list(axes))
	res.PlotPCA=plotPCAgg(pcag,type=representation,text=TRUE,n=10,colorInd="all",axes=axes,indSup=indSup,repel=repel,revertX=revertX,revertY=revertY,sizeText=sizeText,expandBiplot=expandBiplot,colorAttributeForBiplot=colorAttributeForBiplot)
	#res.PlotPCA=PlotPCA(res.PCA, panellists=panellists, representation=representation,  nbAxes=nbAxes, title=title, confInt=confInt, ellipsesType=ellipsesType, productColors=productColors, returnX=returnX, returnY=returnY, type = profileObject$DefaultGraphicalFormat, fileName =fileName,ellipsesCalculation=ellipsesCalculation,bootstrap=bootstrap,nSamples=nSamples,twoDiffFiles=twoDiffFiles,variablesLabels=variablesLabels,individualsLabels=individualLabels)

	# if ("IndividualCoordinates" %in% outputs)
	# {
	# 	write.table(cbind(rownames(res.PCA$IndivCoord),res.PCA$IndivCoord),paste("IndividualCoordinates",".csv",sep=""),sep=",",row.names=F)
	# }
	# if ("VariableCoordinates" %in% outputs)
	# {
	# 	write.table(cbind(rownames(res.PCA$VarCoord),res.PCA$VarCoord),paste("VariableCoordinates",".csv",sep=""),sep=",",row.names=F)
	# }
	#
	call=list(option=option,representation=representation,confInt=confInt,repel=repel,nbAxes=nbAxes,ellipsesType=ellipsesType,returnX=returnX,returnY=returnY,productColors=productColors,
	         ellipsesCalculation=ellipsesCalculation,bootstrap=bootstrap,nSamples=nSamples,twoDiffFiles=twoDiffFiles,itemsToPlot=itemsToPlot,
	         onlySignificantAttributesInMultivariateAnalysis=onlySignificantAttributesInMultivariateAnalysis,alphaForSelectionOfSignificantAttributes=alphaForSelectionOfSignificantAttributes,sizeText=sizeText,expandBiplot=expandBiplot,colorAttributeForBiplot=colorAttributeForBiplot)

	plot(res.PlotPCA)
	list.res=list(res.PCA,res.PlotPCA,call=call)
	return(list.res)

}