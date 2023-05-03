#' profileBoxPlot
#'
#' Returns the boxplot of descriptors per products with ANOVA and tests posthoc results
#'@param profileObject profile object from \link{profileReadData}
#'@param variable descriptor to be plotted
#'@param overallBoxplot default to TRUE. Plot the overall variable (all products confounded)
#'@param plotMeans default to TRUE. Should the means be plotted with crosses ?
#'@param plotStats default to TRUE. Should the stats be displayed
#'@param groupId default to "both". Can also be "letters" or "colors".
#'@export
#'@importFrom stats aggregate sd
profileBoxPlot=function(profileObject,variable, overallBoxplot=TRUE, plotMeans=TRUE, plotStats=TRUE, groupId="both")
{
  variable=as.character(variable)
	dataFrame=profileObject$CompleteExtendedDataWithoutNA[,c("SubjectCode","Replicate","ProductCode",variable)]
	colnames(dataFrame)[4]="Y"
	resAnova=profileObject$Anova[[variable]]
	fun=call("PlotBoxPlot", dataFrame=dataFrame,resAnova=resAnova,minScore=profileObject$MinScore, maxScore=profileObject$MaxScore, overallBoxplot=overallBoxplot, plotMeans=plotMeans, plotStats=plotStats, groupId=groupId,main=paste0("Boxplot: ",variable),subtitle="",xlab="Product",ylab=variable)
	res=GenericPlot(type="R",fileName="",filewidth=9,fileheight=7,CALLFUN=fun)
	return(res)
}

