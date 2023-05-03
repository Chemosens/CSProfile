#'profileBarPlot
#'
#'Returns barplots with ANOVA results and post-hoc tests (if selected in profileObject)
#'@param profileObject  profile object stemming from \link{profileReadData}
#'@param  itemsToPlot default to c("GMean","ErrorBars")
#'@param variable default to first attribute. Attribute to be considered.
#'@param errorBars default to "ConfInt". Can also be "StdDev" or "StdErr"
#'@param statsToPlot default to c("Means","Sd","N"). A subset of these stats is also available
#'@param groupId default to "Both". Can also be "letters" or "colors".
#'@export
#'@examples
#' data(cheeses)
#' profileBarPlot(profileObject=cheeses)
#'@importFrom stats aggregate sd
profileBarPlot=function(profileObject,variable=profileObject$Attributes[[1]], itemsToPlot=c("GMean","ErrorBars"),errorBars="ConfInt",statsToPlot=c("Means","Sd","N"),groupId="Both")
{
  variable=as.character(variable)
	fileNameIni=fileName=NULL
	resPlotBarPlot=list()
	# for (variable in profileObject$Attributes)
	# {
		if(is.null(fileName))
		{
			fileName=paste("BarPlot of", " ", variable,sep="")
		} else
		{
			fileName=paste("BarPlot of", " ", fileNameIni,variable,sep="")
		}
		title=paste("Mean scores of", " ", variable, " ", "by product",sep="")
		fun=call("PlotBarPlot",extendedData=profileObject$CompleteExtendedDataWithoutNA, variable=variable,minScore=profileObject$MinScore, maxScore=profileObject$MaxScore, itemsToPlot=itemsToPlot,errorBars=errorBars,statsToPlot=statsToPlot, groupId=groupId, title=title,subtitle="",resAnova=profileObject$Anova[[variable]])
		resPlotBarPlot=		GenericPlot(type="R",filewidth=9,fileheight=7,CALLFUN=fun)

#	}
	return(resPlotBarPlot)
}