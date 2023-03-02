#'profileRadarPlot
#'
#'Returns the radarplot for each descriptors
#'@param profileObject profile object stemming from \link{profileReadData}
#'@param differentLines default to "ProductCode"
#'@param colors colors for the lines to be plotted
#'@export
#'@examples 
#'data(cheeses)
#'profileRadarPlot(cheeses)
#'@importFrom stats reshape
#'@importFrom grDevices rainbow
profileRadarPlot=function(profileObject,differentLines="ProductCode",colors=NULL)
{
	res=list()
	minScale=profileObject$MinScore
	maxScale=profileObject$MaxScore
	# if (is.null(fileName))
	# {
	# 	fileName="Radar Plot"
	# }

	if(differentLines=="ProductCode")
	{
	  if(is.null(colors))
	  {
	    colors=rainbow(length(profileObject$Products))
	    names(colors)=profileObject$Products
	  }
		fun=call("PlotRadarPlot",extendedData=profileObject$CompleteExtendedDataWithoutNA[,-c(1:4)],productVector=as.character(profileObject$CompleteExtendedDataWithoutNA[,"ProductCode"]), productColors=colors, mainTitle="Products Radar Plot",titleLegend="Product", maxScale=maxScale, minScale=minScale, statistic="mean",resAnova=profileObject$Anova)	
	}
	if(differentLines=="AttributeCode")
	{
	  if(is.null(colors))
	   {
	    colors=rainbow(length(profileObject$Attributes))
	    names(colors)=profileObject$Attributes
	   }
	dataToReshape=profileObject$CompleteCanonicalDataWithoutNA	
	reshapedData=reshape(dataToReshape,direction="wide",timevar="ProductCode",idvar=c("Session","SubjectCode","AttributeCode","Replicate"))
	colnames(reshapedData)[5:dim(reshapedData)[2]]=substring(colnames(reshapedData)[5:dim(reshapedData)[2]],first=7)
	fun=call("PlotRadarPlot",extendedData=reshapedData[,-c(1:4)],productVector=as.character(reshapedData[,"AttributeCode"]), productColors=colors, mainTitle="Radar Plot",titleLegend="Attribute", maxScale=maxScale, minScale=minScale, statistic="mean",resAnova=NULL)	
	}
	res=GenericPlot(type="R",fileName="",filewidth=9,fileheight=7,CALLFUN=fun)	
	return(res)
}	