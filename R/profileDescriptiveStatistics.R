#'profileDescriptiveStatistics
#'
#'Returns some descriptive statistics (min, max, med...)
#'@param profileObject profile object from \link{profileReadData}
#'@param by "AttributeCode" by default. Could also be "ProductCode".
#'@export
#'@examples
#' data(cheeses)
#' profileDescriptiveStatistics(cheeses)
profileDescriptiveStatistics=function(profileObject,by="AttributeCode")
{

	fileName="Descriptive statistics"

	title=paste("Data summary"," ", "By", " ", by,sep="")
	extendedData=profileObject$CompleteExtendedDataWithoutNA
	html=ExtendedDataHtmlSummary(extendedData=extendedData,by=by,title=title,fileName=fileName)
	html[,c("N","Min","Max","Mean","Med","Q1","Q3","Sd")]=as.numeric(html[,c("N","Min","Max","Mean","Med","Q1","Q3","Sd")])
	return(html)
}