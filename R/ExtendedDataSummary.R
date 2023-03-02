#' @importFrom stats quantile
ExtendedDataSummary = function(extendedData)
{
	# Calcul par Produit (autres possibilit?s Produit+Rep, Produit + Rep + Session)
	aggregateBy=list(ProductCode = extendedData$ProductCode)
	colNames=colnames(extendedData)[-c(1:4)]
		
	statN=aggregate(extendedData[,-c(1:4)], aggregateBy, LengthWithoutNA)
	statMin=aggregate(extendedData[,-c(1:4)], aggregateBy, min,na.rm=TRUE)
	statMax=aggregate(extendedData[,-c(1:4)], aggregateBy, max,na.rm=TRUE)
	statMean=aggregate(extendedData[,-c(1:4)], aggregateBy, mean,na.rm=TRUE)
	statMed=aggregate(extendedData[,-c(1:4)], aggregateBy, median,na.rm=TRUE)
	statQ1=aggregate(extendedData[,-c(1:4)], aggregateBy, quantile, probs=c(0.25),na.rm=TRUE)
	statQ3=aggregate(extendedData[,-c(1:4)], aggregateBy, quantile, probs=c(0.75),na.rm=TRUE)
	statSd=aggregate(extendedData[,-c(1:4)], aggregateBy, sd, na.rm=TRUE)
	
	statMin[,-1]=round(statMin[,-1],3)
	statMax[,-1]=round(statMax[,-1],3)
	statMean[,-1]=round(statMean[,-1],3)
	statMed[,-1]=round(statMed[,-1],3)
	statQ1[,-1]=round(statQ1[,-1],3)
	statQ3[,-1]=round(statQ3[,-1],3)
	statSd[,-1]=round(statSd[,-1],3)
	#statRange=statMax[,-c(1:4)]-statMin[,-c(1:4)]
	#cv
	#conf.int
	colnames(statMin)=c("ProductCode",colNames)
	colnames(statMax)=c("ProductCode",colNames)
	colnames(statMean)=c("ProductCode",colNames)
	colnames(statMed)=c("ProductCode",colNames)
	colnames(statQ1)=c("ProductCode",colNames)
	colnames(statQ3)=c("ProductCode",colNames)
	colnames(statSd)=c("ProductCode",colNames)
	colnames(statN)=c("ProductCode",colNames)
	
	return (list(N=statN, Min=statMin, Max=statMax, Mean=statMean, Med=statMed, Q1=statQ1, Q3=statQ3, Sd=statSd))
}