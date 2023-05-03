#'@import fmsb
#'@importFrom stats aggregate median
#'@importFrom graphics layout plot.new
PlotRadarPlot = function(extendedData, productVector, productColors, mainTitle="",titleLegend="", maxScale, minScale, statistic="mean",resAnova=NULL,legend=TRUE,printLimit=FALSE)
{
	# 3 attributs n?cessaires pour trac? un radarplot

	productVector=as.character(productVector)

	# Donn?es aggr?g?es par produit ou attribut
	selectedData=extendedData
	selectedIndividuals=list(productVector)
	# if (dim(extendedData)[1]<3)
	# {
		# stop(TS_GetLabel("InsufficientNumberOfAttributes"))
	# }

	# Choix de la statistique
	if(statistic=="mean")
	{
		aggregatedData=aggregate(selectedData,selectedIndividuals , mean,na.rm=TRUE)
	}
	 if (statistic == "median")
	 {
		 aggregatedData=aggregate(selectedData,selectedIndividuals , median,na.rm=TRUE)
	 }
	# if (statistic == "range")
	# {
		# aggregatedData=aggregate(data[,-c(1:4)], list(ProductCode = data$ProductCode), mean,na.rm=TRUE)
	# }
	rownames(aggregatedData)=aggregatedData[,1]
	aggregatedData=aggregatedData[,-1]
	colnamesAggregatedData=colnames(aggregatedData)
	if(!is.null(resAnova))
	{
		for(k in 1:length(colnamesAggregatedData))
		{

			statF=round(resAnova[[colnamesAggregatedData[k]]][[1]]$FProd,digits=2)
			stars=resAnova[[colnamesAggregatedData[k]]][[1]]$DiagProd
			colnamesAggregatedData[k]=paste(colnamesAggregatedData[k],"\n (F=",statF,stars,")",sep="")
		}
	}
	colnames(aggregatedData)=colnamesAggregatedData
	nCols=ncol(aggregatedData)
	productNames=rownames(aggregatedData)
	# Min et max de l'?chelle
	if (exists("minScale") && exists("maxScale") && is.numeric(minScale) && is.numeric(maxScale) && minScale < maxScale)
	{
		minScale=rep(minScale,nCols)
		maxScale=rep(maxScale,nCols)
		aggregatedData=rbind(minScale,aggregatedData)
		rownames(aggregatedData)[1]="min"
		aggregatedData=rbind(maxScale,aggregatedData)
		rownames(aggregatedData)[1]="max"

		maxmin=TRUE
	} else
	{
		maxmin=FALSE
	}
	# Trac? du graphique
#	LoadPackage("fmsb")
	if(legend)
	{
		mat=matrix(1,nrow=1,ncol=4)
		mat[,4]=2
		layout(mat)
	}
	productColors2=productColors[productNames]
	radarchart(aggregatedData[,ncol(aggregatedData):1], maxmin=maxmin, axistype=0,axislabcol="grey", seg=4, plty=1, pty=32, plwd=1, cglcol="black", pcol=productColors2, title=mainTitle)
	if(printLimit)
	{
		text(0,1,maxScale,col="light grey")
		text(0,0,minScale,col="light grey")
	}
    if(legend)
	{
		par(mar=c(5,0,4,1))
		par(cex=0.9)
		plot.new()
		legend(0,1.04,names(productColors[productNames]),col=productColors[productNames],lwd=2,inset=0.01, title=titleLegend)
	}
	return(aggregatedData)
  #  graphics.off()
}