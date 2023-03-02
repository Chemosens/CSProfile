#'@export
profileHistogramOfDistributions = function(profileObject, highlightedSubjectCode=NULL, footnote=c("m","sd","n","ShapiroWilksTest"), normalDistribution=TRUE, by="Attribute", nbGraphicsByColumn=2, fileName=NULL)
{
	fileNameIni=fileName
	fun=NULL
	res=NULL
	nbLines=0
	listAttributes=profileObject$Attributes
	listProducts=profileObject$Products
	nbAttributes=length(listAttributes)
	nbProducts=length(listProducts)
	
	minScore=profileObject$MinScore
	maxScore=profileObject$MaxScore
	
	xlim=NULL
	
	dataMin=min(profileObject$CompleteExtendedDataWithoutNA[,-c(1:4)],na.rm=TRUE)
		
	if (is.null(minScore) || minScore>dataMin)
	{
		minScore=dataMin
	}
	
	dataMax=max(profileObject$CompleteExtendedDataWithoutNA[,-c(1:4)],na.rm=TRUE)
	if (is.null(maxScore) || maxScore<dataMax)
	{
		maxScore=dataMax
	}
	
	if (!is.null(minScore) & !is.null(maxScore))
	{
		xlim=c(minScore,maxScore)
	}
	
	if (by=="Product")
	{
		nbLines=ceiling(nbAttributes/nbGraphicsByColumn)
	}
	if (by=="Attribute")
	{
		nbLines=ceiling(nbProducts/nbGraphicsByColumn)
	}
	
	subtitle=NULL
	if (!is.null(highlightedSubjectCode))
	{
		subtitle="Green Bar Represents Your Score "
	}
	
	if (nbLines > 0)
	{		
		fun=c(fun,call("split.screen",c(min(nbLines,3),nbGraphicsByColumn)))
	}
		
	if (by=="None")
	{		
		res=list()
		for (attribute in listAttributes)
		{	
			res[[attribute]]=list()
			for (product in listProducts)
			{
				fun=call("PlotHistogramOfDistributions",extendedData=profileObject$CompleteExtendedDataWithoutNA,productCode=product,variable=attribute,highlightedSubjectCode=highlightedSubjectCode,normalDistribution=normalDistribution,xlim=xlim,footnote=footnote)
				if(is.null(fileName))
				{
					fileName=paste("Histogram of distributions of"," ",attribute," x ",product,sep="")
				} else 
				{
					fileName=paste("Histogram of distributions of"," ",fileNameIni,"",attribute," x ",product,sep="")
				}
				fun=c(fun, call("title", sub = subtitle, cex.main=2, cex.sub=1.5, font=2, outer=T))
				res[[attribute]][[product]]=GenericPlot(type="R",fileName=fileName,filewidth=9,fileheight=7,CALLFUN=fun)	
			}	
		}
	}
	if (by=="Attribute")
	{
		productInWindow=list()
		nbWindows=1+nbLines%/%3  # si on a trop de graphique pour tenir sur une fenetre (3 par lignes)
		if(nbLines%%3==0)
		{
			nbWindows=nbWindows-1
		}
		numberOfWindows=rep("",nbWindows)
		for(w in 1:nbWindows)
		{
			fullWindow=length(listProducts)
			if(w!=nbWindows) # si la fenetre est remplie
			{
				productInWindow[[w]]=listProducts[w*(1:(3*nbGraphicsByColumn))]
				if(nbWindows!=1)
				{
					numberOfWindows[w]=paste("(",w,")",sep="")
				}
			}
			else # pour la derniere fenetre ? remplir
			{	
				productInWindow[[w]]=listProducts[(3*nbGraphicsByColumn*(w-1)+1):length(listProducts)]
				if(nbWindows!=1)
				{
					numberOfWindows[w]=paste("(",w,")",sep="")
				}
			}	
		}

		res=list()
		for (attribute in listAttributes)
		{	
			for( w in 1:nbWindows)
			{
				i=1
				fun1=fun
				for (product in productInWindow[[w]])
				{					
					main=product
					fun1=c(fun1, call("screen", i))
					fun2=call("PlotHistogramOfDistributions",extendedData=profileObject$CompleteExtendedDataWithoutNA,productCode=product,variable=attribute,highlightedSubjectCode=highlightedSubjectCode,main=main,normalDistribution=normalDistribution,xlim=xlim,footnote=footnote)
					fun1=c(fun1,fun2)	
					i=i+1				
					title=paste("Distribution of variable",": ",attribute,sep="")
				}
				if(is.null(fileName))
				{
					fileName=paste("Histogram of distributions of"," ", attribute,numberOfWindows[w],sep="")
				} else 
				{
					fileName=paste("Histogram of distributions of"," ", fileNameIni,"", attribute,numberOfWindows[w],sep="")
				}	
				#TODO : gestion erreur				
				tryCatch({ 
					fun1=c(fun1, call("title", main = title, sub = subtitle, cex.main=2, cex.sub=1.5, font=2, outer=T))
					GenericPlot(type=profileObject$DefaultGraphicalFormat,fileName=fileName,filewidth=9,fileheight=7,CALLFUN=fun1)	
					graphics.off()
				},error = function(e) {})			
			}
		}
	}
	if (by=="Product")
	{
		res=list()
		for (product in listProducts)
		{
			i=1
			fun1=fun
			for (attribute in listAttributes)
			{
				main=paste(TS_GetLabel("Distribution"),": ",attribute,sep="")
				fun1=c(fun1,call("screen", i))
				fun1=c(fun1,call("PlotHistogramOfDistributions",extendedData=profileObject$CompleteExtendedDataWithoutNA,productCode=product,variable=attribute,highlightedSubjectCode=highlightedSubjectCode,main=main,normalDistribution=normalDistribution,xlim=xlim,footnote=footnote))
				i=i+1
			}
			title=product
			if(is.null(fileName))
			{
				fileName=paste("Histogram of distributions of"," ",product,sep="")
			} else
			{
				fileName=paste("Histogram of distributions of"," ", fileNameIni," ",product,sep="")
			}
			res=c(res,fileName)
			#TODO : gestion erreur
			tryCatch({ 
				fun1=c(fun1, call("title", main = title, sub = subtitle, cex.main=2, cex.sub=1.5, font=2, outer=T))
				res[[product]]=GenericPlot(type=profileObject$DefaultGraphicalFormat,fileName=fileName,filewidth=9,fileheight=7,CALLFUN=fun1)
			},error = function(e) {})	
				graphics.off()
		}
	}
	return (res)
}