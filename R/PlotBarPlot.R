#'@importFrom graphics lines par text abline mtext barplot
#'@importFrom stats as.formula
#'@importFrom grDevices colorRampPalette rainbow
PlotBarPlot=function(extendedData, variable, minScore=NULL, maxScore=NULL, itemsToPlot=c("GMean","ErrorBars"),errorBars="ConfInt",statsToPlot=c("Means","Sd","N"), groupId="both", title="Mean scores by product",subtitle="",ylab="Score",resAnova=NULL,printLegend=TRUE,LSAlpha=0.05)
{	
	listRes=list()
	# gestion des éventuels paramètres manquants
	if(!exists("minScore") || is.null(minScore)) 
	{ 
		minScore=floor(min(extendedData[,variable],na.rm=TRUE))
	}
	if(!exists("maxScore") || is.null(maxScore) || is.na(maxScore)) 
	{ 
		maxScore=ceiling(max(extendedData[,variable],na.rm=TRUE))
	}
	if(!exists("resAnova") || is.null(resAnova)) 
	{ 
		groupId="None"
	}			
		
	# Calculs des moyennes des produits

	calculProductMeans=aggregate(extendedData[,variable],by=list(extendedData$ProductCode),FUN=mean,na.rm=T)
	productMeans=calculProductMeans[,"x"];names(productMeans)=calculProductMeans[,"Group.1"]
	productMeans=productMeans[order(productMeans)]
	productMeans=productMeans[!is.na(productMeans)]
	products=names(productMeans)[order(productMeans)]
	nbProd=length(products)
	listRes[["ProductMeans"]]=productMeans
	# mise en forme des groupes
	letterForGroup=function(group)
	{
		gg=strsplit(group,"")
		letter=""
		for (ggg in gg[[1]])
		{
			if (ggg!=" ")
			{
				letter=paste(letter,letters[as.numeric(ggg)],sep="")
			}
		}
		return(letter)
	}
	groups=NULL
	vecColor="gray"
	if (!is.null(resAnova))
	{
		LSAlpha=resAnova[[1]]$LSMeansAlpha
		productGroupsNum=resAnova[[1]]$MoyProd$.group;names(productGroupsNum)=resAnova[[1]]$MoyProd$ProductCode
		#productSE=resAnova[[1]]$MoyProd$SE
		productGroups=sapply(productGroupsNum,letterForGroup);names(productGroups)=resAnova[[1]]$MoyProd$ProductCode
	
		# Couleur des groupes
		tmpGrp=NULL
		for(i in 1:length(productGroups))
		{
			# On ne garde que la dernière lettre
			tmpGrp=c(tmpGrp,substring(productGroups[i],nchar(as.character(productGroups[i])),1000))
		}
		groups=unique(tmpGrp)
		nbGroups=length(groups)
		
		groupColors = rainbow(nbGroups)
		names(groupColors)=groups
		listRes[["ProductGroupsNum"]]=productGroupsNum
	}


	# Lettre des boites
	productLabels=products
	if (groupId %in% c("letters","ColorsAndLetters","Letters","both","Both") & length(groups)>1)
	{
		productLabels=paste(productLabels," (",productGroups,")",sep="")
	}
	
	if ("n" %in% statsToPlot || "N" %in% statsToPlot ||errorBars=="StdErr")
	{
		#calculN=aggregate((resAnova[[1]])$Data[,"Y"],by=list((resAnova[[1]])$Data[,"ProductCode"]),FUN="length")
		calculN=aggregate(extendedData[,variable],by=list(extendedData$ProductCode),FUN=LengthWithoutNA)
		names(calculN)=c("ProductCode","n")
		effectif=calculN[match(products,calculN[,"ProductCode"]),"n"]
		if("n" %in% statsToPlot || "N" %in% statsToPlot)
		{
			productLabels=paste(productLabels,"\n n=",round(effectif,2),sep="")
		}
	}

	
	# attention, ici, les ecarts types sont les mêmes pour tous les produits
	#calculStd=aggregate((resAnova[[1]])$Data[,"Y"],by=list((resAnova[[1]])$Data[,"ProductCode"]),FUN="sd")
	#productStd=calculStd[,"x"];names(productStd)=calculStd[,"Group.1"]
	
	# Calculs des écarts types  : option STD ou stdErr
	if(errorBars=="StdDev"||errorBars=="StdError"||"means" %in% statsToPlot||"Means" %in% statsToPlot)
	{
		calculProductStd=aggregate(extendedData[,variable],by=list(extendedData$ProductCode),FUN=sd,na.rm=T); names(calculProductStd)=c("ProductCode","std")
		productStd=calculProductStd[,"std"];names(productStd)=calculProductStd[,"ProductCode"]
		names(calculProductStd)=c("ProductCode","std")
		listRes[["ErrorBars"]]=productStd
	}
	# Calculs des intervalles de confiance "classiques
	if(errorBars=="StdError")
	{
		
		dataForCalculation=merge(calculN,calculProductStd)
		# if(is.null(LSAlpha))
		# { 
			# if(!is.null(resAnova[[1]]$LSMeansAlpha))
			# {
				# LSAlpha=resAnova[[1]]$LSMeansAlpha
			# }
			# else{stop("choose a LSMeansAlpha in your profileObject")	}
		# }
		tAlpha= qnorm(1-(LSAlpha/2))
		productStdError=tAlpha*dataForCalculation[,"std"]/sqrt(dataForCalculation[,"n"]);names(productStdError)=dataForCalculation [,"ProductCode"]
		listRes[["ProductStdError"]]=productStdError
	}
	if(nbProd>4)
	{
		par(mar=c(6,4,2,4))
	} else
	{
		par(mar=c(2,4,2,4))
	}
	if(printLegend)
	{
		par(oma=c(4,0,2,0))	
	} else{par(oma=c(0,0,2,0))	}
	# determination des bornes du graph à construire
	if ("ErrorBars" %in% itemsToPlot & errorBars=="StdDev")	{	maxScore=max(maxScore,max(productMeans,na.rm=TRUE)+max(productStd,na.rm=TRUE));minScore=min(minScore,min(productMeans,na.rm=TRUE)-max(productStd,na.rm=TRUE))	}
	if ("ErrorBars" %in% itemsToPlot & errorBars=="StdError")	{	maxScore=max(max(productMeans,na.rm=TRUE)+max(productStdError,na.rm=TRUE),maxScore);minScore=min(minScore,min(productMeans,na.rm=TRUE)-max(productStdError,na.rm=TRUE))	}	
	if ("ErrorBars" %in% itemsToPlot & errorBars=="ConfInt" & !is.null(resAnova))	{	maxScore=max(max(resAnova[[1]]$MoyProd[,"upper.CL"]+(maxScore-minScore)/10),maxScore,na.rm=T);minScore=min(min(resAnova[[1]]$MoyProd[,"lower.CL"]),minScore,na.rm=T)	}
	
	#coord=as.vector(barplot(productMeans,ylim=c(0,maxScore),horiz=F,names.arg=productLabels,ylab=ylab,width=1,space=0.5))
	if(nbProd>4){namesArg=c("")}else{namesArg=productLabels}
		coord=as.vector(barplot(productMeans,ylim=c(0,maxScore),horiz=F,names.arg=namesArg,ylab=ylab,width=1,space=0.5))
	names(coord)=products
		
	for (product in products)
	{
		m=productMeans[product]
		if (!is.null(resAnova))
		{
			lcl=resAnova[[1]]$MoyProd[resAnova[[1]]$MoyProd[,"ProductCode"]==product,"lower.CL"]
			ucl=resAnova[[1]]$MoyProd[resAnova[[1]]$MoyProd[,"ProductCode"]==product,"upper.CL"]
		}	
		
		y1=m
		y2=m

		# Couleur des boites
		if (groupId %in% c("colors","Both","both","Colors","ColorsAndLetters"))
		{
			productGroup=productGroups[[product]]
			indleft=1
			indright=1
			if (productGroup!="")
			{
				leftColor=groupColors[[substring(productGroup,1,1)]]
				indleft=which(groupColors==leftColor)[[1]]
				rightColor=groupColors[[substring(productGroup,nchar(as.character(productGroup)),1000)]]
				indright=which(groupColors==rightColor)[[1]]
				if(nchar(productGroup)%in% c(2,3))
				{
					if(nchar(productGroup)==2)
					{
					vecColor=colorRampPalette(c(leftColor,rightColor))(100)
					}
					else
					{
					middleColor=groupColors[[substring(productGroup,2,2)]]
					vecColor=colorRampPalette(c(leftColor,middleColor,rightColor))(100)
					}
				} else
				{	
					vecColor=groupColors[indleft:indright]		
				}
			}
			else{vecColor=groupColors[1]}

				
		}
		PlotMultiColorRectangle(coord[[product]]-coord[[1]]/2,0,coord[[product]]+coord[[1]]/2,productMeans[[product]],vecColor)		
				
		if ("ErrorBars" %in% itemsToPlot)
		{
			if (errorBars=="StdDev")
			{
				# Tracé des écarts-types
				stdDev=productStd[product]
				y1=m+stdDev
				y2=m-stdDev
			}
			if (errorBars=="StdError")
			{
				# Tracé des erreurs standard
				stdError=productStdError[product]
				y1=m+stdError
				y2=m-stdError
			}
			if (errorBars=="ConfInt" & !is.null(resAnova))
			{
				# Tracé des intervalles de confiance
				y1=ucl
				y2=lcl
			}
			listRes[[product]]=c(y1,y2)
			lines(rep(coord[[product]],2),c(y1,y2))
			lines(c(coord[[product]]-0.1,coord[[product]]+0.1),rep(y1,2))
			lines(c(coord[[product]]-0.1,coord[[product]]+0.1),rep(y2,2))
		}
		
		# Affichage des moyennes par produit
		if ("means" %in% statsToPlot||"Means" %in% statsToPlot)
		{
			txt=paste(round(productMeans[[product]],2),"+-",round(productStd[[product]],2), sep="")
			text(coord[[product]],y1,txt,pos=3,cex=0.8)
		}
		
	}
	
	# Tracé des noms des produits en dessous des boxes
	if(nbProd>4)
	{
		text(coord, par("usr")[3]-0.2, labels = productLabels, srt = 45, adj = c(1,1), xpd = TRUE, cex=1.1)
	}
	# else
	# {
		# text(coord, par("usr")[3]-0.2, labels = productLabels, srt = 0, adj = c(1,1), xpd = TRUE, cex=1.1)
	# }
	# Tracé de la ligne de moyenne
	if ("GMean" %in% itemsToPlot)
	{
		GMean=mean(extendedData[,variable],na.rm=TRUE)
		#GMean=mean(resAnova[[1]]$Data[,"Y"],na.rm=TRUE)
		abline(h=GMean,col="blue")
		#text(tail(coord,1)+coord[1]/2,GMean,round(GMean,2),col="blue",pos=3)
		text(coord[1]/2,GMean,round(GMean,2),col="blue",pos=3)
	}
			
	#par(oma=c(2,2,2,1))
	mtext(title,3,line=2,cex=1.8,font=2)
	
	if(printLegend)
	{
		if ("ErrorBars" %in% itemsToPlot)
		{
			mtext(paste("Error bars",": ",errorBars,sep=""),1,line=2,outer=T,cex=1,font=3)
		}
		if (!is.null(resAnova))
		{
			mtext(paste("Model",": ", resAnova[[1]]$Model),1,line=1,outer=T,cex=1,font=3)
			mtext(paste("FProd=",round(resAnova[[1]]$FProd,2),", p=",FriendlyPValue(resAnova[[1]]$PProd),sep=""),1,line=3,outer=T,cex=1,font=3,col="black")
			mtext(paste("PostHocTest",": ",resAnova[[1]]$LSMeansAdjustment,", alpha=",resAnova[[1]]$LSMeansAlpha,sep=""),1,line=4,outer=T,cex=1,font=3)
		}
	}
	legend=NULL
	if(groupId %in% c("colors","Colors"))
	{
		legend=paste(legend, "GroupMeansColor",sep="")
	}
	if(groupId %in% c("both","ColorsAndLetters"))
	{
		legend=paste(legend, "GroupMeansOrColor",sep="")
	}
	if(groupId %in% c("letters","Letters"))
	{
		legend=paste(legend, "GroupMeans",sep="")
	}
	listRes[["Coord"]]=coord
	if(printLegend)
	{
	mtext(legend,1,line=5,outer=T,cex=1,font=3)
	}
	return(listRes)
}	
