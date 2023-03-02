PlotHistogramOfDistributions=function(extendedData,productCode,variable,highlightedSubjectCode=NULL,main=NULL,footnote=c("m","sd","n","ShapiroWilksTest"),xlab=NULL,ylab=NULL,xlim=NULL,ylim=NULL,normalDistribution=TRUE)
{
	# Calcul des breaks
	x=(extendedData[extendedData$ProductCode==productCode,variable])
	
	# Titre
	if (is.null(main))
	{
		main=paste("Distribution of variable"," ",variable,sep="")
	}
	
	if (is.null(xlab))
	{
		xlab="Score"
	}

	if (is.null(ylab))
	{
		ylab="Density"
	}
	
	# Calcul de xlim et ylim
	if (is.null(xlim))
	{
		xlim=c(floor(min(x,na.rm=TRUE)),ceiling(max(x,na.rm=TRUE)))
		
	}
	

	# Sous-titre
	sub=""
	if("m" %in% footnote || "M" %in% footnote)
	{
		sub=paste(sub,"m=",round(mean(x,na.rm=T),2),sep="")
	}
	if("sd" %in% footnote || "Sd" %in% footnote)
	{
		if("m" %in% footnote)
		{
			#sub=paste(sub,"\U00b1 ",round(sd(x,na.rm=T),2),sep="")
			sub=paste(sub,"+-",round(sd(x,na.rm=T),2),sep="")
		} else
		{
			sub=paste(sub,"sd=",round(sd(x,na.rm=T),2),sep="")
		}
	}
	if("n" %in% footnote || "N" %in% footnote)
	{
		if (nchar(sub)>0)
		{
			sub=paste(sub,", ",sep="")
		}
		sub=paste(sub,"n=",length(x),sep="")
	}
	if("ShapiroWilksTest" %in% footnote)
	{
		if (nchar(sub)>0)
		{
			sub=paste(sub,"\n",sep="")
		}
		if(sd(x)!=0)
		{
		res.shapiro.test=shapiro.test(x)
		sub=paste(sub,"Normality of distribution"," ",sep="")
		if (res.shapiro.test$p.value>0.1)
		{
			sub=paste(sub," accepted",sep="")
		} else
		{
			sub=paste(sub," rejected",sep="")
		}
		sub=paste(sub," (","Shapiro-Wilk test p-value","=",FriendlyPValue(res.shapiro.test$p.value),")",sep="")
		}
		else
		{
			sub=paste(sub,"No variability in the distributions")
		}
	}
	if("KolmogorovSmirnovTest" %in% footnote)
	{
		if (nchar(sub)>0)
		{
			sub=paste(sub,"\n",sep="")
		}
		res.ks.test=ks.test(x,"pnorm")
		sub=paste(sub,"Normality of distribution"," ",sep="")
		if (res.ks.test$p.value>0.1)
		{
			sub=paste(sub," accepted",sep="")
		} else
		{
			sub=paste(sub," rejected",sep="")
		}
		sub=paste(sub," (","Kolmogorov-Smirnov test p-value","=",FriendlyPValue(res.ks.test$p.value),")",sep="")
	}
	
	 if ((xlim[2]-xlim[1])<=11)
	 {
		 breaks=seq(from=floor(xlim[1]),to=ceiling(xlim[2]),by=1)
		 if((xlim[2]-xlim[1])<2)
		 {
		  breaks=seq(from=xlim[1],to=ceiling(xlim[2]),by=0.1)
		 }
	 } else
	 {
		 breaks=seq(from=xlim[1],to=xlim[2],length.out=10)
	 }
	
	round2 = function(x, n) {
	  posneg = sign(x)
	  z = abs(x)*10^n
	  z = z + 0.5
	  z = trunc(z)
	  z = z/10^n
	  z*posneg
	}
	
	res.hist=hist(x,breaks=breaks,plot=FALSE,freq=FALSE)
		
	colors=rep("lightblue",length(breaks))
	labels=rep("",length(breaks))
	if (!is.null(highlightedSubjectCode))
	{
		subjectScore=extendedData[extendedData$SubjectCode==highlightedSubjectCode && extendedData$ProductCode==productCode,variable]
		colors[which(breaks==floor(subjectScore))]="lightgreen"
		labels[which(breaks==floor(subjectScore))]=paste(highlightedSubjectCode,"\n",subjectScore,sep="")
	}
	par(mar=c(3.7,3,2,0))
	par(oma=c(3+length(grep("\n", sub)),0,2,0))
	par(mgp=c(1.5, 0.5, 0))
	plot(res.hist,freq=FALSE,xaxt="n",xlim=xlim,col=colors,labels=labels,xlab=xlab,ylab=ylab,main="",cex.lab=0.8,cex.axis=0.8)
	axis(1,at=breaks,labels=round(breaks),cex.axis=0.8)
	
	# Titre et sous-titre
	mtext(main,3,line=0,cex=1.6,font=2)
	mtext(sub,1,line=3,cex=0.8,font=3)
	
	# Ajout de la courbe de normalit?
	if (normalDistribution==TRUE)
	{
		myx=seq(xlim[1],xlim[2],length.out=100)
		normal=dnorm(x=myx,mean=mean(x),sd=sd(x))
		lines(myx,normal,col = "blue",lwd = 2)
	}
	return(res.hist$counts)
}