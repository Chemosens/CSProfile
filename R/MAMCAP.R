#'@importFrom CSUtils MatrixToHtml
MAMCAP=function(object,panelLimit=0.05,indivLimit=0.05,language="fr",output="tabCAP",correction=FALSE,option="mam", correlationTest="kendall",negativeCorrection=TRUE,correctOnlyIfSignificant=FALSE,multidimLine=FALSE,levelOption=TRUE)
{# if data mul
	if(is.data.frame(object)){data=object;}else{dataRes=object$CompleteExtendedDataWithoutNA}
    indivLimit1=0.01
    indivLimit2=0.05
    indivLimit3=0.1
	if(!option %in% c("overall","mam")){stop("Please choose an option 'overall' or 'mam'")}
	if(correlationTest=="kendall"||correlationTest=="spearman"||correlationTest=="pearson"){correlationLimit=0.2}else{correlationLimit=indivLimit}
     texteCellule=function(discrim,indivLimit1=0.01,indivLimit2=0.05,indivLimit3=0.1,scaling,p.scaling,indivLimit,p.rep,text.rep,levelOption=TRUE,levelSig=NULL,level=NULL,levelLimit=NULL)
    {
		# renvoie le texte d'une cellule attribut * sujet en fonction des p values qui leurs sont associ?es
		V=sort(c(indivLimit1,indivLimit2,indivLimit3))
		indivLimit1=V[1]
		indivLimit2=V[2]
		indivLimit3=V[3]
		cell=""
		if(!is.na(discrim))
		{     

			#crochet gauche
			if(!is.na(scaling)&scaling<=0){cell=paste(cell,"?",sep="")}
			else
			{
				if(p.scaling<indivLimit)
				{
					if(!is.na(scaling))
					{
						if(scaling>1){cell=paste(cell,"&lt;",sep="")}
						if(scaling<=1&scaling>0){cell=paste(cell,"&gt;",sep="")}
						
					}
					if(is.na(scaling)){cell=paste(cell,"!",sep="")}
				}
				else
				{
					if(!levelOption){	cell=paste(cell,"|",sep="")}
				}
			}
			if(!levelOption)
			{
				if(indivLimit1<discrim & discrim<indivLimit2){cell=paste(cell,"--",sep="")}
				if(indivLimit1>discrim){cell=paste(cell,"---",sep="")}        
				if(indivLimit3<discrim){ cell=paste(cell," ",sep="")}
				if(indivLimit2<discrim & discrim<indivLimit3){ cell=paste(cell,"-",sep="") }
				
			}
			else
			{
				if(levelSig<levelLimit & sign(level)==-1 ){cell=paste(cell,"-",sep="")}
				if(levelSig<levelLimit & sign(level)==1){ cell=paste(cell,"+",sep="")}       
				if(levelSig>levelLimit ){cell=paste(cell," ",sep="")}  
						
			}
			if(!is.na(p.rep)){if(p.rep<indivLimit){cell=paste(cell,text.rep,sep="")}}
			#crochet droit
			if(scaling<=0){cell=paste(cell,"?",sep="")}
			else
			{
				if(p.scaling<indivLimit)
				{
					if(!is.na(scaling))
					{
					if(scaling>1){cell=paste(cell,"&gt;",sep="")}
					if(scaling<=1&scaling>0){cell=paste(cell,"&lt;",sep="")}
					if(scaling<=0){cell=paste(cell,"?",sep="")}
					}
					if(is.na(scaling)){cell=paste(cell,"!",sep="")	}

				}
				else
				{
					if(!levelOption){cell=paste(cell,"|",sep="")}
				}    
			}
		}
		if(is.na(discrim))
		{
		  cell="0"
		}
      
		return(cell)
    }
   
    # #######################
    # # Initialisations
    # #######################
    # firstvar=5
    # lastvar=dim(dataRes)[2]
    attribut="Attributes"
     titre=paste("MAM CAP Table (",option, ")",sep="") 
    if(!levelOption){text.rep="LR"}else{text.rep="!"}
    att=as.character(object$Attributes); attributeNumber=length(att)
	subjectNumber=length(as.character(object$Subjects))
    products=object$Products;productNumber=length(products)
    rep=object$Replicates;replicateNumber=length(rep)	     
    if(correction){indivLimit=indivLimit/subjectNumber}
   
  ## conditions d'utilisation de la fonction scalesensperf : 

    if (productNumber<3){stop(paste("This output is not available for strictly less than 3 products"))}
    if (replicateNumber<2){stop("This output is not available for only 1 replicate")}
    if (sum(is.na(dataRes))>0){stop("This output is not available for unbalanced data")}
    
	# The frame MUST have the structure: ass prod rep att1 att2 ....attP
    ## on applique la fonction scalesensperf (correction for the use of the scale (brockhoff))
    dataRes2=dataRes[,c("SubjectCode","ProductCode","Replicate",att)]
    # resultat=scalesensperf2(dataRes2,pvalue=TRUE,adjustedMAM=negaativeCorrection,alpha_conditionalMAM=0.2,correctOnlyIfSignificant=correctOnlyIfSignificant)
	
	# resMultiMAM=MultiMAM(dataRes2, type="total",negativeCorrection=negativeCorrection,correctOnlyIfSignificant=correctOnlyIfSignificant,limitOfSignificance=0.05,plotReg=FALSE)
	resultatPerf=PanelPerformances(frame=dataRes2, modelType="overall",negativeCorrection=TRUE,correctOnlyIfSignificant=FALSE,limitOfSignificance=0.05,onlySignificantDim=FALSE,manovaTest="Hotelling", panelistPerf=TRUE,correlationTest=correlationTest,levelOption=levelOption,whenOverallUseMAMForTest=TRUE)
	listPanelPerf=resultatPerf$listPanelPerf
	#if(option=="overall"){	scalingCoefficient=resultatPerf$Beta}else{scalingCoefficient=resultatPerf$Beta}
	
	
	panelProductF=listPanelPerf$Fdiscr;panelProductPvalue=listPanelPerf$Pdiscr
	if(levelOption)
	{
			panelSubjectF=listPanelPerf$Fsuj;panelSubjectPvalue=listPanelPerf$Psuj
		
	}
	panelDisagreementF=listPanelPerf$Fdisag;panelDisagreementPvalue=listPanelPerf$Pdisag
	panelScalingF=listPanelPerf$Fscal;panelScalingPvalue=listPanelPerf$Pscal
	panelRepeatability=(listPanelPerf$SRMSError)
	
	panelAverage=listPanelPerf$avg
	listPanelistPerf=resultatPerf$listPanelistPerf
	individualProductF=listPanelistPerf$Fdiscr;individualProductPvalue=listPanelistPerf$Pdiscr
	individualDisagreementF=listPanelistPerf$Fdisag;individualDisagreementPvalue=listPanelistPerf$Pdisag
	individualScalingF=listPanelistPerf$Fscal;individualScalingPvalue=listPanelistPerf$Pscal
	individualRepeatabilityF=listPanelistPerf$MSError;individualRepeatabilityPvalue=listPanelistPerf$Perror

	individualOverallLevelSig=listPanelistPerf$sigOverallTTest
	individualOverallMeans=listPanelistPerf$sigOverallMean
	individualLevel=listPanelistPerf$level
	individualLevelTTest=listPanelistPerf$levelTTest
	individualScaling=resultatPerf$UsualBeta
	
	individualOverallScaling=listPanelistPerf$overallScaling
	individualOverallScalingSig=listPanelistPerf$sigOverallScaling
	 FRank=rep(0,subjectNumber); names(FRank)=colnames(listPanelistPerf$MSError)  
	 suj=colnames(listPanelistPerf$MSError)  

	# {
		# multiResults=multiMAM(frame=dataRes2)
	# }
	# if(option=="MAM")
	# {
		# multiResults=MAMExtended(frame=dataRes2)
	# }	
	
	

    #obtention des rankf
	individualProductF3=individualProductF
    individualProductF3[is.na(individualProductF3)]=0
    individualProductF2=max(individualProductF3[!is.na(individualProductF3)])-individualProductF3
    rankMatrix=apply(individualProductF2,1,rank)

  
    for(j in 1:subjectNumber){  FRank[j]=mean(rankMatrix[j,])}
    ###########
    # TRI
	###########
    ind=sort(FRank,index.return=TRUE)$ix
    names(panelScalingF)=att
    names(panelProductF)=att    
    indice=(1:subjectNumber)[ind]
    suj=suj[ind]
	individualOverallLevelSig=individualOverallLevelSig[ind]
	individualOverallMeans=individualOverallMeans[ind]
	individualOverallScaling=individualOverallScaling[ind]
	individualOverallScalingSig=individualOverallScalingSig[ind]
    # on retrie tout dans l'ordre...
    FRank=FRank[ind]
    ind2=sort(panelProductF,index.return=TRUE,decreasing=TRUE)$ix
    att=att[ind2]
	
	if(levelOption)
	{
		panelSubjectF=	panelSubjectF[ind2]
		panelSubjectPvalue=panelSubjectPvalue[ind2]
	}
	
    panelProductF=panelProductF[ind2]
	
    panelScalingF=panelScalingF[ind2]
    panelDisagreementF=panelDisagreementF[ind2]
    panelRepeatability=panelRepeatability[ind2] 
    panelScalingPvalue=panelScalingPvalue[ind2]
    panelProductPvalue=panelProductPvalue[ind2]
    panelDisagreementPvalue=panelDisagreementPvalue[ind2]
	
    individualProductF= individualProductF[ind2,ind]
    individualProductPvalue=individualProductPvalue[ind2,ind]
   # significanceMatrix=significanceMatrix[ind2,ind]
    individualRepeatabilityF=individualRepeatabilityF[ind2,ind]
    individualRepeatabilityPvalue=individualRepeatabilityPvalue[ind2,ind]
    individualDisagreementF=individualDisagreementF[ind2,ind]
	individualDisagreementPvalue=individualDisagreementPvalue[ind2,ind]
	individualLevel=individualLevel[ind2,ind]
	individualLevelTTest=individualLevelTTest[ind2,ind]
    
	#mat.level=mat.level[ind2,ind]
    individualScalingPvalue=individualScalingPvalue[ind2,ind]
	#scalingCoefficient=scalingCoefficient[ind]
	individualScaling=individualScaling[ind2,ind]
    #panel.repet=panel.repet[ind2]
    panelAverage=panelAverage[ind2]
   
	# Resulting list
    if(!levelOption)
	{
	resultingList=list(panelProductF,panelScalingF,panelDisagreementF,panelRepeatability,panelScalingPvalue,panelProductPvalue,panelDisagreementPvalue,
	individualProductF,individualProductPvalue,individualScaling, individualScalingPvalue,individualDisagreementF,individualDisagreementPvalue, individualRepeatabilityF,individualRepeatabilityPvalue)
	names(resultingList)=c("panelProductF","panelScalingF","panelDisagreementF","panelRepeatability","panelScalingPvalue","panelProductPvalue","panelDisagreementPvalue",
	"individualProductF","individualProductPvalue" ,"scalingCoefficient","individualScalingPvalue","individualDisagreementStat", "individualDisagreementPvalue","individualCME","individualRepeatabilityPvalue")
	}
	else
	{
		resultingList=list(panelProductF,panelSubjectF,panelScalingF,panelDisagreementF,panelRepeatability,panelScalingPvalue,panelProductPvalue,panelSubjectPvalue,panelDisagreementPvalue,
	individualProductF,individualProductPvalue, individualScalingPvalue,individualDisagreementF,individualDisagreementPvalue, individualRepeatabilityF,individualRepeatabilityPvalue,individualScaling,
	individualOverallScaling,individualOverallScalingSig,individualOverallLevelSig,individualOverallMeans,individualLevel,individualLevelTTest)

		names(resultingList)=c("panelProductF","panelSubjectF","panelScalingF","panelDisagreementF","panelRepeatability","panelScalingPvalue","panelProductPvalue","panelSubjectPvalue","panelDisagreementPvalue",
	"individualProductF","individualProductPvalue" ,"individualScalingPvalue","individualDisagreementStat", "individualDisagreementPvalue","individualCME","individualRepeatabilityPvalue","individualScaling",
	"overallScaling","pvalOverallScaling","overallLevel","pvalOverallLevel","individualLevel","pvalIndividualLevel")
	
	}
	####################################
    # OBTENTION DES STYLES DE LA TABLE
    ####################################
           
    style=NULL
    indices=which(panelProductPvalue<=panelLimit,arr.ind=TRUE) # colonne FProd
    if (length(indices) > 0)
    {
		for (i in 1:length(indices))
		{
			style=AddHtmlStyle(style,att[indices[i]],"FProd",backgroundColor="#00FF00")
		}
    }
    indices=which(panelProductPvalue>panelLimit,arr.ind=TRUE)
    if (length(indices) > 0)
    {
		for (i in 1:length(indices))
		{
			style=AddHtmlStyle(style,att[indices[i]],"FProd",backgroundColor="red")
		}
    }
	if(levelOption)
	{
		 indices=which(panelSubjectPvalue>=panelLimit,arr.ind=TRUE) # colonne FSuj
		if (length(indices) > 0)
		{
			for (i in 1:length(indices))
			{
				style=AddHtmlStyle(style,att[indices[i]],"FSubj",backgroundColor="#00FF00")
			}
		}
		indices=which(panelSubjectPvalue<panelLimit,arr.ind=TRUE)
		if (length(indices) > 0)
		{
			for (i in 1:length(indices))
			{
				style=AddHtmlStyle(style,att[indices[i]],"FSubj",backgroundColor="red")
			}
		}
		
	}
    indices=which(panelScalingPvalue<=panelLimit,arr.ind=TRUE) #colonne FScal
    if (length(indices) > 0)
    {
		for (i in 1:length(indices))
		{
			style=AddHtmlStyle(style,att[indices[i]],"FScal",backgroundColor="red")
		}
    }
    indices=which(panelScalingPvalue>panelLimit,arr.ind=TRUE)
    if (length(indices) > 0)
    {
		for (i in 1:length(indices))
		{
			style=AddHtmlStyle(style,att[indices[i]],"FScal",backgroundColor="#00FF00")
		}
    }
	
	
    indices=which(panelDisagreementPvalue<=panelLimit,arr.ind=TRUE)
    if (length(indices) > 0)
    {
		for (i in 1:length(indices))
		{
			style=AddHtmlStyle(style,att[indices[i]],"FDisag",backgroundColor="red")
		}
    }
    indices=which(panelDisagreementPvalue>panelLimit,arr.ind=TRUE)
    if (length(indices) > 0)
    {
		for (i in 1:length(indices))
		{
        style=AddHtmlStyle(style,att[indices[i]],"FDisag",backgroundColor="#00FF00")
		}
    }
    
	
    indices=which(individualProductPvalue<=indivLimit&individualDisagreementPvalue>correlationLimit,arr.ind=TRUE)
    if (length(indices) > 0)
    { 
		if(correlationTest=="kendall"|correlationTest=="pearson"|correlationTest=="spearman"){color="red"}else{color="#00FF00"}
		if(is.vector(indices)){nbIndices=length(indices)}else{nbIndices=nrow(indices)}
		for (i in 1:nbIndices)
		{
			if(is.vector(indices)){ind1=1;ind2=indices[i]}else{ind1=indices[i,1];ind2=indices[i,2]}
			style=AddHtmlStyle(style,att[ind1],suj[ind2],backgroundColor=color)
		}
    }
    
	
	indices=which(individualProductPvalue<=indivLimit&individualDisagreementPvalue<=correlationLimit,arr.ind=TRUE)
    if (length(indices) > 0)
    {
		if(correlationTest=="kendall"|correlationTest=="pearson"|correlationTest=="spearman"){color="#00FF00"}else{color="red"}
		if(is.vector(indices)){nbIndices=length(indices)}else{nbIndices=nrow(indices)}
		for (i in 1:nbIndices)
		{
			if(is.vector(indices)){ind1=1;ind2=indices[i]}else{ind1=indices[i,1];ind2=indices[i,2]}
			style=AddHtmlStyle(style,att[ind1],suj[ind2],backgroundColor=color)
		}
    }    
    indices=which(individualProductPvalue>indivLimit,arr.ind=TRUE)
    if (length(indices) > 0)
    {
		if(is.vector(indices)){nbIndices=length(indices)}else{nbIndices=nrow(indices)}
		for (i in 1:nbIndices)
		{
			if(is.vector(indices)){ind1=1;ind2=indices[i]}else{ind1=indices[i,1];ind2=indices[i,2]}
			style=AddHtmlStyle(style,att[ind1],suj[ind2],backgroundColor="yellow")
		}
    }
    indices=which(is.na(individualProductPvalue),arr.ind=TRUE)
    if (length(indices) > 0)
    { 
		if(correlationTest=="kendall"|correlationTest=="pearson"|correlationTest=="spearman"){color="red"}else{color="#00FF00"}
		if(is.vector(indices)){nbIndices=length(indices)}else{nbIndices=nrow(indices)}
		for (i in 1:nbIndices)
		{
			if(is.vector(indices)){ind1=1;ind2=indices[i]}else{ind1=indices[i,1];ind2=indices[i,2]}
			style=AddHtmlStyle(style,att[ind1],suj[ind2],backgroundColor="orange")
		}
    }
    
	tableauTxt=matrix("",attributeNumber,subjectNumber) #levelSig
    for(j in 1:subjectNumber)
    {
		if(attributeNumber>1)
		{
		  for(i in 1:attributeNumber)
		  {

		  	if(!levelOption)
			{
				tableauTxt[i,j]=texteCellule(discrim=individualProductPvalue[i,j],indivLimit1,indivLimit2,indivLimit3,scaling=individualScaling[i,j],p.scaling=individualScalingPvalue[i,j],indivLimit,individualRepeatabilityPvalue[i,j],text.rep=text.rep,levelOption=FALSE)
			}
			if(levelOption)
			{
			 	tableauTxt[i,j]=texteCellule(discrim=individualProductPvalue[i,j],indivLimit1,indivLimit2,indivLimit3,scaling=individualScaling[i,j],p.scaling=individualScalingPvalue[i,j],indivLimit=indivLimit,p.rep=individualRepeatabilityPvalue[i,j],text.rep=text.rep,levelOption=TRUE,levelSig=individualLevelTTest[i,j],level=individualLevel[i,j],levelLimit=0.05)
			}
		 }
		}
		else
		{	
			if(!levelOption)
			{
				tableauTxt[1,j]=texteCellule(discrim=individualProductPvalue[j],indivLimit1,indivLimit2,indivLimit3,scaling=individualScaling[j],p.scaling=individualScalingPvalue[j],indivLimit=indivLimit,individualRepeatabilityPvalue[j],text.rep=text.rep,levelOption=FALSE)
			}
			if(levelOption)
			{
			 	tableauTxt[1,j]=texteCellule(discrim=individualProductPvalue[j],indivLimit1,indivLimit2,indivLimit3,scaling=individualScaling[j],p.scaling=individualScalingPvalue[j],indivLimit=indivLimit,p.rep=individualRepeatabilityPvalue[j],text.rep=text.rep,levelOption=TRUE,levelSig=individualLevelTTest[j],level=individualLevel[j],levelLimit=0.05)
			}
		}
    }
    # Multidimensional line
	if(levelOption)
	{
		multiPanelistPerf=rep(NA,subjectNumber)

		  for(j in 1:subjectNumber)
		 {
		
			   multiPanelistPerf[j]= texteCellule(discrim=1,indivLimit1,indivLimit2,indivLimit3,scaling=individualOverallScaling[j],p.scaling=individualOverallScalingSig[j],indivLimit,p.rep=1,text.rep=text.rep,levelSig=individualOverallLevelSig[j],level=individualOverallMeans[j],levelLimit=0.05)
		 }
		  multidimensionalLineToAdd=c(round(mean(panelAverage),digits=2),"","","", "","", multiPanelistPerf) 
	}
		 
	 #tableauRes=rbind(tableauRes,multidimensionalLine
	 
	if(!levelOption)
	{
		tableauRes=cbind(sprintf("%.2f",panelAverage), sprintf("%.2f",panelProductF),sprintf("%.2f",panelScalingF), sprintf("%.2f",panelDisagreementF),sprintf("%.2f",panelRepeatability),tableauTxt)
		tableauRes=rbind(tableauRes,c("-","-","-","-","-",as.character(round(FRank,digits=2))))
		colnames(tableauRes)=c("Mean","FProd","FScal","FDisag","RMSE",suj)   
    }
	else
	{
		tableauRes=cbind(sprintf("%.2f",panelAverage), sprintf("%.2f",panelProductF),sprintf("%.2f",panelSubjectF),sprintf("%.2f",panelScalingF), sprintf("%.2f",panelDisagreementF),sprintf("%.2f",panelRepeatability),tableauTxt)
		tableauRes=rbind(tableauRes,c("-","-","-","-","-","-",as.character(round(FRank,digits=2))))
		colnames(tableauRes)=c("Mean","FProd","FSubj","FScal","FDisag","RMSE",suj)   
	}
	#rownames(tableauRes)=c(att,TS_GetLabel("RankF",language),TS_GetLabel("Total",language))           
    rownames(tableauRes)=c(att,"RankF")           
    if(levelOption)
	{
		tableauRes=rbind(tableauRes,multidimensionalLineToAdd)
		rownames(tableauRes)=c(att,"RankF","Total")
	}
	
  	#############################
    # Writing table and legend
    ##############################         
    txt="<html>"          
    txt=paste(txt, "<h3>",titre,"</h3>",MatrixToHtml(tableauRes, style=style), sep="") 
    txt=paste(txt,"<table border='1' cellspacing='1'><tr><td colspan='3' style='text-align:center'><b>","PanelPerf","</b></td><td style='text-align:center'><b>","PanelistPerf","</b>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;</td></tr>")
   
	if(!levelOption)
	{txt=paste(txt,"<tr><td style='text-align:center'><b>","FProd","</b></td><td style='text-align:center'><b>","FScal","</b></td><td style='text-align:center'><b>","FDisag","</b></td><td rowspan='3'>&nbsp;","ProductDiscrimination",": --- p < 0.01 -- p < 0.05 - p < 0.1<br/>&nbsp;Scaling Information: || Average scale >< Significant Smaller Scale <> Significant Larger Scale ? ? Negative scale<br>&nbsp;","Agreement"," <span style='background-color:green'>","Yes","</span> <span style='background-color:red'>","No","</span> <span style='background-color:yellow'> ","NotTested","</span><br/>&nbsp;lr: ","Less Repeatable","</td></tr>",sep="")}

	
	if(levelOption){  txt=paste(txt,"<tr><td style='text-align:center'><b>","FProd","</b></td><td style='text-align:center'><b>","FScal","</b></td><td style='text-align:center'><b>","FDisag","</b></td><td rowspan='3'>&nbsp;","ProductDiscrimination",": --- p < 0.01 -- p < 0.05 - p < 0.1  <span style='background-color:yellow'> ","NotTested","</span><br/>&nbsp;Scaling Information: || Average scale >< Significant Smaller Scale <> Significant Larger Scale ? ? Negative scale<br>&nbsp;","Agreement"," <span style='background-color:green'>","Yes","</span> <span style='background-color:red'>","No","</span><br/>&nbsp;LR: ","Less Repeatable","</td></tr>",sep="")}
    txt=paste(txt,"<tr><td style='text-align:center;background-color:#FF0000'>p> ",panelLimit,"</td><td style='text-align:center;background-color:#FF0000'>p<",panelLimit,"</td><td style='text-align:center;background-color:#FF0000'> p<",panelLimit,"</td></tr>")
    txt=paste(txt,"<tr><td style='text-align:center;background-color:#00FF00'>p< ",panelLimit,"</td><td style='text-align:center;background-color:#00FF00'>p>",panelLimit,"</td><td style='text-align:center;background-color:#00FF00'> p>",panelLimit,"</td></tr>") 
 #   txt=paste(txt,"</table><br>",TS_GetLabel("PhraseNA",language),"</html>")
  	write(txt,file=paste(output,".html",sep=""))
	resultingList[["txt"]]=txt
	return(resultingList)
}

