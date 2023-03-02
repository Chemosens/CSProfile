#'profileFlashTable
#'
#'Return the flash table of the profileObject
#'@param profileObject profile object from \link{profileReadData}
#'@param classificationMethod default to"Complete", could also be any method available in the method of hclust ward.D", "ward.D2", "single", "complete", "average" (= UPGMA), "mcquitty" (= WPGMA), "median" (= WPGMC) or "centroid" (= UPGMC).
#'@param  explainedVariance default to 0.5. Percentage of variance to be explained.
#'@param  alphaContrast default to 0.1. Limit for significance of the contrast tests
#'@param  similarity default to "Pearson". Can also be 'Spearman' or 'Kendall'
#'@param  fileName default to "Flash table"
#'@param  show "" by default, can also contain "Colors" or "NonSignificantDifferentMeans"
#'@param contrastOption default to "GMean". Can also be "Product". In this case, the name of the product should be entered in contrastProduct parameter.
#'@param contrastProduct default to NULL. When contrastOption=="Product", should be the name of a product (character)
#'@export
#'@importFrom stats dist cor hclust var cutree as.formula
#'@examples
#'data(cheeses)
#'profileFlashTable(cheeses)
profileFlashTable = function(profileObject,classificationMethod="Complete", explainedVariance=0.5, alphaContrast=0.1, similarity="Pearson", fileName="Flash table", show="",contrastOption="GMean",contrastProduct=NULL)
{ 
  if(!profileObject$additionalCalculations)
  {
    if(length(profileObject$Replicates)==1){model="TwoWayAdditive"}else{model="TwoWayMultiplicative"}
    profileObject=profileSetUnivariateAnalysisParameters(profileObject, model=model, randomEffects="Subject", anovaCalculationMode="Ols", lsMeansAdjustment="Tukey", lsMeansAlpha=.05)
  }

	if(contrastOption=="Product"&&is.null(contrastProduct)){stop("The product chosen for contrasts is missing")}
	color=FALSE
	if ("Colors" %in% show)
	{
		color=TRUE
	}
	showNonSignificantNumbers=FALSE
	if ("NonSignificantDifferentMeans" %in% show)
	{
		showNonSignificantNumbers=TRUE
	}
	
	similarity=tolower(similarity)
	classificationMethod=tolower(classificationMethod)	
	showOnlySignificantVariables=profileObject$OnlySignificantAttributesInMultivariateAnalysis
	

		language="en"

	
	# to do ? distinction entre les groupes de variables
	extendedData=profileObject$CompleteExtendedDataWithoutNA[,-c(1:4)]
	canonicalData=profileObject$CompleteCanonicalDataWithoutNA
		
	# selection (ou non) des attributs significatifs uniquement dans listAttributes
	listAttributes=NULL
	listAnovas=list()
	Fprod=rep(NA,length(profileObject$Attributes))
	Pprod=rep(NA,length(profileObject$Attributes))
	atts=as.character(profileObject$Attributes)
	names(Fprod)=profileObject$Attributes;	names(Pprod)=profileObject$Attributes;
	 listAnova=list()
	 if(length(profileObject$Replicates)>1)
	 {
			 for (att in atts)
			 {
				listAnova[[att]]=profileObject$Anova[[att]]
				Fprod[att]=listAnova[[att]][[1]]$FProd
				Pprod[att]=listAnova[[att]][[1]]$PProd
				
				if (showOnlySignificantVariables==TRUE)
				{		
					if((profileObject$Anova[[att]][[1]]$PProd)<(profileObject$AlphaForSelectionOfSignificantAttributes))
					{
						listAttributes=c(listAttributes,att)
					}
				} else
				{
					listAttributes=profileObject$Attributes
				}		
			}
		}
		else # si on a une seule r?p?tition
		{
			for (att in atts)
			{
				listAnova[[att]]=profileObject$Anova[[att]]
				Fprod[att]=listAnova[[att]][[1]]$FProd
				Pprod[att]=listAnova[[att]][[1]]$PProd
				if (showOnlySignificantVariables==TRUE)
				{		
					if((profileObject$Anova[[att]][[1]]$PProd)<(profileObject$AlphaForSelectionOfSignificantAttributes))
					{
						listAttributes=c(listAttributes,att)
					}
				} else
				{
					listAttributes=profileObject$Attributes
				}		
			}
		
		}
		# cas ou aucun attribut n'est significatif
		if (length(listAttributes)==0)
		{
			# if(type=="html")
			# {
				 fileName=paste(output,".html", sep="")	
				 html=("<html><body>")
				 html=paste(html, "<h2>","Flash Table", "</h2>")
				 html=paste(html, "<p>","No Significant Attribute","</p>")
				 html=paste(html,"</body></html>",sep="",collapse="")		
				 cat(html,file=fileName, append=TRUE)
				 return()
			#}
		 }	
	
	## 1) determiner les groupes par la procedure VARCLUS: i est le nombre de groupes obtenus groups contient le lien entre les groupes et les attributs (tri al?atoire des groupes)
	distances=(dist(cor(scale(extendedData),method=similarity,use="pairwise.complete.obs")))
	hClust=hclust(distances,classificationMethod)
	i=2 #i nombre de groupes
	totalVariance=sum(diag(var(extendedData,na.rm=TRUE)))
	
	tabVar=NULL
	currentExplainedVariance=0
	while (currentExplainedVariance<=explainedVariance &&i<length(listAttributes))
	{
		# attention la ligne suivante n'est pas un doublon, initialisation imp?rative dans la boucle while
		currentExplainedVariance=0
		groups=cutree(hClust, k=i)
		for (u in 1:i)
		{ 
		
			indices=which(groups==u)
			datap=extendedData[,indices]
			mcov=var(datap,na.rm=TRUE)
			eig1=eigen(mcov)$values[1]
			vari=eig1/totalVariance
			tabVar[u]=vari
			currentExplainedVariance=currentExplainedVariance+vari
		}
		i=i+1
	}
	# vect contient l'ordre des groupes avec la meilleure variance expliquee : le premier groupe sera celui indiqu? par groups[vect]
	vect=order(tabVar, decreasing=TRUE)
	
	# Choix du premier attribut (dans le vecteur att1) de chaque groupe : celui avec le plus grand F produit et determination des variables significatives
	i=i-1
	att1=NULL
	
	for (u in 1:i)
	{ 
		indices=which(groups==u)
		datap=extendedData[,indices]
		if (length(indices)!=1)
		{
			Mcor=cor(datap,use="pairwise.complete.obs")
			listAtt=dimnames(Mcor)[[1]]
			FprodGp=rep(NA,length(indices))
			names(FprodGp)=listAtt
			PprodGp=rep(NA,length(indices))
			names(PprodGp)=listAtt
			
			for(attribute in listAtt)
			{
				FprodGp[attribute]=Fprod[attribute]
				PprodGp[attribute]=	Pprod[attribute]			
			}
			att1[u]=listAtt[which(FprodGp==max(FprodGp[!is.na(FprodGp)]))[1]]
		} else
		{
			att1[u]=names(indices)
		}
	}


		# firstAtt est le tout premier attribut de la table Flash: celui du groupe avec le meilleur F, #et le groupe qui explique le mieux la variance
	firstAtt=att1[vect[1]] 
	groupeContenantDesAttributsSignificatifs=1
	
	# si jamais ce premier attribut n'est pas significatif... prendre le premier significatif
	while(!firstAtt%in%listAttributes)
	{
		groupeContenantDesAttributsSignificatifs=groupeContenantDesAttributsSignificatifs+1
		firstAtt=att1[vect[groupeContenantDesAttributsSignificatifs]] 
	}
	

	# On d?termine l'ordre ? l'int?rieur de chaque groupe en comparant les corr?lations des premiers ?l?ments de chaque groupe avec le premier ?l?ment 

	liste=NULL
	for (k in 1:i) 
	{
		u=vect[k] # choix du groupe dans l'ordre des variances expliqu?es d?croissantes
		indices=which(groups==u)
		datap=extendedData[,indices] 
		if (length(indices)>2)  # si on a au moins deux descripteurs dans le groupe
		{
			# calculer les correlations des descripteurs avec le premier descripteur
			correlation=cor(datap[,att1[u]], datap[,-(which(colnames(datap)==att1[u]))],use="pairwise.complete.obs")
			v=order(abs(correlation), decreasing=TRUE)
			listord=colnames(correlation)[v]
			listord=c(att1[u],listord) #liste des attributs dans l'ordre pour le groupe u
		} else
		{
			if(length(indices)==2)
			{
			otherAtt=names(indices)[names(indices)!=att1[u]]
			listord=c(att1[u],otherAtt)
			}
			else
			{
				listord=c(att1[u])
			}
		}
		liste=c(liste, listord) # liste des attributs tous groupes confondus
	}
	liste=intersect(liste,listAttributes) # selection ?ventuelle des attributs significatifs uniqumeent

	options(contrasts = c("contr.sum","contr.sum"))
	
	# Moyenne par attribut
	pMeans=aggregate(as.formula("Score~AttributeCode+ProductCode"), canonicalData, mean, na.rm=TRUE)
	gMeans=aggregate(as.formula("Score~AttributeCode"), canonicalData, mean, na.rm=TRUE)
	
	# Vecteur de contrastes
	nbProducts=length(profileObject$Products)
	contrastVector=rep(1/(nbProducts-1),nbProducts)
	names(contrastVector)=sort(as.character(profileObject$Products))
	
	# Style pour sortie HTML
	style=NULL
	
	# Construction de la table FLASH
	flash=matrix(0,nrow=length(listAttributes), ncol=length(profileObject$Products)+3)
	meanMatrix=matrix(0,nrow=length(listAttributes), ncol=length(profileObject$Products))	
	pvalueMatrix=matrix(0,nrow=length(listAttributes), ncol=length(profileObject$Products))	
	colnames(flash)=c("F-Prod","P(F)","GMEAN",sort(as.character(profileObject$Products)))
	rownames(flash)=listAttributes
	colnames(meanMatrix)=sort(as.character(profileObject$Products))
	rownames(meanMatrix)=listAttributes
	colnames(pvalueMatrix)=sort(as.character(profileObject$Products))
	rownames(pvalueMatrix)=listAttributes
	
	# couleur de chaque premier attribut de chaque groupe
	 att1resulted=liste[liste%in%att1] # liste des premiers attributs significatifs dans l'ordre 
	 colorByFirstAttribute=rep(c("white","#D8D8D8"),length(att1resulted))
	 colorByFirstAttribute=colorByFirstAttribute[1:length(att1resulted)]
	 names(colorByFirstAttribute)=att1resulted #nom dans l'ordre des groupes qui apparaissent
	 
	for (attribute in listAttributes)
	{	
		groupAttribut=groups[attribute]
		firstAttributeOfGroup=att1[groupAttribut]
		groupColor=colorByFirstAttribute[firstAttributeOfGroup]
		style=AddHtmlStyle(style,attribute,"GMEAN",backgroundColor=groupColor)
		style=AddHtmlStyle(style,attribute,"F-Prod",backgroundColor=groupColor)
		style=AddHtmlStyle(style,attribute,"P(F)",backgroundColor=groupColor)

		flash[attribute,"F-Prod"]=sprintf("%.2f",Fprod[attribute])
		flash[attribute,"P(F)"]=FriendlyPValue(Pprod[attribute])
		gMean=sprintf("%.2f",gMeans[gMeans$AttributeCode==attribute,"Score"])
		flash[attribute,"GMEAN"]=gMean
		
		for (product in sort(as.character(profileObject$Products)))
		{
			currentMean=sprintf("%.2f",pMeans[pMeans$AttributeCode==attribute & pMeans$ProductCode==product,"Score"])
			meanMatrix[attribute,product]=currentMean	
			resLsmeans=profileObject$Anova[[attribute]][[1]][["resLsmeans"]]
			if(contrastOption=="GMean")
			{
				tmpContrastVect=contrastVector
				tmpContrastVect[product]=-1
				resContrast=summary(contrast(resLsmeans,method=list(tmpContrastVect)),adjust="holm")
				Pr=resContrast$p.value
			}
			if(contrastOption=="Product")
			{
				if(!contrastProduct%in%as.character(profileObject$Products)){stop("Please enter the name of a product.")}
				else
				{
					if(contrastProduct!=product)
					{
			
						tmpContrastVect=rep(0,length(profileObject$Products))
						names(tmpContrastVect)=(profileObject$Products)
						tmpContrastVect[contrastProduct]=-1
						tmpContrastVect[product]=1
						resContrast=summary(contrast(resLsmeans,list(tmpContrastVect)),adjust="holm")
						Pr=resContrast$p.value
					}
					else
					{
						Pr=1
					}
				
				}
			}
			
		
			if (Pr<=alphaContrast)
			{
				if(as.numeric(currentMean)>=as.numeric(gMean))
				{
					style=AddHtmlStyle(style,attribute,product,backgroundColor="#00FF00")
					currentMean=paste(currentMean,"+",sep="")
				} else
				{
					style=AddHtmlStyle(style,attribute,product,backgroundColor="yellow")
					currentMean=paste(currentMean,"-",sep="")
				}
			} else
			{
				if(!showNonSignificantNumbers)
				{
					currentMean=""
					style=AddHtmlStyle(style,attribute,product,backgroundColor=groupColor)
		
				}else
				{
					currentMean=currentMean
					style=AddHtmlStyle(style,attribute,product,backgroundColor=groupColor)
				}
			}	
			flash[attribute,product]=currentMean		
		}
	}	

	sortedProductIndex=sort(as.numeric(as.character(meanMatrix[firstAtt,])),decreasing=FALSE,index.return=TRUE)$ix
	flash[,4:dim(flash)[2]]=flash[,sortedProductIndex+3]
	colnames(flash)=c("F-Prod","P(F)","GMEAN",sort(as.character(profileObject$Products))[sortedProductIndex])
	sortedAttributeIndex=match(liste,as.character(listAttributes)) #inversion?
	flash=flash[sortedAttributeIndex,]	
	colnames(flash)=c("F-Prod","P(F)","GMEAN",sort(as.character(profileObject$Products))[sortedProductIndex])
	rownames(flash)=listAttributes[sortedAttributeIndex]
	
	# Output 
	html=("<html><body>")
	html=paste(html, "<h2>","Flash Table", "</h2>")
	html=paste(html, "<p>","Similarity",": ", similarity, "</p>")
	html=paste(html, "<p>","Classification Method",": ", classificationMethod, "</p>")
	html=paste(html, "<p>","Minimum Proportion Of Variance Explained",": ", explainedVariance, "</p>")
	html=paste(html, "<p>","Contrast Test Alpha",": ", alphaContrast, "</p>")
	html=paste(html, "<p>","Variable Selection Alpha",": ",profileObject$AlphaForSelectionOfSignificantAttributes , "</p>")
	html=paste(html, "<p>","Model",": ", profileObject$AnovaModel, "</p>")
	html=paste(html, MatrixToHtml(flash,style=style), sep="")
	html=paste(html,"<p style='color:#00FF00'>","FlashTableLegendGreen","</p>",sep="")
	html=paste(html,"<p style='color:yellow'>","FlashTableLegendYellow","</p>",sep="")
	html=paste(html,"</body></html>",sep="",collapse="")
	print(paste0("The file was produced in ", getwd()))
	cat(html,file=paste(fileName,".html",sep=""), append=FALSE)
  	
	return(flash)
}