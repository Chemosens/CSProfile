#'@importFrom stats dist cor hclust var hclust cutree as.formula
FlashTable = function(profileObject,classificationMethod="complete", model="ANOVA", explainedVariance=0.5, alphaContrast=0.1, similarity="pearson", color=TRUE, showNonSignificantNumbers=FALSE, fileName="FlashTable",randomSubject=T,ml=T)
{
	showOnlySignificantVariables=profileObject$OnlySignificantAttributesInMultivariateAnalysis
	# choix du language
	language="en"
	type=output=NULL
	# to do ? distinction entre les groupes de variables
	extendedData=profileObject$CompleteExtendedDataWithoutNA[,-c(1:4)]
	canonicalData=profileObject$CompleteCanonicalDataWithoutNA

	# 1 ANOVA par descripteur (en fonction du mod?le)
	# if (model == "ANOVA")
	# {
		# listAnovas = ANOVA(profileObject$extendedData, variable="Score", mainEffects=c("ProductCode","SubjectCode"), interactionOrder=1, randomEffects=c("SubjectCode"), anovaAlpha=0.05, varianceTest="none", varianceTestAlpha=0.05, normalityTest="none", normalityTestAlpha=0.05, multipleComparisonTest="LSD", multipleComparisonTestAlpha=0.05)
	# }
	# TODO AnovaV2
	# if (model == "MAM")
	# {
		# listANOVAS = MAM(profileObject, language)
	# }

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
				# dataSubset=canonicalData[canonicalData[,"AttributeCode"]==att,]
				 # listAnovas[[att]]=anova(lm(dataSubset$Score~dataSubset$ProductCode*dataSubset$SubjectCode))
				# dataSubset=profileObject$ExtendedData[,c("SubjectCode","ProductCode","Replicate",as.character(att))]
				# colnames(dataSubset)[4]="Y"
				listAnova[[att]]=profileObject$Anova[[att]]

				# Fprod[att]=listAnovas[[att]][1,3]/listAnovas[[att]][3,3]
				# Pprod[att]=pf(Fprod[att],df1=listAnovas[[att]][1,1],df2=listAnovas[[att]][3,1], lower.tail=FALSE)
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

				# dataSubset=profileObject$ExtendedData[,c("SubjectCode","ProductCode","Replicate",attribute)]
				# colnames(dataSubset)[4]="Y"
				listAnova[[att]]=profileObject$Anova[[att]]
				# listAnova[[att]]=AnovaV2(dataFrame=dataSubset,model="TwoWayMultiplicative",randomSubject=randomSubject,ml=ml,correlationStructure="AR1",testRep="EachRepVsPrevious",lsMeansAlpha=0.10, lsMeansAdjustment="Tukey")

				# dataSubset=canonicalData[canonicalData[,"AttributeCode"]==att,]
				# listAnovas[[att]]=anova(lm(dataSubset$Score~dataSubset$ProductCode+dataSubset$SubjectCode))
				# Fprod[att]=listAnova[[att]][[1]]$FProd
				# Pprod[att]=listAnova[[att]][[1]]$FProd
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
			 if(type=="html")
			 {
				 fileName=paste(output,".",type, sep="")
				 html=("<html><body>")
				 html=paste(html, "<h2>FlashTable", "</h2>")
				 html=paste(html, "<p>NoSignificantAttribute","</p>")
				 html=paste(html,"</body></html>",sep="",collapse="")
				 cat(html,file=fileName, append=TRUE)
				 return()
			 }
		 }

			# anovaG=listAnovas$Results[[att]]
			# anovaTable=anovaG$ANOVATable
			# anovaF=anovaTable["ProductCode","Pr(>F)"]
			# if (anovaF<=alpha)
			# {
				# listAttributes=c(listAttributes,att)
			# }
		# }



	## 1) determiner les groupes par la procedure VARCLUS: i est le nombre de groupes obtenus groups contient le lien entre les groupes et les attributs (tri al?atoire des groupes)
	distances=(dist(cor(scale(extendedData),method=similarity,use="pairwise.complete.obs")))
	hClust=hclust(distances,classificationMethod)
	i=2 #i nombre de groupes
	totalVariance=sum(diag(var(extendedData,na.rm=TRUE)))
	currentExplainedVariance=0
	tabVar=NULL
	while (currentExplainedVariance<=explainedVariance)
	{
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
			FprodGp=rep(0,length(indices))
			names(FprodGp)=listAtt
			PprodGp=rep(0,length(indices))
			names(PprodGp)=listAtt

			for(attribute in listAtt)
			{
				#if(model=="ANOVA")
				#{
					FprodGp[attribute]=Fprod[attribute]
					PprodGp[attribute]=	Pprod[attribute]
				#}
				# if(model=="MAM")
				# {
					# anovAtt=brockhoffAnova[[2]][,,attribute]
					# Fprod[attribute] = decimal(as.numeric(anovAtt[2,4]),digits=2)
					# anovaP=anovaAtt["ProductCode","Pr(>F)"]
					# if (anovaP<=alpha)
					# {
						# listAttributes=c(listAttributes,attribute)
					# }
				# }
			}
			att1[u]=listAtt[which(FprodGp==max(FprodGp))[1]]
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


	# tabCor=NULL
	# for (u in 1:i)
	# {
		# tabCor[u]=cor(extendedData[,firstAtt], extendedData[, att1[u]],use="pairwise.complete.obs")
	# }

		# On d?termine l'ordre ? l'int?rieur de chaque groupe en comparant les corr?lations des premiers ?l?ments de chaque groupe avec le premier ?l?ment

	liste=NULL
	for (k in 1:i)
	{
		u=vect[k] # choix du groupe dans l'ordre des variances expliqu?es d?croissantes
		indices=which(groups==u)
		datap=extendedData[,indices]
		if (length(indices)>2)  # si on a au moins trois descripteurs dans le groupe
		{
			# calculer les correlations des descripteurs avec le premier descripteur
			correlation=cor(datap[,att1[u]], datap[,-(which(colnames(datap)==att1[u]))],use="pairwise.complete.obs")
			v=order(abs(correlation), decreasing=TRUE)
			listord=colnames(correlation)[v]
			listord=c(att1[u],listord) #liste des attributs dans l'ordre pour le groupe u
		} else
		{
			listord=names(indices)
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
	names(contrastVector)=(profileObject$Products)
	# contrastMatrix=matrix(1/(nbProducts-1),nbProducts,nbProducts)
	# diag(contrastMatrix)=-1
	# contrastMatrix[,1]=1/nbProducts
	# contrastSolvedMatrix=solve(t(contrastMatrix))

	# Style pour sortie HTML
	style=NULL

	# Construction de la table FLASH
	flash=matrix(0,nrow=length(listAttributes), ncol=length(profileObject$Products)+3)
	meanMatrix=matrix(0,nrow=length(listAttributes), ncol=length(profileObject$Products))
	pvalueMatrix=matrix(0,nrow=length(listAttributes), ncol=length(profileObject$Products))
	colnames(flash)=c("F","P(F)","GMEAN",as.character(profileObject$Products))
	rownames(flash)=listAttributes
	colnames(meanMatrix)=profileObject$Products
	rownames(meanMatrix)=listAttributes
	colnames(pvalueMatrix)=profileObject$Products
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
		style=AddHtmlStyle(style,attribute,"F",backgroundColor=groupColor)
		style=AddHtmlStyle(style,attribute,"P(F)",backgroundColor=groupColor)
		#style=AddHtmlStyle(style,attribute,1,backgroundColor=groupColor)

		flash[attribute,"F"]=round(Fprod[attribute],2)
		flash[attribute,"P(F)"]=FriendlyPValue(Pprod[attribute])
		gMean=round(gMeans[gMeans$AttributeCode==attribute,"Score"],2)
		flash[attribute,"GMEAN"]=gMean

		# currentAnovaTable=listAnovas[[attribute]]
		# df2=currentAnovaTable["Residuals", "Df"]
		for (product in profileObject$Products)
		{
			currentMean=round(pMeans[pMeans$AttributeCode==attribute & pMeans$ProductCode==product,"Score"],2)
			meanMatrix[attribute,product]=currentMean

			# Test de contraste Anova Score~ProductCode
			# tmpContrastVect=contrastVector
			# tmpContrastVect[product]=-1
			# factormat=canonicalData[canonicalData$AttributeCode==attribute,"ProductCode"]
			# scoremat=canonicalData[canonicalData$AttributeCode==attribute,"Score"]
			# contrasts(factormat,1)[profileObject$Products,]<-(as.matrix(tmpContrastVect))
			# resAnova=anova(lm(scoremat~factormat))
			# contrasts(canonicalData[canonicalData$AttributeCode==attribute,"ProductCode"],1)=contr.sum
			# Pr=pf(resAnova["factormat","Mean Sq"]/currentAnovaTable["Residuals","Mean Sq"],df1=1,df2=df2,lower.tail=FALSE)
			# pvalueMatrix[attribute,product]=Pr
			# ---------- fait la m?me chose qu'en dessous avec le summary contrasts -------------
			#dataSubset=profileObject$ExtendedData[,c("SubjectCode","ProductCode","Replicate",as.character(attribute))]
			#colnames(dataSubset)[4]="Y"

			#resLsmeans=lsmeans(lm(Y~ProductCode*SubjectCode,data=dataSubset),spec="ProductCode")
			resLsmeans=profileObject$Anova[[attribute]][[1]][["resLsmeans"]]
			tmpContrastVect=contrastVector
			tmpContrastVect[product]=-1
			resContrast=summary(contrast(resLsmeans,list(tmpContrastVect)),adjust="holm")
			Pr=resContrast$p.value
			if (Pr<=alphaContrast)
			{
				if(currentMean>=gMean)
				{
					style=AddHtmlStyle(style,attribute,product,backgroundColor="green")
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

	sortedProductIndex=sort(meanMatrix[firstAtt,],decreasing=FALSE,index.return=TRUE)$ix
	flash[,4:dim(flash)[2]]=flash[,sortedProductIndex+3]
	colnames(flash)=c("F","P(F)","GMEAN",as.character(profileObject$Products[sortedProductIndex]))
	sortedAttributeIndex=match(liste,as.character(listAttributes)) #inversion?
	flash=flash[sortedAttributeIndex,]
	colnames(flash)=c("F","P(F)","GMEAN",as.character(profileObject$Products[sortedProductIndex]))
	rownames(flash)=listAttributes[sortedAttributeIndex]

	# Output
	html=("<html><body>")
	html=paste(html, "<h2>","FlashTable", "</h2>")
	html=paste(html, "<p>","Similarity",": ", similarity, "</p>")
	html=paste(html, "<p>","ClassificationMethod",": ", classificationMethod, "</p>")
	html=paste(html, "<p>","MinimumProportionOfVarianceExplained",": ", explainedVariance, "</p>")
	html=paste(html, "<p>","ContrastTestAlpha",": ", alphaContrast, "</p>")
	html=paste(html, "<p>","VariableSelectionAlpha",": ",profileObject$alphaForSelectionOfSignificantAttributes , "</p>")
	html=paste(html, "<p>","Model",": ", model, "</p>")
	html=paste(html, MatrixToHtml(flash,style=style), sep="")
	html=paste(html,"<p style='color:green'>","FlashTableLegendGreen","</p>",sep="")
	html=paste(html,"<p style='color:yellow'>","FlashTableLegendYellow","</p>",sep="")
	html=paste(html,"</body></html>",sep="",collapse="")
	cat(html,file=paste(fileName,".html",sep=""), append=FALSE)

	return(flash)
}