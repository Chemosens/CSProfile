#'@importFrom CSUtils MatrixToHtml
#'@importFrom lsmeans contrast
#'@importFrom stats lm cor.test cor aggregate sd
CAP=function(object,panelLimit=0.05,indivRepeatabilityLimit=0.01,indivDiscriminationLimit=0.1,indivAgreementLimit=0.2,language="fr",output="CAP table",correction=TRUE,correlationTest="kendall",repInIndModel=FALSE)
{
 	correlationTest=tolower(correlationTest)

	vectorialIn=function(valeurs,vecteur)
	{
	  res=rep(FALSE,length(valeurs))

	  for(i in 1:length(valeurs))
	  {
	    res[i]=valeurs[i]%in%vecteur
	  }
	  return(res)
	}
	# Initialisations
	data=object$CompleteCanonicalDataWithoutNA
	tab2=data
  titre=paste("CAP Table")
	att=levels(as.factor(as.character(data[,"AttributeCode"])))
	attributeNumber=length(att)
	panelAverage=rep(NA,attributeNumber);names(panelAverage)=att
	panelProductF=rep(NA,attributeNumber);names(panelProductF)=att
	panelProductPvalue=rep(NA,attributeNumber);names(panelProductPvalue)=att
	panelInteractionF=rep(NA,attributeNumber);names(panelInteractionF)=att
	panelInteractionPvalue=rep(NA,attributeNumber);names(panelInteractionPvalue)=att
	panelRepeatability=rep(NA,attributeNumber);names(panelRepeatability)=att
	suj=levels(as.factor(as.character(data[,"SubjectCode"])))
	prod=levels(as.factor(data[,"ProductCode"]))
	productNumber=length(prod)
	repetitions=levels(as.factor(data[,"Replicate"]))
	replicateNumber=length(repetitions)
	subjectNumber=length(suj)
	individualProductF=matrix(NA,attributeNumber,subjectNumber);rownames(individualProductF)=att;colnames(individualProductF)=suj
	individualRepeatability=matrix(NA,attributeNumber,subjectNumber);rownames(individualRepeatability)=att;colnames(	individualRepeatability)=suj
	individualDisagreementPvalue=matrix(NA,attributeNumber,subjectNumber);rownames(individualDisagreementPvalue)=att;colnames(individualDisagreementPvalue)=suj
	individualDisagreementF=matrix(NA,attributeNumber,subjectNumber);rownames(individualDisagreementF)=att;colnames(individualDisagreementF)=suj
	individualProductPvalue=matrix(NA,attributeNumber,subjectNumber);rownames(individualProductPvalue)=att;colnames(individualProductPvalue)=suj
	significanceMatrix=matrix(FALSE,attributeNumber,subjectNumber);rownames(individualProductF)=att;colnames(individualProductF)=suj
	rankMatrix=matrix(NA,attributeNumber,subjectNumber);rownames(individualProductF)=att;colnames(individualProductF)=suj
	colnames(rankMatrix)=suj
	rownames(rankMatrix)=att
	FRank=rep(NA,subjectNumber)
	if(correction)
	{
		indivDiscriminationLimit=indivDiscriminationLimit/subjectNumber
	}
	if(object$ANOVAModel=="TwoWayMultiplicative")
	{
		for (i in  1:attributeNumber)
		{

			resAnova=object$Anova[[att[i]]]
			dataI=data[data[,"AttributeCode"]==att[i],]
			panelAverage[i]=mean(dataI[!is.na(dataI[,"Score"]),"Score"])
			if(!is.na(resAnova))
			{
				panelRepeatability[i]=resAnova[[1]]$RMSE
			}
			else
			{
				panelRepeatability[i]=0
			}
			if(!is.na(panelRepeatability[i])&panelRepeatability[i]>0.0000001)
			{
				panelProductF[i]=resAnova[[1]]$FProd
				panelProductPvalue[i]=resAnova[[1]]$PProd
				panelInteractionF[i]=resAnova[[1]]$FProdSubj
				panelInteractionPvalue[i]=resAnova[[1]]$PProdSubj
			} else # cas des ajustements parfaits
			{
				panelProductF[i]=NA
				panelInteractionF[i]=NA
				panelProductPvalue[i]=NA
				panelInteractionPvalue[i]=NA
			}
			dataEvent=dataI
			for(j in 1:subjectNumber)
			{

				# Discrimination matrix
				dataIj=dataI[dataI[,"SubjectCode"]==suj[j],]
				dataFrameIj=dataIj[,c("SubjectCode","ProductCode","Replicate","Score")]
				colnames(dataFrameIj)[4]="Y"
				if(repInIndModel)
				{
					anovaResults2=AnovaV2(dataFrame=dataFrameIj,model="TwoWayAdditiveBySubject",correlationStructure="",testRep="",lsMeansAlpha=object$LsMeansAlpha, lsMeansAdjustment=object$LsMeansAdjustment,randomSubject=object$RandomSubject,anovaCalculationMode=object$AnovaCalculationMode)
				} else
				{
					anovaResults2=AnovaV2(dataFrame=dataFrameIj,model="OneWayBySubject",correlationStructure="",testRep="",lsMeansAlpha=object$LsMeansAlpha, lsMeansAdjustment=object$LsMeansAdjustment,randomSubject=object$RandomSubject,anovaCalculationMode=object$AnovaCalculationMode)
				}


					if(anovaResults2[[1]]$RMSE>(2e-5)) # si ajustement non parfait
					{
						individualProductF[i,j]=anovaResults2[[1]]$FProd
						individualProductPvalue[i,j]=anovaResults2[[1]]$PProd
						repetable1=aggregate(dataEvent[,"Score"],list(dataEvent[,"ProductCode"],dataEvent[,"SubjectCode"]),sd)
						colnames(repetable1)=c("ProductCode","SubjectCode","var")
						resLsmeans=lsmeans::lsmeans(lm(var~ProductCode+SubjectCode,data=repetable1),spec="SubjectCode")
						tmpContrastVect=rep(1/(subjectNumber-1),subjectNumber)
						names(tmpContrastVect)=suj
						tmpContrastVect[suj[j]]=-1
						resContrast=summary(contrast(resLsmeans,list(tmpContrastVect)),adjust="holm")
						Pr=resContrast$p.value
						individualRepeatability[i,j]=Pr
					} else
					{
						if(anovaResults2[[1]]$RMSE>(2e-16)) # si ajustement parfait et moyennes produits diff?rentes
						{
							individualProductF[i,j]=NA
							individualProductPvalue[i,j]=0
						}
						if(anovaResults2[[1]]$RMSE<(2e-16)) # si ajustement parfait et moyennes produits diff?rentes
						{
							individualProductF[i,j]=NA
							individualProductPvalue[i,j]=NA
						}
						individualRepeatability[i,j]=1
					}


			}
		}

		names(panelProductF)=att
		print(panelProductF)
		names(panelInteractionF)=att
		names(panelProductPvalue)=att
		names(panelInteractionPvalue)=att
		#obtention des rankf
		individualProductF2=max(individualProductF[!is.na(individualProductF)])-individualProductF
		rankMatrix=apply(individualProductF2,1,rank,na.last="keep")

		colnames(rankMatrix)=att
		rownames(rankMatrix)=suj

		for(j in 1:subjectNumber)
		{
			FRank[j]=mean(rankMatrix[j,],na.rm=TRUE)
		}
		names(FRank)=suj
		rownames(individualProductPvalue)=att
		colnames(individualProductPvalue)=suj

		significanceMatrix=(individualProductPvalue<indivDiscriminationLimit)
		colnames(significanceMatrix)=suj
		rownames(significanceMatrix)=att
		significanceMatrix[is.na(significanceMatrix)]=FALSE
	   # matrice d'accord : test de kendall
		for(i in 1:attributeNumber)
		{
			for(j in 1:subjectNumber)
			{
				subjectData=data[data[,"SubjectCode"]==suj[j]&data[,"AttributeCode"]==att[i],]
				resu=aggregate(subjectData[,"Score"],list(subjectData[,"ProductCode"],subjectData[,"SubjectCode"]),mean)
				colnames(resu)=c("ProductCode","SubjectCode", "moy")
				prod1=resu[,"ProductCode"]

				if(length(suj[significanceMatrix[i,]])>1) # on compte le nombre de sujets discriminants
				{
					# if(replicateNumber>1)
					# {
						meanVec=data[data[,"SubjectCode"]!=suj[j]&data[,"AttributeCode"]==att[i]&vectorialIn(data[,"SubjectCode"],suj[significanceMatrix[i,]]),]
					# } else
					# {
					#	meanVec=data[data[,"SubjectCode"]!=suj[j]&data[,"AttributeCode"]==att[i],]
					# }

					resultVec=rep(0,productNumber)
					for(k in 1:productNumber)
					{
						resultVec[k]=mean(meanVec[meanVec[,"ProductCode"]==prod1[k],"Score"])
					}

					names(resultVec)=prod1
					if(productNumber>2)
					{
						test.cor=cor.test(resu[,"moy"],resultVec,method=correlationTest,alternative="greater")
						individualDisagreementPvalue[i,j]=test.cor$p.value
						individualDisagreementF[i,j]=test.cor$estimate
					}
					else
					{
						individualDisagreementPvalue[i,j]=NA
						individualDisagreementF[i,j]=cor(resu[,"moy"],resultVec,method=correlationTest)
					}
				} else # si un seul sujet est discriminant, il est d'accord avec lui meme...
				{
				  individualDisagreementPvalue[i,j]=0
				  individualDisagreementF[i,j]=NA
				}
			}
		}

		colnames(individualDisagreementPvalue)=suj
		rownames(individualDisagreementPvalue)=att
		colnames(individualDisagreementF)=suj
		rownames(individualDisagreementF)=att
		colnames(individualRepeatability)=suj
		rownames(individualRepeatability)=att
		colnames(individualProductF)=suj
		rownames(individualProductF)=att

		# TRI
		ind=order(FRank,na.last=T)

		indice=(1:subjectNumber)[ind]
		suj=suj[ind]

		# on retrie tout dans l'ordre...'
		FRank=FRank[ind]

		ind2=order(panelProductF,decreasing=TRUE,na.last=T)
		att=att[ind2]
		panelProductF=panelProductF[ind2]
		panelProductPvalue=panelProductPvalue[ind2]
		panelInteractionF=panelInteractionF[ind2]
		panelInteractionPvalue=panelInteractionPvalue[ind2]
		panelAverage=panelAverage[ind2]
		panelRepeatability=panelRepeatability[ind2]

		individualProductF= individualProductF[ind2,ind]
		individualProductPvalue=individualProductPvalue[ind2,ind]
		significanceMatrix=significanceMatrix[ind2,ind]
		individualRepeatability=individualRepeatability[ind2,ind]
		individualDisagreementPvalue=individualDisagreementPvalue[ind2,ind]
		individualDisagreementF=individualDisagreementF[ind2,ind]
		resultingList=list(panelProductF,panelProductPvalue,panelInteractionF,panelInteractionPvalue, panelAverage,panelRepeatability,individualProductF,individualProductPvalue,
		significanceMatrix,individualRepeatability,individualDisagreementPvalue,individualDisagreementF)
		names(resultingList)=c("panelProductF","panelProductPvalue","panelInteractionF","panelInteractionPvalue", "panelAverage","panelRepeatability","individualProductF","individualProductPvalue",
		"significanceMatrix","individualRepeatability","individualDisagreementPvalue","individualDisagreementStat")
	}
	#-------------------------------------
	# Cas ou on n'a pas de r?p?titions...
	#-------------------------------------
	if(object$ANOVAModel=="TwoWayAdditive")
	{
		for (i in  1:attributeNumber)
		{
			resAnova=object$Anova[[att[i]]]

			dataI=data[data[,"AttributeCode"]==att[i],]
			panelAverage[i]=mean(dataI[!is.na(dataI[,"Score"]),"Score"])
			if(!is.na(resAnova[[1]]))
			{
				panelRepeatability[i]=resAnova[[1]]$RMSE
			}
			else
			{
				panelRepeatability[i]=0
			}
			if(!is.na(panelRepeatability[i])&panelRepeatability[i]!=0)
			{
				panelProductF[i]=resAnova[[1]]$FProd
				panelProductPvalue[i]=resAnova[[1]]$PProd
			} else # cas des ajustements parfaits
			{
				panelProductF[i]=NA
				panelProductPvalue[i]=NA
			}
			for(j in 1:subjectNumber)
			{

				subjectData=data[data[,"SubjectCode"]==suj[j]&data[,"AttributeCode"]==att[i],]
					resu=aggregate(subjectData[,"Score"],list(subjectData[,"ProductCode"],subjectData[,"SubjectCode"]),mean)
					colnames(resu)=c("ProductCode","SubjectCode", "moy")
					prod1=resu[,"ProductCode"]

					# correlation des moyennes du sujet aux moyenne du reste du panel
					meanVec=data[data[,"SubjectCode"]!=suj[j]&data[,"AttributeCode"]==att[i],]
					resultVec=rep(0,length(prod));	names(resultVec)=prod1
					for(k in 1:productNumber)
					{
						resultVec[k]=mean(meanVec[meanVec[,"ProductCode"]==prod1[k],"Score"],na.rm=TRUE)
					}
					if(productNumber>2)
					{

						if(sum(!is.na(resu[,"moy"]))>1) # si on a au moins deux observations non NA
						{
							# cas o? on n'a pas de variabilit?: on met un tiret dans la case
							if(sd(resu[,"moy"])==0)
							{
							individualDisagreementF[i,j]=-2
							}
							else
							{
							test.cor=cor.test(resu[,"moy"],resultVec[resu[,"ProductCode"]],method=correlationTest,alternative="greater")
							individualDisagreementPvalue[i,j]=test.cor$p.value
							individualDisagreementF[i,j]=test.cor$estimate
							}
						}
						else
						{
							individualDisagreementPvalue[i,j]=NA
							individualDisagreementF[i,j]=NA

						}
					}
					else
					{
						individualDisagreementPvalue[i,j]=NA
						if(sd(subjectData[,"Score"],na.rm=T)!=0) # si des diff?rences existent
						{
							individualDisagreementF[i,j]=cor(resu[,"moy"],resultVec,method=correlationTest)
						}
						else
						{
							individualDisagreementPvalue[i,j]=NA
							individualDisagreementF[i,j]=NA
						}
					}

			}
			#  pour obtenir des couleurs dans la table, on attribue la valeur de 0 aux p values de #discrimination individuelles, et 1 ? celles de r?p?tabilit?
			individualProductPvalue=matrix(0,attributeNumber,subjectNumber)
			individualRepeatability=matrix(1,attributeNumber,subjectNumber)
		}

		moyenneStatAg=apply(individualDisagreementF,2,'mean',na.rm=TRUE)
		ind=order(moyenneStatAg,decreasing=TRUE,na.last=T)
		ind2=order(panelProductF,decreasing=TRUE,na.last=T)
		att=att[ind2]
		suj=suj[ind]

		panelAverage=panelAverage[ind2]
		panelProductF=panelProductF[ind2]
		panelProductPvalue=panelProductPvalue[ind2]
		panelRepeatability=panelRepeatability[ind2]
		individualDisagreementPvalue=individualDisagreementPvalue[ind2,ind]
		individualDisagreementF=individualDisagreementF[ind2,ind]
		individualProductPvalue=individualProductPvalue[ind2,ind]
		individualProductF=individualProductF[ind2,ind]

		resultingList=list(panelProductF,panelProductPvalue, panelAverage,panelRepeatability,
		individualDisagreementPvalue,individualDisagreementF)
		names(resultingList)=c("panelProductF","panelProductPvalue", "panelAverage","panelRepeatability","individualDisagreementPvalue","individualDisagreementStat")
	}


	#########################
	# OBTENTION DES STYLES DE LA TABLE
	########################individualProductPvalue

	#panel
	style=NULL
	indices=which(panelProductPvalue<=panelLimit,arr.ind=TRUE) # colonne FProd
	if (length(indices) > 0)
	{
	  for (i in 1:length(indices))
	  {
	    style=AddHtmlStyle(style,att[indices[i]],as.character("FProd"),backgroundColor="#00FF00")
	  }
	}
	indices=which(panelProductPvalue>panelLimit,arr.ind=TRUE)
	if (length(indices) > 0)
	{
	  for (i in 1:length(indices))
	  {
	    style=AddHtmlStyle(style,att[indices[i]],as.character("FProd"),backgroundColor="red")
	  }
	}

	indices=which(panelInteractionPvalue<=panelLimit,arr.ind=TRUE) # colonne FProd
	if (length(indices) > 0)
	{
	  for (i in 1:length(indices))
	  {
	    style=AddHtmlStyle(style,att[indices[i]],as.character("FDisag"),backgroundColor="red")
	  }
	}
	indices=which(panelInteractionPvalue>panelLimit,arr.ind=TRUE)
	if (length(indices) > 0)
	{
	  for (i in 1:length(indices))
	  {
	    style=AddHtmlStyle(style,att[indices[i]],as.character("FDisag"),backgroundColor="#00FF00")
	  }
	}
	# sujets
	indices=which(individualProductPvalue<=indivDiscriminationLimit&individualDisagreementPvalue<=indivAgreementLimit,arr.ind=TRUE)
	if (length(indices) > 0)
	{
	  if(is.vector(indices)){nbIndices=length(indices)}else{nbIndices=nrow(indices)}

		for (i in 1:nbIndices)
		{
		 if(is.vector(indices)){ind1=1;ind2=indices[i]}else{ind1=indices[i,1];ind2=indices[i,2]}
			style=AddHtmlStyle(style,att[ind1],suj[ind2],backgroundColor="#00FF00")
		}

	}
	indices=which(individualProductPvalue<=indivDiscriminationLimit&individualDisagreementPvalue>indivAgreementLimit,arr.ind=TRUE)
	if (length(indices) > 0)
	{
		if(is.vector(indices)){nbIndices=length(indices)}else{nbIndices=nrow(indices)}

	  for (i in 1:nbIndices)
	  {
	    if(is.vector(indices)){ind1=1;ind2=indices[i]}else{ind1=indices[i,1];ind2=indices[i,2]}
		style=AddHtmlStyle(style,att[ind1],suj[ind2],backgroundColor="red")
	  }
	}
	indices=which(individualProductPvalue>indivDiscriminationLimit,arr.ind=TRUE)
	if (length(indices) > 0)
	{
		if(is.vector(indices)){nbIndices=length(indices)}else{nbIndices=nrow(indices)}
		  for (i in 1:nbIndices)
		  {
		  if(is.vector(indices)){ind1=1;ind2=indices[i]}else{ind1=indices[i,1];ind2=indices[i,2]}
			style=AddHtmlStyle(style,att[ind1],suj[ind2],backgroundColor="yellow")
		   }

	}

	# cas de deux produits : pas de individual disagreement pvalue
	 indices=which(individualProductPvalue<indivDiscriminationLimit&is.na(individualDisagreementPvalue)&!is.na(individualDisagreementF)&individualDisagreementF>0,arr.ind=TRUE)
	 if (length(indices) > 0)
	 {
			if(is.vector(indices)){nbIndices=length(indices)}else{nbIndices=nrow(indices)}
			for (i in 1:nbIndices)
			{
			 if(is.vector(indices)){ind1=1;ind2=indices[i]}else{ind1=indices[i,1];ind2=indices[i,2]}
			style=AddHtmlStyle(style,att[ind1],suj[ind2],backgroundColor="#00FF00")
			}
	 }

	  indices=which(individualProductPvalue<indivDiscriminationLimit&is.na(individualDisagreementPvalue)&!is.na(individualDisagreementF)&individualDisagreementF<0,arr.ind=TRUE)
	 if (length(indices) > 0)
	 {
	   if(is.vector(indices)){nbIndices=length(indices)}else{nbIndices=nrow(indices)}
	   for (i in 1:nbIndices)
	  {
	  if(is.vector(indices)){ind1=1;ind2=indices[i]}else{ind1=indices[i,1];ind2=indices[i,2]}
	     style=AddHtmlStyle(style,att[ind1],suj[ind2],backgroundColor="red")
	  }

	 }
	  indices=which(individualProductPvalue<indivDiscriminationLimit&is.na(individualDisagreementPvalue)&is.na(individualDisagreementF),arr.ind=TRUE)
	  if (length(indices) > 0)
	  {
		if(is.vector(indices)){nbIndices=length(indices)}else{nbIndices=nrow(indices)}
	    for (i in 1:nbIndices)
	   {
			if(is.vector(indices)){ind1=1;ind2=indices[i]}else{ind1=indices[i,1];ind2=indices[i,2]}
	      style=AddHtmlStyle(style,att[ind1],suj[ind2],backgroundColor="orange")
	   }

	  }

	# cas problematique : pas de pvalue produit
	indices=which(is.na(individualProductPvalue),arr.ind=TRUE)
	if (length(indices) > 0)
	{
	  if(is.vector(indices)){nbIndices=length(indices)}else{nbIndices=nrow(indices)}
	  for (i in 1:nbIndices)
	  {
		if(is.vector(indices)){ind1=1;ind2=indices[i]}else{ind1=indices[i,1];ind2=indices[i,2]}
	    style=AddHtmlStyle(style,att[ind1],suj[ind2],backgroundColor="orange")
	  }
	}
	tableauTxt=matrix("",attributeNumber,subjectNumber)

	texteCellule=function(indivDiscrimination,indivDiscriminationLimit,indivAgreement,indivAgreementLimit,indivRepeatability,indivRepeatabilityLimit,txt=NULL)
	{

		if(is.na(indivDiscrimination))
		{	cell="0"}
		else
		{
			if(is.null(txt))
			{
				if(indivDiscrimination<indivDiscriminationLimit)
				{
					if(!is.na(indivAgreement))
					{
						if(indivAgreement<indivAgreementLimit){cell="+"}
						if(indivAgreement>=indivAgreementLimit){cell="-"}
					}
					else{cell="!"}
				}
				if(!is.na(indivDiscrimination) && indivDiscrimination>=indivDiscriminationLimit){cell="="}
			}
			else
			{
				cell=txt
			}
		}
		if(!is.na(indivRepeatability) && indivRepeatability<indivRepeatabilityLimit){cell=paste(cell,"LR")}
		return(cell)
	}



	if(object$ANOVAModel=="TwoWayMultiplicative")
	{
		for(j in 1:subjectNumber)
		{
			if(attributeNumber>1)
			{
			 for(i in 1:attributeNumber)
				{
				tableauTxt[i,j]=texteCellule(individualProductPvalue[i,j],indivDiscriminationLimit,individualDisagreementPvalue[i,j],indivAgreementLimit,individualRepeatability[i,j],indivRepeatabilityLimit)
				}
			}
			else
			{
			tableauTxt[1,j]=texteCellule(individualProductPvalue[j],indivDiscriminationLimit,individualDisagreementPvalue[j],indivAgreementLimit,individualRepeatability[j],indivRepeatabilityLimit)

			}

		}
		tableauRes=cbind(sprintf("%.2f",panelAverage), sprintf("%.2f",panelProductF),sprintf("%.2f",panelInteractionF),sprintf("%.2f",panelRepeatability),tableauTxt)
		# ligne des rangs
		tableauRes=rbind(tableauRes,c("-","-","-","-",as.character(sprintf("%.2f",FRank))))
		colnames(tableauRes)=c("Mean","FProd","FDisag","RMSE",suj)
		rownames(tableauRes)=c(att,"RankF")
	}
	if(object$ANOVAModel=="TwoWayAdditive")
	{
		for(j in 1:subjectNumber)
		{
			if(attributeNumber==1)
			{
				if(!is.na(individualDisagreementF[j])&individualDisagreementF[j]==-2)
				{
					txtToWrite="-"
				}
				else
				{
					txtToWrite=sprintf("%.2f",individualDisagreementF[j])
				}

				tableauTxt[i,j]=texteCellule(individualProductPvalue[j],indivDiscriminationLimit,individualDisagreementPvalue[j],indivAgreementLimit,individualRepeatability[j],indivRepeatabilityLimit,txt=txtToWrite)
			}
			else
			{
			   for(i in 1:attributeNumber)
			  {
				if(!is.na(individualDisagreementF[i,j])&individualDisagreementF[i,j]==-2)
				{
					txtToWrite="-"
				}
				else
				{
					txtToWrite=sprintf("%.2f",individualDisagreementF[i,j])
				}

				tableauTxt[i,j]=texteCellule(individualProductPvalue[i,j],indivDiscriminationLimit,individualDisagreementPvalue[i,j],indivAgreementLimit,individualRepeatability[i,j],indivRepeatabilityLimit,txt=txtToWrite)
			  }

			}

		}
		tableauRes=cbind(sprintf("%.2f",panelAverage), sprintf("%.2f",panelProductF),sprintf("%.2f",panelRepeatability),tableauTxt)
		colnames(tableauRes)=c("Mean","FProd","RMSE",as.character(suj))
		rownames(tableauRes)=c(att)
	}

	txt="<html>"
	txt=paste(txt, "<h3>",titre,"</h3>",MatrixToHtml(tableauRes, style=style), sep="")


	#############################
	# Writing table and legend
	##############################
	txt="<html>"
	txt=paste(txt, "<h3>",titre,"</h3>",MatrixToHtml(tableauRes, style=style), sep="")
	if(object$ANOVAModel=="TwoWayMultiplicative")
	{
		txt=paste(txt,"<table border='1' cellspacing='1'><tr><td colspan='2' style='text-align:center'><b>","Panel Performances","</b></td><td style='text-align:center'><b>","PanelistPerf","</b>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;</td></tr>")
		txt=paste(txt,"<tr><td style='text-align:center'><b>","FProd","</b></td><td style='text-align:center'><b>","FDisag","</b></td><td rowspan='3'>&nbsp;<span style='background-color:#00FF00'> + </span> ","Discriminant And Agreement","<br/> <span style='background-color:#FF0000'> - </span> ","DiscriminantAndDisagreement","<br/> <span style='background-color:yellow'> = </span> ","Not Discriminant","<br/> LR: ","Less Repeatable","</td></tr>",sep="")
		txt=paste(txt,"<tr><td style='text-align:center;background-color:#FF0000'>p> ",panelLimit,"</td><td style='text-align:center;background-color:#FF0000'>p<",panelLimit ,"</td></tr>")
		txt=paste(txt,"<tr><td style='text-align:center;background-color:#00FF00'>p< ",panelLimit,"</td><td style='text-align:center;background-color:#00FF00'>p>",panelLimit,"</td></tr>")
		#txt=paste(txt,"<tr><td></td><td></td><td>",TS_GetLabel("LegendRepet",language),"</td></tr>")

	txt=paste(txt,"</table><br>")
#	txt=paste(txt,TS_GetLabel("PhraseNA",language))
	if(productNumber==2){txt=paste(txt,"<p>","! means that the subject was discriminant but the correlation test was not computable  with 2 products","</p>")}
	txt=paste(txt,"<p>","Panel discrimination limit:",panelLimit,"</p>")
	txt=paste(txt,"<p>","Panel agreement limit:",panelLimit,"</p>")
	txt=paste(txt,"<p>","Individual discrimination limit:",sprintf("%.2f",indivDiscriminationLimit),"</p>")
	txt=paste(txt,"<p>","Individual repeatability limit:",sprintf("%.2f",indivRepeatabilityLimit),"</p>")
	}
	if(object$ANOVAModel=="TwoWayAdditive")
	{
		txt=paste(txt,"<p>","Panel discrimination limit:",panelLimit,"</p>")
		 txt=paste(txt,"legend", " (",correlationTest,")",sep="")
	}
	txt=paste(txt,"<p>","Individual agreement limit:",indivAgreementLimit,"</p>")
	txt=paste(txt,"<p>","- indicates no variability for the panelist scores, and NA indicates missing values or computational issues.","</p>")
	txt=paste(txt,"</html>")

	write(txt,file=paste(output,".html",sep=""))
	return(resultingList)
}


