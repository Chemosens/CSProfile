#' @importFrom CSUtils AnovaV2
profileSetUnivariateAnalysisParameters=function(profileObject, model, randomEffects="", anovaCalculationMode, lsMeansAdjustment, lsMeansAlpha)
{
	profileObject[["ANOVAModel"]]=model
	profileObject[["RandomSubject"]]="Subject" %in% randomEffects
	profileObject[["RandomSession"]]="Session" %in% randomEffects
	profileObject[["AnovaCalculationMode"]]=anovaCalculationMode
	profileObject[["LsMeansAlpha"]]=lsMeansAlpha
	profileObject[["LsMeansAdjustment"]]=lsMeansAdjustment

	if (nlevels(profileObject[["Products"]])<2)
	{
		profileObject[["Anova"]]=NULL
	} else
	{
		for(att in levels(profileObject[["Attributes"]]))
		{
			if(model=="TwoWayMultiplicative" && nlevels(factor(profileObject[["Replicates"]]))<2)
			{
				warning("The number of replicates is unsufficient for the Two-Way Multiplicative model. The model chosen is Two Way Additive")
				profileObject[["ANOVAModel"]]=model
				model="TwoWayAdditive";
			}

			X=profileObject[["CompleteExtendedDataWithoutNA"]][,c("SubjectCode","ProductCode","Replicate",att)]
			colnames(X)[4]="Y"
			# Replicates : facteur ordonné
			X$Replicate=as.factor(ordered(as.numeric(X$Replicate)))

			if(sum(!is.na(X[,"Y"]))>0)
			{
				if(length(levels(factor(as.character(X[!is.na(X[,"Y"]),"ProductCode"]))))>1) # si il y a plus d'un produit de noté
				{
				# nombre d'observations par sujet et par produit
					nbObsParProdSuj=aggregate(X[!is.na(X[,"Y"]),"Y"],by=list(X[!is.na(X[,"Y"]),"SubjectCode"],X[!is.na(X[,"Y"]),"ProductCode"]),length)[,"x"]
					if(sd(nbObsParProdSuj)==0&nbObsParProdSuj[1]>1) # cas ou tous les panelistes ont repondu pour au moins deux evaluations
					{
						if(sd(X[,"Y"],na.rm=TRUE)!=0)
						{
							res.Anova=NULL
							tryCatch({
							res.Anova=AnovaV2(dataFrame=X,model=model,randomSubject=profileObject[["RandomSubject"]],correlationStructure="",testRep="",lsMeansAlpha=lsMeansAlpha,lsMeansAdjustment=lsMeansAdjustment,varianceTest="None", normalityTest="None",anovaCalculationMode=anovaCalculationMode)
							},error=function(e)
							{
							 res.Anova=NULL
							 stop(e)
							})
						}
						else
						{
							res.Anova=NULL
							stop(paste("<p> ",att," was removed from the dataset: no variability in it, the average is ",mean(X[,"Y"],na.rm=TRUE)," for ",dim(X)[1]," observations</p>",sep=""))
							profileObject$Attributes=as.factor(as.character(profileObject$Attributes[profileObject$Attributes!=att]))
							profileObject$CompleteCanonicalDataWithoutNA=profileObject$CompleteCanonicalDataWithoutNA[profileObject$CompleteCanonicalDataWithoutNA[,"AttributeCode"]!=att,]
							profileObject$CompleteExtendedDataWithoutNA=profileObject$CompleteExtendedDataWithoutNA[,-which(colnames(profileObject$CompleteExtendedDataWithoutNA)==att)]

						}
					}
					else # cas ou les panelistes n'ont fait qu'une repetition de chaque produit (exemple de deux sessions différentes)
					{
					#	stop(paste("<p>","Warning, the dataset is not balanced for ",att,". The chosen option for ANOVA was Additive Two-way",sep=""))
						res.Anova=AnovaV2(dataFrame=X,model="TwoWayAdditive",randomSubject=profileObject[["RandomSubject"]],correlationStructure="",testRep="",lsMeansAlpha=lsMeansAlpha,lsMeansAdjustment=lsMeansAdjustment,varianceTest="None", normalityTest="None",anovaCalculationMode=anovaCalculationMode)
						profileObject[["ANOVAModel"]]="TwoWayAdditive"
					}
					if(!is.null(res.Anova)){profileObject[["Anova"]][[att]]=res.Anova	}else{profileObject[["Anova"]][[att]]=NA}
				}
				else
				{
				 stop(paste(att,": only one product is scored"))
				}
			}
			else
			{
				stop(paste(att," has only NA  values."))
			}
		}
	}

	return(profileObject)
}