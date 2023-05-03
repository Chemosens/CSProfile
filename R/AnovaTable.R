# Cr?e un tableau des r?sultats d'ANOVA sur un tableau de donn?es au format ?tendu
#'@importFrom CSUtils Asterisks
#' @importFrom stats dnorm manova
AnovaTable=function(extendedData,defaultAnova=NULL, model="ThreeWayMultiplicativeRepeated", randomSubject=T, correlationStructure="AR1", testRep="EachRepVsPrevious", lsMeansAlpha=0.10, lsMeansAdjustment="Tukey", anovaCalculationMode="Ols", show=c("Groups","StatMANOVA","F", "PValues","RMSE", "VarianceTest", "NormalityTest","GMean"), fileName="ANOVATable",varianceTest="None", normalityTest="None",orderByF=TRUE)
{
	## Cr?ation du tableau Anova
	defaultModel=FALSE
	if (!is.null(defaultAnova))
	{
		defaultModel=TRUE
	}
	ProductCode=NULL

	attributs=colnames(extendedData)
	attributs=attributs[!attributs%in%c("Replicate","ProductCode","SubjectCode","Session")]
	products=unique(as.character(extendedData$ProductCode))
	replicates=unique(as.character(extendedData$Replicate))
	subjects=unique(as.character(extendedData$SubjectCode))
	newCol=rep(NA,length(attributs))
	meanAtt=rep(NA,length(attributs));names(meanAtt)=attributs

	# cas ou on a choisi une anova par sujet et une seule rep
	if(!is.null(model))
	{
		if(model=="OneWayBySubject"&length(replicates)==1)
		{
			show=c("GMean")
			warning("Warnings: the model 'OneWayBySubject' has no meaning with only one replicate")
		}
	}else
	{
	stop("Running ANOVA is not possible with your data. Model is missing.")
	}

	meanWith=function(res.lsmean, stdError=FALSE, group=FALSE)
	{
		res=sprintf("%.2f",res.lsmean$lsmean)
		se=""
		letter=""
		if (stdError==FALSE && group==FALSE)
		{
			return (res)
		}
		if (stdError==TRUE)
		{
			se=sprintf("%.2f",res.lsmean[["SE"]])
			res=paste(res,"+-",se)
		}
		if (group==TRUE)
		{
			group=res.lsmean[[".group"]]
			gg=strsplit(group,"")
			for (ggg in gg[[1]])
			{
				if (ggg!=" ")
				{
					letter=paste(letter,letters[as.numeric(ggg)],sep="")
				}
			}
			res=paste(res,"(",letter,")",sep="")
		}
		return (res)
	}

	res=list()
	res2=list()

	listNames=NULL
	for (att in attributs)
	{

		X=extendedData[,c("SubjectCode","ProductCode","Replicate",att)]
		colnames(X)[4]="Y"
		# Replicates : facteur ordonn?
		X$Replicate=as.factor(ordered(as.numeric(X$Replicate)))

		if(!defaultModel)
		{
			if(!"NormalityTest" %in% show){normalityTest="None"}
			if(!"VarianceTest" %in% show){varianceTest="None"}
			res.Anova=AnovaV2(dataFrame=X,model=model,randomSubject=randomSubject,correlationStructure=correlationStructure,testRep=testRep,lsMeansAlpha=lsMeansAlpha,lsMeansAdjustment=lsMeansAdjustment,varianceTest=varianceTest, normalityTest=normalityTest,anovaCalculationMode=anovaCalculationMode)
		} else
		{
			res.Anova=defaultAnova[[att]]
			normalityTest="None"
			varianceTest="None"
		}
		res2[[att]]=res.Anova


		for (i in 1:length(res.Anova))
		{

			if(all(is.na(res.Anova[[i]])))
			{
				for (p in products)
				{
					res[[i]][att,p]=mean(X[X[,"ProductCode"]==p,"Y"],na.rm=T)
					if("GMean" %in% show){res[[i]][att,"GMean"]=mean(X[,"Y"],na.rm=T)}
				}
			}
			else
			{

				if (!is.null(res.Anova[[i]][["Name"]]) && !res.Anova[[i]][["Name"]] %in% listNames)
				{
					listNames=c(listNames,res.Anova[[i]][["Name"]])
				}

				if (length(res)<i)
				{
					res[[i]]=data.frame(row.names=attributs)
				}
				if(model!="OneWayBySubject")
				{
					if("GMean" %in% show){res[[i]][att,"GMean"]=sprintf("%.2f",mean(X[,"Y"],na.rm=TRUE))}
				}
				if(model=="OneWayBySubject")
				{
					if("GMean" %in% show){res[[i]][att,"GMean"]=sprintf("%.2f",mean(res.Anova[[i]]$Data[,"Y"],na.rm=TRUE))}
				}
				if (!is.null(res.Anova[[i]][["FProd"]]) && !is.na(res.Anova[[i]][["FProd"]]))
				{

					if (!("FProd" %in% colnames(res[[i]])))
					{
						# Cr?ation des colonnes produit
						FProd=PProd=DiagProd=newCol
						res[[i]]=cbind(res[[i]],FProd,PProd,DiagProd)
						for (p in products)
						{
							res[[i]]=cbind(res[[i]],newCol)
							colnames(res[[i]])[ncol(res[[i]])]=p
						}
					}

					res[[i]][att,"FProd"]=sprintf("%.2f",res.Anova[[i]][["FProd"]])
					res[[i]][att,"PProd"]=sprintf("%.3f",res.Anova[[i]][["PProd"]])
					if(!is.null(res.Anova[[i]][["DiagProd"]]))
					{
					  res[[i]][att,"DiagProd"]=res.Anova[[i]][["DiagProd"]]
					}

					for (p in products)
					{
						tmp=subset(res.Anova[[i]][["MoyProd"]],ProductCode==p)
						if(!is.null(tmp$lsmean))
						{
						  res[[i]][att,p]=sprintf("%.2f",tmp$lsmean)
						  showGroup="Groups" %in% show && res[[i]][att,"PProd"]<=lsMeansAlpha
						  showStdError="StdError" %in% show
						  res[[i]][att,p]=meanWith(res.lsmean=tmp,stdError=showStdError,group=showGroup)
						}
							# if ("Groups" %in% show && res[[i]][att,"PProd"]<=lsMeansAlpha)
						# {
							# res[[i]][att,p]=meanWithGroup(tmp)
						# }
					}

				}

				if (!is.null(res.Anova[[i]][["FRep"]]) && !is.na(res.Anova[[i]][["FRep"]]))
				{
					if (!("FRep" %in% colnames(res[[i]])))
					{
						# Cr?ation des colonnes rep
						FRep=PRep=DiagRep=newCol
						res[[i]]=cbind(res[[i]],FRep,PRep,DiagRep)
						for (r in replicates)
						{
							res[[i]]=cbind(res[[i]],newCol)
							colnames(res[[i]])[ncol(res[[i]])]=paste("Rep.",r,sep="")
						}
					}
					res[[i]][att,"FRep"]=sprintf("%.2f",res.Anova[[i]][["FRep"]])
					res[[i]][att,"PRep"]=sprintf("%.3f",res.Anova[[i]][["PRep"]])
					res[[i]][att,"DiagRep"]=res.Anova[[i]][["DiagRep"]]
					for (r in replicates)
					{

						tmp=subset(res.Anova[[i]][["MoyRep"]],Replicate==r)

						 if(dim(tmp)[1]>0) # si pas de pb de desequilibre
						 {
							#res[[i]][att,r]=sprintf("%.2f",tmp$lsmean)
							showGroup="Groups" %in% show && res[[i]][att,"PRep"]<=lsMeansAlpha
							showStdError="StdError" %in% show
							res[[i]][att,paste("Rep.",r,sep="")]=meanWith(res.lsmean=tmp,stdError=showStdError,group=showGroup)
						 }
						 else
						 {
							 res[[i]][att,paste("Rep.",r,sep="")]=NA
						 }
						 # if ("Groups" %in% show && res[[i]][att,"PRep"]<=lsMeansAlpha)
						 # {
							 # res[[i]][att,paste("Rep.",r,sep="")]=meanWith(tmp)
						 # }


					}

				}

				if (!is.null(res.Anova[[i]][["FSubj"]]) && !is.na(res.Anova[[i]][["FSubj"]]))
				{
					if (!("FSubj" %in% colnames(res[[i]])))
					{
						# Cr?ation des colonnes sujet
						FSubj=PSubj=DiagSubj=newCol
						res[[i]]=cbind(res[[i]],FSubj,PSubj,DiagSubj)
					}
					res[[i]][att,"FSubj"]=sprintf("%.2f",res.Anova[[i]][["FSubj"]])
					res[[i]][att,"PSubj"]=sprintf("%.3f",res.Anova[[i]][["PSubj"]])
					res[[i]][att,"DiagSubj"]=res.Anova[[i]][["DiagSubj"]]

				}

				if (!is.null(res.Anova[[i]][["FProdRep"]]) && !is.na(res.Anova[[i]][["FProdRep"]]))
				{
					if (!("FProdRep" %in% colnames(res[[i]])))
					{
						# Cr?ation des colonnes ProdRep
						FProdRep=PProdRep=DiagProdRep=newCol
						res[[i]]=cbind(res[[i]],FProdRep,PProdRep,DiagProdRep)
					}
					res[[i]][att,"FProdRep"]=sprintf("%.2f",res.Anova[[i]][["FProdRep"]])
					res[[i]][att,"PProdRep"]=sprintf("%.3f",res.Anova[[i]][["PProdRep"]])
					res[[i]][att,"DiagProdRep"]=res.Anova[[i]][["DiagProdRep"]]
				}

				if (!is.null(res.Anova[[i]][["FProdSubj"]]) && !is.na(res.Anova[[i]][["FProdSubj"]]))
				{
					if (!("FProdSubj" %in% colnames(res[[i]])))
					{
						# Cr?ation des colonnes ProdSubj
						FProdSubj=PProdSubj=DiagProdSubj=newCol
						res[[i]]=cbind(res[[i]],FProdSubj,PProdSubj,DiagProdSubj)
					}
					res[[i]][att,"FProdSubj"]=sprintf("%.2f",res.Anova[[i]][["FProdSubj"]])
					res[[i]][att,"PProdSubj"]=sprintf("%.3f",res.Anova[[i]][["PProdSubj"]])
					res[[i]][att,"DiagProdSubj"]=res.Anova[[i]][["DiagProdSubj"]]
				}

				if (!is.null(res.Anova[[i]][["FSubjRep"]]) && !is.na(res.Anova[[i]][["FSubjRep"]]))
				{
					if (!("FRepSubj" %in% colnames(res[[i]])))
					{
						# Cr?ation des colonnes ProdSubj
						FRepSubj=PRepSubj=DiagRepSubj=newCol
						res[[i]]=cbind(res[[i]],FRepSubj,PRepSubj,DiagRepSubj)
					}
					res[[i]][att,"FRepSubj"]=sprintf("%.2f",res.Anova[[i]][["FSubjRep"]])
					res[[i]][att,"PRepSubj"]=sprintf("%.3f",res.Anova[[i]][["PSubjRep"]])
					res[[i]][att,"DiagRepSubj"]=res.Anova[[i]][["DiagSubjRep"]]
				}

				if (!is.null(res.Anova[[i]][["RMSE"]]) && "RMSE" %in% show)
				{
					if (!("RMSE" %in% colnames(res[[i]])))
					{
						# Cr?ation de la colonne RMSE
						RMSE=newCol
						res[[i]]=cbind(res[[i]],RMSE)
					}
					res[[i]][att,"RMSE"]=sprintf("%.2f",res.Anova[[i]][["RMSE"]])
				}

				if (varianceTest!="None")
				{
					if(!is.null(res.Anova[[i]][["VarianceTestResult"]][["Product"]]) && !is.na(res.Anova[[i]][["VarianceTestResult"]][["Product"]]))
					{
						res[[i]][att,"TestVarProd"]=res.Anova[[i]][["VarianceTestResult"]][["Product"]]
					}
					if(!is.null(res.Anova[[i]][["VarianceTestResult"]][["Subject"]]) && !is.na(res.Anova[[i]][["VarianceTestResult"]][["Subject"]]))
					{
						res[[i]][att,"TestVarSubj"]=res.Anova[[i]][["VarianceTestResult"]][["Subject"]]
					}
					if(!is.null(res.Anova[[i]][["VarianceTestResult"]][["Replicate"]]) && !is.na(res.Anova[[i]][["VarianceTestResult"]][["Replicate"]]))
					{
						res[[i]][att,"TestVarRep"]=res.Anova[[i]][["VarianceTestResult"]][["Replicate"]]
					}
				}
				if (normalityTest!="None")
				{
					if(is.null(res.Anova[[i]]))

					res[[i]][att,"NormTest"]=res.Anova[[i]][["NormalityTest"]]
					#plot(density(res.Anova[[i]][["Residuals"]]))
					#qqnorm(res.Anova[[i]][["Residuals"]])
					#qqline(res.Anova[[i]][["Residuals"]])
					#qqnorm(res.Anova[[i]][["Residuals"]])
					xfit<-seq(min(res.Anova[[i]][["Residuals"]]),max(res.Anova[[i]][["Residuals"]]),length=40)
					yfit<-dnorm(xfit)
					fun=call("hist",res.Anova[[i]][["Residuals"]], freq=FALSE,  main="Distribution of residuals",xlab=att)
					fun=c(fun,call("lines",xfit, yfit,col="blue"))
					GenericPlot(type="pdf",fileName=paste("Distribution of", att,"residuals"),filewidth=9,fileheight=7,CALLFUN=fun)
				}
			}
		}
	}

	if (!is.null(listNames))
	{
		names(res)=listNames
	}

	for (i in 1:length(res))
	{
		if (orderByF==T)
		{
			# Tri par F croissant
			if ("FProd" %in% colnames(res[[i]]))
			{
				tmp=order(as.numeric(res[[i]][,"FProd"]),decreasing=T)
				res[[i]]=res[[i]][tmp,]
			} else if ("FRep" %in% colnames(res[[i]]))
			{
				tmp=order(as.numeric(res[[i]][,"FRep"]),decreasing=T)
				res[[i]]=res[[i]][tmp,]
			}

		}

		if ("StatMANOVA" %in% show && length(attributs)>1)
		{
			# Ajout ligne MANOVA
			res.manova=NULL
			cols=!colnames(extendedData)%in%c("Replicate","ProductCode","SubjectCode","Session")

			if (model %in% c("ThreeWayMultiplicativeRepeated","ThreeWayMultiplicative"))
			{
				Y=as.matrix(extendedData[,cols])
				Replicate=extendedData$Replicate
				Product=extendedData$ProductCode
				Subject=extendedData$SubjectCode
				res.manova=manova(Y~Replicate+Product+Subject+Replicate*Product+Replicate*Subject+Product*Subject,data=extendedData)
			}

			if (model=="TwoWayMultiplicative")
			{
				Y=as.matrix(extendedData[,cols])
				Replicate=extendedData$Replicate
				Product=extendedData$ProductCode
				Subject=extendedData$SubjectCode
				res.manova=manova(Y~Product+Subject+Product*Subject,data=extendedData)
			}

			if (model=="TwoWayAdditiveByRep")
			{
				tmp=subset(extendedData,Replicate==replicates[[i]])
				Y=as.matrix(tmp[,cols])
				Product=tmp$ProductCode
				Subject=tmp$SubjectCode
				res.manova=manova(Y~Product+Subject,data=tmp)
			}

			if (model=="TwoWayAdditiveByProduct")
			{
				tmp=subset(extendedData,ProductCode==products[[i]])
				Y=as.matrix(tmp[,cols])
				Subject=tmp$SubjectCode
				Replicate=tmp$Replicate
				res.manova=manova(Y~Subject+Replicate,data=tmp)
			}

			if (model=="TwoWayAdditiveBySubject")
			{
				tmp=subset(extendedData,Subject==subjects[[i]])
				Y=as.matrix(tmp[,cols])
				Product=tmp$ProductCode
				Replicate=tmp$Replicate
				res.manova=manova(Y~Product+Replicate,data=tmp)
			}

			# if (model=="OneWayBySubject")
			# {
				# res.manova=manova(Y~Product)
			# }

			# if (model=="OneWayByProduct")
			# {
				# res.manova=manova(Y~Subject)
			# }

			if(!is.null(res.manova))
			{
				tryCatch({
					#TODO : v?rifier F de MANOVA
					# Je crois que le F de MANOVA est bon lorsqu'on specifie bien que le test de la MANOVA est "Hotelling", sinon, par d?faut c'est Pillai...
					# A v?rifier avec ton jeu de donn?es
					# Je change ?a dans la macro Manova

					# Ajout ligne MANOVA
					res.summary=summary(res.manova,test="Hotelling-Lawley")

					res[[i]][nrow(res[[i]])+1,]=rep(NA,ncol(res[[i]]))
					rownames(res[[i]])[nrow(res[[i]])]="Overall"

					# Recalcul des F si sujet al?atoire
					if (randomSubject==T && ("Product:Subject" %in% rownames(res.summary$stats) || "Replicate:Subject" %in% rownames(res.summary$stats)))
					{
						if ("Product" %in% rownames(res.summary$stats))
						{
							res.manova.prod=TimeSens::Manova(P=res.summary$SS["Product"][[1]],R=res.summary$SS["Product:Subject"][[1]],df2=res.summary$stats["Product:Subject","Df"],test="Hotelling")
							res[[i]]["Overall","FProd"]=sprintf("%.2f",Re(res.manova.prod$f))
							res[[i]]["Overall","PProd"]=sprintf("%.3f",res.manova.prod$pvalue)
							res[[i]]["Overall","DiagProd"]=Asterisks(res.manova.prod$pvalue)
						}

						if ("Replicate" %in% rownames(res.summary$stats))
						{
							res.manova.rep=TimeSens::Manova(P=res.summary$SS["Replicate"][[1]],R=res.summary$SS["Replicate:Subject"][[1]],df2=res.summary$stats["Replicate:Subject","Df"],test="Hotelling")
							res[[i]]["Overall","FRep"]=sprintf("%.2f",Re(res.manova.rep$f))
							res[[i]]["Overall","PRep"]=sprintf("%.3f",res.manova.rep$pvalue)
							res[[i]]["Overall","DiagRep"]=Asterisks(res.manova.rep$pvalue)
						}
					} else
					{
						if ("Product" %in% rownames(res.summary$stats))
						{
							res[[i]]["Overall","FProd"]=sprintf("%.2f",res.summary$stats["Product","approx F"])
							res[[i]]["Overall","PProd"]=sprintf("%.3f",res.summary$stats["Product","Pr(>F)"])
							res[[i]]["Overall","DiagProd"]=Asterisks(res.summary$stats["Product","Pr(>F)"])
						}

						if ("Replicate" %in% rownames(res.summary$stats))
						{
							res[[i]]["Overall","FRep"]=sprintf("%.2f",res.summary$stats["Replicate","approx F"])
							res[[i]]["Overall","PRep"]=sprintf("%.3f",res.summary$stats["Replicate","Pr(>F)"])
							res[[i]]["Overall","DiagRep"]=Asterisks(res.summary$stats["Replicate","Pr(>F)"])
						}
					}

					if ("Subject" %in% rownames(res.summary$stats))
					{
						res[[i]]["Overall","FSubj"]=sprintf("%.2f",res.summary$stats["Subject","approx F"])
						res[[i]]["Overall","PSubj"]=sprintf("%.3f",res.summary$stats["Subject","Pr(>F)"])
						res[[i]]["Overall","DiagSubj"]=Asterisks(res.summary$stats["Subject","Pr(>F)"])
					}

					if ("Product:Subject" %in% rownames(res.summary$stats))
					{
						res[[i]]["Overall","FProdSubj"]=sprintf("%.2f",res.summary$stats["Product:Subject","approx F"])
						res[[i]]["Overall","PProdSubj"]=sprintf("%.3f",res.summary$stats["Product:Subject","Pr(>F)"])
						res[[i]]["Overall","DiagProdSubj"]=Asterisks(res.summary$stats["Product:Subject","Pr(>F)"])
					}

					if ("Replicate:Product" %in% rownames(res.summary$stats))
					{
						res[[i]]["Overall","FProdRep"]=sprintf("%.2f",res.summary$stats["Replicate:Product","approx F"])
						res[[i]]["Overall","PProdRep"]=sprintf("%.3f",res.summary$stats["Replicate:Product","Pr(>F)"])
						res[[i]]["Overall","DiagProdRep"]=Asterisks(res.summary$stats["Replicate:Product","Pr(>F)"])
					}

					if ("Replicate:Subject" %in% rownames(res.summary$stats))
					{
						res[[i]]["Overall","FRepSubj"]=sprintf("%.2f",res.summary$stats["Replicate:Subject","approx F"])
						res[[i]]["Overall","PRepSubj"]=sprintf("%.3f",res.summary$stats["Replicate:Subject","Pr(>F)"])
						res[[i]]["Overall","DiagRepSubj"]=Asterisks(res.summary$stats["Replicate:Subject","Pr(>F)"])
					}
				},error = function(e) {})
			}
		}
		# Mise en forme colonne F et pvalue
		att2=attributs
		if ("StatMANOVA" %in% show && length(attributs)>1 && exists("res.summary") && !is.null(res.summary))
		{
			att2=c(attributs,"Overall")
		}

		for (att in att2)
		{
			if (!("PValues" %in% show))
			{
				if ("FProd" %in% colnames(res[[i]]))
				{
					res[[i]][att,"FProd"]=paste(res[[i]][att,"FProd"],res[[i]][att,"DiagProd"],sep="")
				}
				if ("FRep" %in% colnames(res[[i]]))
				{
					res[[i]][att,"FRep"]=paste(res[[i]][att,"FRep"],res[[i]][att,"DiagRep"],sep="")
				}
				if ("FSubj" %in% colnames(res[[i]]))
				{
					res[[i]][att,"FSubj"]=paste(res[[i]][att,"FSubj"],res[[i]][att,"DiagSubj"],sep="")
				}
				if ("FProdRep" %in% colnames(res[[i]]))
				{
					res[[i]][att,"FProdRep"]=paste(res[[i]][att,"FProdRep"],res[[i]][att,"DiagProdRep"],sep="")
				}
				if ("FProdSubj" %in% colnames(res[[i]]))
				{
					res[[i]][att,"FProdSubj"]=paste(res[[i]][att,"FProdSubj"],res[[i]][att,"DiagProdSubj"],sep="")
				}
				if ("FRepSubj" %in% colnames(res[[i]]))
				{
					res[[i]][att,"FRepSubj"]=paste(res[[i]][att,"FRepSubj"],res[[i]][att,"DiagRepSubj"],sep="")
				}
			}
			if (!("F" %in% show))
			{
				if ("FProd" %in% colnames(res[[i]]))
				{
					res[[i]][att,"PProd"]=paste(res[[i]][att,"PProd"],res[[i]][att,"DiagProd"],sep="")
				}
				if ("FRep" %in% colnames(res[[i]]))
				{
					res[[i]][att,"PRep"]=paste(res[[i]][att,"PRep"],res[[i]][att,"DiagRep"],sep="")
				}
				if ("FSubj" %in% colnames(res[[i]]))
				{
					res[[i]][att,"PSubj"]=paste(res[[i]][att,"PSubj"],res[[i]][att,"DiagSubj"],sep="")
				}
				if ("FProdRep" %in% colnames(res[[i]]))
				{
					res[[i]][att,"PProdRep"]=paste(res[[i]][att,"PProdRep"],res[[i]][att,"DiagProdRep"],sep="")
				}
				if ("FProdSubj" %in% colnames(res[[i]]))
				{
					res[[i]][att,"PProdSubj"]=paste(res[[i]][att,"PProdSubj"],res[[i]][att,"DiagProdSubj"],sep="")
				}
				if ("FRepSubj" %in% colnames(res[[i]]))
				{
					res[[i]][att,"PRepSubj"]=paste(res[[i]][att,"PRepSubj"],res[[i]][att,"DiagRepSubj"],sep="")
				}
			}

		}

		# Style pour le fichier HTML

		if (!is.null(fileName))
		{
			# Attributs significatifs
			style=NULL
			if ("PProd" %in% colnames(res[[i]]))
			{

				for (j in which(res[[i]][,"PProd"]<=lsMeansAlpha))
				{
					style=AddHtmlStyle(style,x=rownames(res[[i]])[j],y="FProd",fontWeight="bold")
					style=AddHtmlStyle(style,x=rownames(res[[i]])[j],y="PProd",fontWeight="bold")
				}

				for (k in rownames(res[[i]]))
				{

					style=AddHtmlStyle(style,x=k,y="PProd",backgroundColor="lightgreen")
					style=AddHtmlStyle(style,x=k,y="FProd",backgroundColor="lightgreen")
					for (l in products)
					{
						style=AddHtmlStyle(style,x=k,y=l,backgroundColor="lightgreen")
					}
				}

			}
			if ("PRep" %in% colnames(res[[i]]))
			{
				for (j in which(res[[i]][,"PRep"]<=lsMeansAlpha))
				{
					style=AddHtmlStyle(style,x=rownames(res[[i]])[j],y="FRep",fontWeight="bold")
					style=AddHtmlStyle(style,x=rownames(res[[i]])[j],y="PRep",fontWeight="bold")
				}
				for (k in rownames(res[[i]]))
				{
					style=AddHtmlStyle(style,x=k,y="PRep",backgroundColor="lightblue")
					style=AddHtmlStyle(style,x=k,y="FRep",backgroundColor="lightblue")
					for (l in replicates)
					{
						style=AddHtmlStyle(style,x=k,y=l,backgroundColor="lightblue")
					}
				}
			}
			if ("PSubj" %in% colnames(res[[i]]))
			{
				for (j in which(res[[i]][,"PSubj"]<=lsMeansAlpha))
				{
					style=AddHtmlStyle(style,x=rownames(res[[i]])[j],y="FSubj",fontWeight="bold")
					style=AddHtmlStyle(style,x=rownames(res[[i]])[j],y="PSubj",fontWeight="bold")
				}
				for (k in rownames(res[[i]]))
				{
					style=AddHtmlStyle(style,x=k,y="PSubj",backgroundColor="lightgray")
					style=AddHtmlStyle(style,x=k,y="FSubjp",backgroundColor="lightgray")
				}
			}

			if ("PProdRep" %in% colnames(res[[i]]))
			{
				for (j in which(res[[i]][,"PProdRep"]<=lsMeansAlpha))
				{
					style=AddHtmlStyle(style,x=rownames(res[[i]])[j],y="FProdRep",fontWeight="bold")
					style=AddHtmlStyle(style,x=rownames(res[[i]])[j],y="PProdRep",fontWeight="bold")
				}
				for (k in rownames(res[[i]]))
				{
					style=AddHtmlStyle(style,x=k,y="PProdRep",backgroundColor="lightcyan")
					style=AddHtmlStyle(style,x=k,y="FProdRep",backgroundColor="lightcyan")
				}
			}
			if ("PProdSubj" %in% colnames(res[[i]]))
			{
				for (j in which(res[[i]][,"PProdSubj"]<=lsMeansAlpha))
				{
					style=AddHtmlStyle(style,x=rownames(res[[i]])[j],y="FProdSubj",fontWeight="bold")
					style=AddHtmlStyle(style,x=rownames(res[[i]])[j],y="PSubjRep",fontWeight="bold")
				}
				for (k in rownames(res[[i]]))
				{
					style=AddHtmlStyle(style,x=k,y="PProdRep",backgroundColor="lavender")
					style=AddHtmlStyle(style,x=k,y="FProdRep",backgroundColor="lavender")
				}
			}
			if ("PRepSubj" %in% colnames(res[[i]]))
			{
				for (j in which(res[[i]][,"PRepSubj"]<=lsMeansAlpha))
				{
					style=AddHtmlStyle(style,x=rownames(res[[i]])[j],y="FRepSubj",fontWeight="bold")
					style=AddHtmlStyle(style,x=rownames(res[[i]])[j],y="PRepSubj",fontWeight="bold")
				}
				for (k in rownames(res[[i]]))
				{
					style=AddHtmlStyle(style,x=k,y="PProdRep",backgroundColor="paleturquoise")
					style=AddHtmlStyle(style,x=k,y="FProdRep",backgroundColor="paleturquoise")
				}
			}
		}

		# Colonnes ? supprimer
		colToRemove=c("DiagProd","DiagRep","DiagSubj","DiagProdRep","DiagProdSubj","DiagRepSubj")
		if (!("F" %in% show))
		{
			colToRemove=c(colToRemove,"FProd","FRep","FSubj","FProdRep","FProdSubj","FRepSubj")
		}
		if (!("PValues" %in% show))
		{
			colToRemove=c(colToRemove,"PProd","PRep","PSubj","PProdRep","PProdSubj","PRepSubj")
		}

		colToKeep=!(colnames(res[[i]]) %in% colToRemove)
		res3=as.data.frame(res[[i]][,colToKeep]);colnames(res3)=colnames(res[[i]])[colToKeep];rownames(res3)=rownames(res[[i]])
		res[[i]]=res3
		# Fichier HTML
		if (!is.null(fileName))
		{
			res[[i]][is.na(res[[i]])]=""
			res[[i]][res[[i]]=="NANA"]="NA"

			f=fileName
			html=paste("<html><body><h2>",fileName,sep="")
			if (!is.null(names(res)[i]))
			{

			  if(length(names(res)[i])>1)
			  {
			    f=paste(fileName," ",names(res)[i],sep="")
			    html=paste(html,", ",names(res)[i],sep="")
			  }
			}
			html=paste(html,"</h2>",sep="")
			html=paste(html,"",MatrixToHtml(res[[i]], style=style),"")

			html=paste(html,"<p>","Model",": ", model,sep="")
			if (randomSubject==T)
			{
				html=paste(html," (","Random subject",")",sep="")
			}
			html=paste(html,".</p>",sep="")
			if (model=="ThreeWayMultiplicativeRepeated")
			{
				html=paste(html,"<p>","Repeated measure",": ", correlationStructure, ", Phi=", res[[i]][["Phi"]],".</p>",sep="")
			}
			if ("PProd" %in% colnames(res[[i]]) || "FProd" %in% colnames(res[[i]]))
			{
				html=paste(html,"<p>","Product means identified by the same letters are not significantly different", ".</p>",sep="")
			}
			if ("PRep" %in% colnames(res[[i]]) || "FRep" %in% colnames(res[[i]]))
			{
				html=paste(html,"<p>","Contrast for Replicate factor",": ", testRep, ", alpha=", lsMeansAlpha, ", ", "Adjustment method for the p-values",": ", lsMeansAdjustment, ".",sep="")
				html=paste(html,"(+) ","Significant increase over replicates",", (-) ", "Significant decrease over replicates",".",sep="")
				html=paste(html,"","Replicate means identified by the same letters are not significantly different", ".</p>",sep="")
			}
			html=paste(html,"(.) ","Significant at 10%", ", (*) ", "Significant at 5%",", (**) ", "Significant at 1%",", (***) ", "Significant at 0.1%",".</p>",sep="")

			cat(html,file=paste(f,".html",sep=""), append=FALSE)
			html = paste(html,"</body></html>")
		}

	}

	return (list(Table=res,Anovas=res2))
}