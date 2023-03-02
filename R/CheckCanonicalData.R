CheckCanonicalData=function(canonicalData,variables=c("SubjectCode","ProductCode","AttributeCode","Replicate","Score","Time","Session"),transformSessionIntoReplicate=TRUE,selectOtherVariables=NULL)
{
	#TODO : log des modifs
	SubjectCode=ProductCode=AttributeCode=NULL
	res=list()
	
	# V?rification des variables obligatoires
	missingVariables=NULL
	colNames = colnames(canonicalData)
	for (x in variables)
	{
		if (! (x %in% colNames))
		{
			if (x == "Session")
			{
				canonicalData$Session = "1"
			} else
			if (x == "Replicate")
			{
				canonicalData$Replicate = "1"
			} else
			{
				missingVariables=c(missingVariables,x)
			}
		}
	}
	colNames = colnames(canonicalData)
	
	res[["MissingVariables"]]=missingVariables
	if(length(missingVariables)>0)
	{		
		stop(paste("MissingVariables: ",paste(missingVariables,collapse=", ")))
	}
	
	
	# Suppression des caract?res sp?ciaux, Transformation en facteurs
	if ("SubjectCode" %in% colNames && "SubjectCode" %in% variables)
	{
		if(length(canonicalData$SubjectCode[!is.na(canonicalData$SubjectCode)])==0){stop("[TS] No subjects in the subject column")}
		canonicalData$SubjectCode=gsub("[!#$%&'()*+,-./:;<=>?@]", "_", canonicalData$SubjectCode)
		canonicalData$SubjectCode = factor(canonicalData$SubjectCode)
		canonicalData=subset(canonicalData,SubjectCode!="")
	}
	if ("ProductCode" %in% colNames && "ProductCode" %in% variables)
	{
		if(length(canonicalData$ProductCode[!is.na(canonicalData$ProductCode)])==0){stop("[TS] No products in the product column")}
	
		canonicalData$ProductCode=gsub("[!#$%&'()*+,-./:;<=>?@]", "_", canonicalData$ProductCode)
		canonicalData$ProductCode = factor(canonicalData$ProductCode)
		canonicalData=subset(canonicalData,ProductCode!="")
	}
	if ("AttributeCode" %in% colNames && "AttributeCode" %in% variables)
	{
		if(length(canonicalData$AttributeCode[!is.na(canonicalData$AttributeCode)])==0){TS_LogEntry("[TS] No attribute in the attribute column")}
		canonicalData$AttributeCode=gsub("[!#$%&'()*+,-./:;<=>?@]", "_", canonicalData$AttributeCode)
		canonicalData$AttributeCode = factor(canonicalData$AttributeCode) 
		canonicalData=subset(canonicalData,AttributeCode!="")
	}
	if ("Session" %in% colNames && "Session" %in% variables)
	{
		canonicalData$Session=gsub("[!#$%&'()*+,-./:;<=>?@]", "_", canonicalData$Session)
		canonicalData[is.na(canonicalData[,"Session"]),"Session"]="NoSession"
		canonicalData$Session = factor(canonicalData$Session)	
	} 
	if ("Replicate" %in% colNames && "Replicate" %in% variables)
	{
		canonicalData$Replicate=gsub("[!#$%&'()*+,-./:;<=>?@]", "_", canonicalData$Replicate)
		canonicalData$Replicate = factor(canonicalData$Replicate) 
	}
	if ("Group" %in% colNames && "Group" %in% variables)
	{
		canonicalData$Group=gsub("[!#$%&'()*+,-./:;<=>?@]", "_", canonicalData$Group)
		canonicalData$Group = as.character(canonicalData$Group) 
	}
	if ("Score" %in% colNames && "Score" %in% variables)
	{
		if (!is.numeric(canonicalData$Score))
		{
			canonicalData$Score = as.numeric(as.character(canonicalData$Score))
		}
	}
	if ("Time" %in% colNames && "Time" %in% variables)
	{
		if (!is.numeric(canonicalData$Time))
		{
			canonicalData$Time = as.numeric(as.character(canonicalData$Time))
		}
	}
	# ajout eventuel de colonnes pour les caract?ristiques sujets/produits...
	supplementaryVariables=c()
	# Enlever les caract?res sp?ciaux?
	#selectOtherVariables=gsub("[!#$%&'()*+,-./:;<=>?@]", "_", selectOtherVariables)
	for(i in 1:length(selectOtherVariables))
	{
		if(!is.null(selectOtherVariables))
		{
			if(selectOtherVariables[i] %in% colNames)
			{
				nameVariable=selectOtherVariables[i]
				canonicalData[,nameVariable]=as.character(canonicalData[,nameVariable])
				supplementaryVariables=c(supplementaryVariables,selectOtherVariables[i])
			}
		}
	}

	canonicalData[,"OldReplicate"]=canonicalData[,"Replicate"]
	
	# V?rification quadruplet SubjectCode*ProductCode*Replicate*Session unique
	f=function(x)
	{
		return (length(unique(x)))
	}
	tmp=aggregate(Session~SubjectCode+ProductCode+Replicate,canonicalData,f)
	if (transformSessionIntoReplicate == TRUE && max(tmp$Session)>1)
	{
		# Recalcul de Replicate qui en prend en compte Session+Replicate		
		canonicalData[,"NewReplicate"]=NA
		canonicalData[,"SessionRep"]=paste(canonicalData[,"Session"],canonicalData[,"Replicate"])
		subjects=unique(canonicalData$SubjectCode)
		products=unique(canonicalData$ProductCode)
		for (subject in subjects)
		{
			for (product in products)
			{
				tmp=subset(canonicalData,ProductCode==product & SubjectCode==subject)
				if (nrow(tmp)>0)
				{
					tabSessionRep=data.frame(unique(tmp$SessionRep))
					colnames(tabSessionRep)="SessionRep"
					tabSessionRep$NewRep=rownames(tabSessionRep)
					for (i in 1:nrow(tabSessionRep))
					{
						canonicalData[canonicalData$ProductCode==product & canonicalData$SubjectCode==subject & canonicalData$SessionRep==tabSessionRep[i,"SessionRep"],]$NewReplicate=tabSessionRep[i,"NewRep"]
					}
				}
			}
		}
		canonicalData$Replicate=canonicalData$NewReplicate
		canonicalData$Replicate = factor(canonicalData$Replicate) 
	}
	
	#res[["CanonicalData"]]=canonicalData[,c(variables,"OldReplicate")]
	res[["CanonicalData"]]=canonicalData[,c(variables,supplementaryVariables)]
	
	return (res)
}