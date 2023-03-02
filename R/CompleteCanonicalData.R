#' @importFrom stats var xtabs
CompleteCanonicalData=function(canonicalData, percentageOfSubjectsByReplicate=0.75, percentageOfReplicatesBySubject=0.5,percentageOfProductBySubject=0.5)
{
    res=list()
	# Cas des jeux de donn?es import?s ou des NA sont pr?sents
	if("Score" %in% colnames(canonicalData)){
	canonicalData=canonicalData[!is.na(canonicalData[,"Score"]),]
	}
	canonicalData$SubjectCode=factor(canonicalData$SubjectCode);
	nbSubjects=length(unique(canonicalData$SubjectCode))
	canonicalData$Replicate=factor(canonicalData$Replicate)
	nbReplicates=length(unique(canonicalData$Replicate))
	canonicalData$ProductCode=factor(canonicalData$ProductCode)
	nbProducts=length(unique(canonicalData$ProductCode))
	
	canonicalData$AttributeCode=factor(canonicalData$AttributeCode)
	nbAttributes=length(unique(canonicalData$AttributeCode))
	# Nombres d'observations avant
	res[["Rows"]]=nrow(canonicalData)
	
	# Nombre de doublons
	res[["DuplicatedRows"]]=sum(duplicated(canonicalData[,c("ProductCode","SubjectCode","Replicate","AttributeCode")])==TRUE)
	
	# JDD complet ? Tous les juges doivent avoir vu tous les produits au moins une fois
	f=aggregate(AttributeCode~SubjectCode+ProductCode,canonicalData,length)
	
	res[["IsComplete"]]= (res[["DuplicatedRows"]] == 0) & (sum(f$AttributeCode==0)==0) & dim(f)[1]==nbProducts*nbSubjects
	
	# JDD ?quilibr? ? Tous les juges doivent avoir vu tous les produit le m?me nombre de fois
	res[["IsBalanced"]]=(res[["IsComplete"]])
	if(dim(f)[1]>1) # si on a plus d'un sujet*produit
	{
		g=aggregate(SubjectCode~ProductCode,canonicalData,length)
		res[["IsBalanced"]]=(res[["IsComplete"]] == TRUE & var(f$AttributeCode)==0 & (var(g$SubjectCode)==0 || is.na(var(g$SubjectCode))))
		
		#
		h=aggregate(AttributeCode~ProductCode+SubjectCode,canonicalData,length)
	}
	
	
	res[["CleanRows"]] = res[["Rows"]]
	res[["DeletedRows"]] = 0
	res[["CompleteCanonicalData"]] = canonicalData
	res[["CompleteRows"]] = res[["Rows"]]
	res[["MissingRows"]] = 0
	res[["PercentageOfMissingRows"]] = 0
	
	if (res[["IsBalanced"]]==FALSE)
	{
		# Tableau : nombre d'observations par produit juge rep
		a=(xtabs(~SubjectCode+ProductCode+Replicate, data=canonicalData))
		
		# Suppression d'une r?p si moins de 75% des juges (moins de percentageOfSubjectsByReplicate ont r?pondu: pour  percentageOfSubjectsByReplicate=0, rien n'est fait
		for (i in dim(a)[3]:1)
		{
			if (sum(a[,,i]>0)/length(a[,,i])<percentageOfSubjectsByReplicate)
			{
				canonicalData=canonicalData[-which(canonicalData$Replicate==i),]
			}
		}
		
		if (nrow(canonicalData)>0)
		{
			# Suppression d'une juge s'il participe ? strictement moins d'une r?p sur 2 percentageOfReplicatesBySubject<0.5
			b=aggregate(AttributeCode~SubjectCode+ProductCode+Replicate,canonicalData,length)
			b$IsOK=0
			b[b$AttributeCode>0,"IsOK"]=1
			c=aggregate(IsOK~SubjectCode,b,sum)
			indicesOfSubjectsToRemove=which((c$IsOK/max(c$IsOK))<percentageOfReplicatesBySubject)
			canonicalData=canonicalData[!canonicalData$SubjectCode %in% c[indicesOfSubjectsToRemove,"SubjectCode"],]
			
			# Suppression d'un produit s'il est not? dans moins de 50% des cas
			d=aggregate(AttributeCode~SubjectCode+ProductCode+Replicate,canonicalData,length)
			d$IsOK=0
			d[d$AttributeCode>0,"IsOK"]=1
			e=aggregate(IsOK~ProductCode,d,sum)
			indicesOfProductsToRemove=which((e$IsOK/max(e$IsOK))<percentageOfProductBySubject)
			canonicalData=canonicalData[!canonicalData$ProductCode %in% e[indicesOfProductsToRemove,"ProductCode"],]
			
			res[["CleanRows"]]=nrow(canonicalData)
			res[["DeletedRows"]]=res[["Rows"]]-res[["CleanRows"]]
			
			# Toutes les combinaisons session/produit/juge/rep/descripteur
			listProducts=unique(canonicalData$ProductCode)
			listSubjects=unique(canonicalData$SubjectCode)
			listReplicates=unique(canonicalData$Replicate)
			listAttributes=unique(canonicalData$AttributeCode)
			cols=MultiMerge(listDataframes=list(listProducts,listSubjects,listReplicates,listAttributes), mergeBy=NULL)
			colnames(cols)=c("ProductCode","SubjectCode","Replicate","AttributeCode")
			
			# CanonicalData compl?tes
			res[["CompleteCanonicalData"]]=merge(canonicalData,cols,by=c("Replicate", "ProductCode", "SubjectCode", "AttributeCode"),all.y=TRUE)
			
			# Nombres d'observations apr?s
			res[["CompleteRows"]]=nrow(res[["CompleteCanonicalData"]])
			
			# Nombres d'observations manquantes
			res[["MissingRows"]]=res[["CompleteRows"]]-res[["CleanRows"]]
			
			# % d'observations manquantes
			res[["PercentageOfMissingRows"]]=res[["MissingRows"]]/res[["CompleteRows"]]
		} else
		{
			res[["CleanRows"]]=0
			res[["DeletedRows"]]=nrow(canonicalData)
			res[["CompleteCanonicalData"]]=NULL
			res[["CompleteRows"]]=0
			res[["MissingRows"]]=res[["DeletedRows"]]
			res[["PercentageOfMissingRows"]]=1
		}
	}
	return (res)
}