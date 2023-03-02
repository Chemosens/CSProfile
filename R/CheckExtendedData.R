CheckExtendedData=function(extendedData)
{
	# res=list()
	
	##Facteurs
	# fact=sapply(extendedData, is.factor)
	# cols=names(fact[fact==TRUE])
	
	##Doublons
	# res[["DuplicatedRows"]]=sum(duplicated(extendedData[,cols])==TRUE)
	
	# completeDesign=levels(extendedData[,cols[1]])
	# for (col in cols[-1])
	# {
		# completeDesign=merge(completeDesign,levels(extendedData[,col]),by=NULL)
	# }
	
	##Lignes manquantes
	# res[["MissingRows"]]=nrow(completeDesign)-nrow(extendedData)+res[["DuplicatedRows"]]
	
	# res[["IsComplete"]]=res[["MissingRows"]]==0
	# res[["IsBalanced"]]=res[["DuplicatedRows"]]==0
	
	# return (res)
}