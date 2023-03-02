InputMissingValuesInCanonicalData = function(canonicalData, replaceNA="crossmean", variable="Score",complete=TRUE, percentageOfSubjectsByReplicate=0.75, percentageOfReplicatesBySubject=0.5,percentageOfProductBySubject=0.5)
{
	#TODO : moyenner les doublons?
	completeData=CompleteCanonicalData(canonicalData,percentageOfSubjectsByReplicate=percentageOfSubjectsByReplicate, percentageOfReplicatesBySubject=percentageOfReplicatesBySubject,percentageOfProductBySubject=percentageOfProductBySubject)$CompleteCanonicalData
	

	if (replaceNA %in% c("zeros","Zeros"))
	{
		# Score remplacé par 0
		completeData[,variable]=replace(completeData[,variable], is.na(completeData[,variable]), 0)
	}
	if (replaceNA %in% c("colmean","Colmean"))
	{
		# Score remplacé par moyenne de score
		completeData[,variable]=replace(completeData[,variable], is.na(completeData[,variable]), mean(completeData[,variable],na.rm=TRUE))
	}
	
	if (replaceNA %in% c("crossmean","Crossmean"))
	{
		# Valeur = moyenne produit + moyenne juge - moyenne totale
		indices=which(is.na(completeData[,variable]))
		for (indice in indices)
		{
			productCode=completeData[indice,"ProductCode"]
			subjectCode=completeData[indice,"SubjectCode"]
			grandMean=mean(completeData[,variable],na.rm=TRUE)
			productMean=mean(completeData[completeData$ProductCode==productCode,variable],na.rm=TRUE)
			subjectMean=mean(completeData[completeData$SubjectCode==subjectCode,variable],na.rm=TRUE)
			if (is.na(subjectMean))
			{
				subjectMean=grandMean
			}
			if (is.na(productMean))
			{
				productMean=grandMean
			}
			completeData[indice,variable]=productMean+subjectMean-grandMean
		}
	}
	return (completeData)
}