TS_SetProfileGraphicsOptions=function(profileObject, defaultGraphicalFormat, minScore="Auto", maxScore="Auto")
{
	if(minScore=="Auto")
	{
		minScore=min(profileObject[["CompleteCanonicalDataWithoutNA"]][,"Score"],na.rm=TRUE)
	}
	if(maxScore=="Auto")
	{
		maxScore=max(profileObject[["CompleteCanonicalDataWithoutNA"]][,"Score"],na.rm=TRUE)
	}
	profileObject[["MinScore"]]=minScore
	profileObject[["MaxScore"]]=maxScore
	profileObject[["DefaultGraphicalFormat"]] = tolower(defaultGraphicalFormat)
	
	return(profileObject)
}