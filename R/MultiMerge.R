MultiMerge=function(listDataframes, mergeBy, all.x=FALSE, all.y=FALSE)
{
	mergedDataframe = listDataframes[[1]]
	for (x in 2:length(listDataframes))
	{
		mergedDataframe = merge(mergedDataframe,listDataframes[[x]],by=mergeBy,all.x=all.x, all.y=all.y)
	}
	return (mergedDataframe)
}