profileObject=profileReadData(file="../../data/profile.csv",allData=NULL,
                              replaceNA="crossmean", sep=";",
                              transformSessionIntoReplicate=TRUE, percentageOfSubjectsByReplicate=0,
                              percentageOfReplicatesBySubject=0,percentageOfProductBySubject=0)


profileReadData(allData=profileObject$CompleteExtendedDataWithoutNA[,-1],format="wide")
