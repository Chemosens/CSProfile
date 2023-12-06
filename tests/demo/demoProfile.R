library("CSProfile")

profileObject=profileReadData(file="./data/profile.csv",allData=NULL,outputsCsv=NULL,
                              replaceNA="crossmean", language="en",sep=";",
                              transformSessionIntoReplicate=TRUE,
                              percentageOfSubjectsByReplicate=0,
                              percentageOfReplicatesBySubject=0,percentageOfProductBySubject=0)

res_anova=profileAnova(profileObject,model="DefaultModel", randomEffects=c("Subject"), lsMeansAlpha=0.10, lsMeansAdjustment="Tukey", anovaCalculationMode="Ols", show=c("Groups","StatMANOVA","F", "PValues","RMSE"),fileName=NULL,varianceTest="None", normalityTest="None",orderByF=TRUE)
res_anova

res_stats=profileDescriptiveStatistics(profileObject,by="AttributeCode")
res_stats
res_stats=profileDescriptiveStatistics(profileObject,by="ProductCode")
res_stats

res_boxplot=profileBoxPlot(profileObject,variable=profileObject$Attributes[1])
res_boxplot

#profileHistogramOfDistributions(profileObject)
res_barplot=profileBarPlot(profileObject,variable=profileObject$Attributes[1])
res_barplot
res_barplot=profileBarPlot(profileObject,variable=profileObject$Attributes[3])

res_radarplot=profileRadarPlot(profileObject)
res_radarplot
# General statistics around profileObject
res_flashtable=profileFlashTable(profileObject,classificationMethod="Complete", explainedVariance=0.5, alphaContrast=0.1, similarity="Pearson", fileName="Flash table", show="",contrastOption="GMean",contrastProduct=NULL)
res_flashtable
# Use of MAM model
res_mam=profileMAM(profileObject)
res_mam
# Performances of panelists
res_mamcaptable=profileCAPTable(profileObject,model="MAM")
res_mamcaptable
#res_captable=profileCAPTable(profileObject,model="CAP")

# Cartography
res_pca=profilePCA(profileObject)
res_pca=profilePCA(profileObject,itemsToPlot=c("ProductEllipses","Panellists"))

res_cva=profileCVA(profileObject)
res_cva=profileCVA(profileObject,itemsToPlot=c("ProductEllipses","ProductSegments"))
res_cva=profileCVA(profileObject,itemsToPlot=c("Panellists"))
