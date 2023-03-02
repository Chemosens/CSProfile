data(cheeses)
profileBarPlot(profileObject=cheeses)
profileBarPlot(profileObject=cheeses, variable="Sticky",itemsToPlot=c("GMean","ErrorBars"),errorBars="ConfInt",statsToPlot=c("Means","Sd","N"),groupId="Both",fileName=NULL)
profileBarPlot(profileObject=cheeses, variable="Sticky",itemsToPlot=c(""),errorBars="ErrStd",statsToPlot=c("Means","Sd","N"),groupId="Both",fileName=NULL)
