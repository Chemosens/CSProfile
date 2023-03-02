data(cheeses)
p_cva=profileCVA(profileObject=cheeses)
profileCVA(profileObject, test="Hotelling-Lawley",option="TwoWayANOVA", itemsToPlot=c("ProductEllipses","ProductSegments","ProductLabels","AttributeLabels"), representation="Biplot", nbAxes="Auto", outputs=NULL,confInt=0.9,ellipsesType="barycentric",revertAxes=NULL,productColors=FALSE,fileName=NULL,ellipsesCalculation="Chi",twoDiffFiles=TRUE,onlySignificantAttributesInMultivariateAnalysis=TRUE,alphaForSelectionOfSignificantAttributes=0.05)
  