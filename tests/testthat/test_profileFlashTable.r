data(cheeses)
flash1=profileFlashTable(profileObject=cheeses,classificationMethod="Complete", 
                         explainedVariance=0.5, alphaContrast=0.1, similarity="Pearson",
                         fileName="Flash table", show="",contrastOption="GMean",contrastProduct=NULL)
flash2=profileFlashTable(profileObject=cheeses,classificationMethod="Complete", 
                         explainedVariance=0.5, alphaContrast=0.1, similarity="Pearson",
                         fileName="Flash table 2", show="",contrastOption="GMean",contrastProduct=NULL)
