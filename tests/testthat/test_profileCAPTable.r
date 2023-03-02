data(cheeses)
mamcapTable=profileCAPTable(profileObject=cheeses,model="MAM")
overallTable=profileCAPTable(profileObject=cheeses,model="Overall",output="overall.html")

capTable=profileCAPTable(profileObject=cheeses,model="CAP",output="CAP.html")
#profileObject,model="MAM",panelLimit=0.05,indivLimit=0.05,output="CAPTable",correction=FALSE,correlationTest="Kendall", indivRepLimCap=0.01, indivAgLimCap=0.2, repInIndModel=FALSE)

capTable2=profileCAPTable(profileObject=cheeses,model="CAP",output="CAP2.html",
panelLimit=0.2,indivLimit=0.2,correction=TRUE,correlationTest="pearson", 
indivRepLimCap=0.001, indivAgLimCap=0.2, 
repInIndModel=FALSE)

# Repetabilite similaire pour tout les panels
test_that("Similar repeatability for mamcap and capTable",
          expect_true(
            all(round(mamcapTable$panelRepeatability-capTable$panelRepeatability[names(mamcapTable$panelRepeatability)],digits=8)==0)
            
          )
)

test_that("Similar repeatability for capTable2 and capTable",
          expect_true(
            all(round(capTable2$panelRepeatability-capTable$panelRepeatability[names(capTable2$panelRepeatability)],digits=8)==0)
            
          )
)

test_that("Similar repeatability for overall and capTable",
          expect_true(
            all(round(overallTable$panelRepeatability-capTable$panelRepeatability[names(overallTable$panelRepeatability)],digits=8)==0)
            
          )
)

# Verification des F produits ! TODO (avec !ANOVA_V2)! 
all(round(capTable2$panelProductF-capTable$panelProductF[names(capTable2$panelProductF)],digits=8)==0)
