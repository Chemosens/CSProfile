library("chemosensR")

#tds=tdsRead("./data/TDS.csv", cols=list(subject="SubjectCode", product="ProductCode", descriptor="AttributeCode",time="Time",score="Score",rep="Replicate"), supCols="", sep=";",startWithFirstCitation=FALSE,discretization=0.2,periods=1)
data(tds)
print("Panel behavior distribution")
a1=analysis(tds,type="Panel behaviour distribution", title="Panel behaviour distribution",plot=c("violin","boxplot","jitter"),outputFile="",outputFormat="")
print("Panel behavior table")
a2=analysis(tds,type="Panel behaviour table", title="Panel behaviour table",sequenceStart=15,sequenceDuration=6,nbDescriptors=1,nbClicks=1.5,descriptorDuration=2,outputFile="",outputFormat="")
print("ANOVA of behaviors")
a3=analysis(tds,type="ANOVA of behaviours", title="ANOVA of behaviours", columns=c("G_Mean","F_Product","P_Product","F_Product_Significance","SE_Product","R2","P_ShapiroWilks","P_Levene_Product","Mean_Product","Group_Product","P_Subject"), alpha=0.05,outputFile="",outputFormat="")
print("ANOVA of durations")
a4=analysis(tds,type="ANOVA of durations", title="ANOVA of durations", columns=c("G_Mean","F_Product","P_Product"),outputFile="",outputFormat="")
#a4=analysis(tds,type="Clusters of subjects durations", title="Clusters of subjects durations")
#TODO CRI

d3=analysis(tds,type="ANOVA of durations", title="ANOVA of purations", columns=c("G_Mean","F_Product","P_Product","F_Product_Significance","SE_Product","R2","P_ShapiroWilks","P_Levene_Product","Mean_Product","Group_Product","P_Subject"), alpha=0.05,outputFile="",outputFormat="")
d4=analysis(tds,type="ANOVA of durations", title="ANOVA of durations by rep", runBy="rep", columns=c("G_Mean","F_Product","P_Product","F_Product_Significance","SE_Product","R2","P_ShapiroWilks","P_Levene_Product","Mean_Product","Group_Product","P_Subject"), alpha=0.05,outputFile="",outputFormat="")
print("PCA")
d5=analysis(tds,type="PCA of durations", title="PCA",outputFile="",outputFormat="")
d6=analysis(tds,type="PCA of durations", title="PCA", runBy="rep",outputFile="",outputFormat="")

b2=analysis(tds,type="Dominance curves", title="Dominance curves", alpha=0.05, repAsIndividual=TRUE, rows="product", color="descriptor", smooth=TRUE, draw=c("significance","meanComparison"),outputFile="",outputFormat="")
b2=analysis(tds,type="Dominance curves", title="Dominance curves", alpha=0.05, repAsIndividual=TRUE, rows="product", color="descriptor", smooth=TRUE, draw=c("significance","meanComparison"),fontSizeCex=2,outputFile="",outputFormat="")
b3=analysis(tds,type="Standardized dominance curves", title="Dominance curves", alpha=0.05, repAsIndividual=TRUE, rows="product", cols="rep", color="descriptor", smooth=TRUE, draw=c("significance","hasard","graymask"),outputFile="",outputFormat="")
b6=analysis(tds,type="Differences of dominance curves", title="Difference curves", alpha=0.1, repAsIndividual=TRUE, rows="product", cols="rep", color="descriptor", smooth=TRUE)
# Fonctionne mais très long... ?  pourquoi ?
b5=analysis(tds,type="Maximum dominance rates", title="Maximum dominance rates", alpha=0.05, draw=c("significance","hasard","graymask"),outputFile="",outputFormat="")
b7=analysis(tds,type="Standardized differences of dominance curves", title="Difference curves", selection="product=='P001' OR product=='P002'", alpha=0.1,outputFile="",outputFormat="")
b8=analysis(tds,type="Panel sequences", title="Panel bandplot", alpha=0.05, repAsIndividual=TRUE,outputFile="",outputFormat="")
b9=analysis(tds,type="Standardized panel sequences", title="Panel bandplot", alpha=0.05, repAsIndividual=TRUE,outputFile="",outputFormat="")

# TODO : ajouter les priorité parametres dans les fonctions, le parametre parameters, ecrire l'aide
# A debugguer pour des raisons annexes... 
b1=analysis(tds,type="Individual sequences",title="Individual sequences",outputFile="",outputFormat="") # TODO corriger les bugs


#b4=analysis(tds,type="Dominance curves", title="Dominance curves", runBy="rep", selection="product=='P001'",rows="product",cols="rep",color="descriptor",alpha=0.2, repAsIndividual=FALSE)

# Citations
c1=analysis(tds,type="Citation distribution", title="Citation distribution",plot=c("violin","boxplot","jitter"))
c2=analysis(tds,type="Barplot of citations", title="Barplot of citations", confInt=0.95, errorBars="CI")
#c3=analysis(tds,type="MRCA", title="MRCA", runBy="rep", nBoot=10, alpha=0.05, nbAxes="signif", twoSided=FALSE)
#TODO: ANOVA comptage

# Durées
d1=analysis(tds,type="Duration distribution", title="Duration distribution",plot=c("violin","boxplot","jitter"))
d2=analysis(tds,type="Barplot of durations", title="Barplot of durations", confInt=0.95, errorBars="CI")
d7=analysis(tds,type="CVA of durations", title="CVA",runBy="rep")
d8=analysis(tds,type="CVA of durations", title="CVA", axes=list(c(1,2)))
d8b=analysis(tds,type="CVA of durations", title="CVA", axes=list(c(1,2)),
nbDimHotelling=1)

d9=analysis(tds,type="PCA of trajectories", title="PCA of trajectories",fontSizeCex=2)

test_that("tds cva",
          expect_true(length(d8[[1]])==4)
)
test_that("tds hotelling",
          expect_true(sum(d8b[[1]]$coordinates$hotelling!=
                            d8[[1]]$coordinates$hotelling)!=0)
)
test_that("tds hotelling2",
          expect_true(sum(d8b[[1]]$hotelling!=
                            d8[[1]]$hotelling)!=0)
)


