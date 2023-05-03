#'profileMAM
#'
#'Returns the results of Mixed Assessor Model
#'@param profileObject profileObject from \link{profileReadData}
#'@param output default to "MAM.html". Names of the output.
#'@param correctOnlyIfSignificant FALSE by default. If TRUE, scaling effect is corrected only when it is significant.
#'@param limitOfSignificance default to 0.05 limit of significance when correctOnlyIfSignificant is TRUE
#'@param negativeCorrection default to TRUE: the negative scaling are NOT taken into account in the sum squares. When FALSE, negative scaling are considered as scaling effect.
#'@export
#'@importFrom CSUtils PanelPerformances
#'@examples
#'data(cheeses)
#'profileMAM(cheeses)
profileMAM = function(profileObject, output="MAM.html",correctOnlyIfSignificant=FALSE,limitOfSignificance=0.05,negativeCorrection=TRUE)
{
	nbProducts=length(profileObject$Products)
	nbReplicates=length(profileObject$Replicates)
	nbSubjects=length(profileObject$Subjects)
	nbAttributes=length(profileObject$Attributes)
	if(!profileObject$IsBalanced)
	{
	  stop("This analysis requires a balanced dataset")
	}
	N=nrow(profileObject$CompleteCanonicalDataWithoutNA)
	if (nbProducts<3)
	{
		stop("This analysis requires at least 3 products")
	}
	if (nbReplicates<2)
	{
	  stop("This analysis requires at least 2 replicates")
	}
	SubjectCode=profileObject$CompleteExtendedDataWithoutNA[,"SubjectCode"]
	ProductCode=profileObject$CompleteExtendedDataWithoutNA[,"ProductCode"]
	Replicate=profileObject$CompleteExtendedDataWithoutNA[,"Replicate"]
	dataQuanti=profileObject$CompleteExtendedDataWithoutNA[,-c(1:4)]
	sumByAtt=apply(dataQuanti,2,sum)
	indAttWithNA=which(is.na(sumByAtt))
	attWithNA=names(sumByAtt)[indAttWithNA]
#	warning(paste("NA in",names(attWithNA),": MAM is not possible",sep=""))
	if(length(indAttWithNA)>0){	dataQuanti=dataQuanti[,-indAttWithNA]}
	# if(dim(dataQuanti)[2]==0){stop("[TS] No attribute without NA in the data: MAM do not run")} # cas o? tous les attributs ont des NA -> pas de MAM possible
	input=cbind(SubjectCode,ProductCode,Replicate,dataQuanti)
	if(is.null(dim(dataQuanti))) # cas ou on a un seul attribut
	{
		colnames(input)[4]=colnames(profileObject$CompleteExtendedDataWithoutNA)[5]
	}
	# on applique la fonction PanelPerformances
	resultat2=PanelPerformances(frame=input, modelType="mam",negativeCorrection=negativeCorrection,correctOnlyIfSignificant=correctOnlyIfSignificant,limitOfSignificance=limitOfSignificance,onlySignificantDim=FALSE,manovaTest="Hotelling", panelistPerf=FALSE,correlationTest="none")

	AnovaTables2=resultat2$listAnova
	txt="<html>"
	txt=paste(txt,"<h2>","Mixed Assessor Model results","</h2>",sep="")
	for(i in 1:nbAttributes)
	{
		if(!is.null(AnovaTables2[[as.character(profileObject$Attributes[i])]]))
		{
			AnovaTable=AnovaTables2[[as.character(profileObject$Attributes[i])]]
			txt=paste(txt,"<h3>",profileObject$Attributes[i],"</h3>",MatrixToHtml(AnovaTable),sep="")
		}
		else
		{
			txt=paste(txt,"<h3>",profileObject$Attributes[i],"</h3>","<p>Issues in the calculations of MAM: potential NA for this descriptor. You can replace the NA by imputation</p>",sep="")
		}
	}
	txt=paste(txt,"</html>")
	write(txt,paste(output,sep=""))
	return(AnovaTables2)
}




