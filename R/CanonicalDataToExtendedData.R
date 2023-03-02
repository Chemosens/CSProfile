#'@importFrom stats reshape
CanonicalDataToExtendedData = function(canonicalData)
{
	canonicalData$Session=1
	canonicalData=canonicalData[,c("Session","ProductCode", "SubjectCode", "Replicate","Score","AttributeCode")]
	res = reshape(canonicalData,idvar=c("Session","ProductCode", "SubjectCode", "Replicate"),timevar="AttributeCode",direction="wide")
	
	colNames = colnames(res)
	
	# Attributs en colonne
	colNames[-c(1:4)]=substr(colNames[-c(1:4)],7,100)
	colnames(res)=colNames
	
	return (res)
}
