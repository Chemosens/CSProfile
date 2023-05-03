#'@importFrom stats aggregate sd
ExtendedDataHtmlSummary=function(extendedData,by="AttributeCode",title="",fileName="ExtendedDataSummary")
{
	res.ExtendedDataSummary=ExtendedDataSummary(extendedData)

	html = paste("<html><body><h2>",title,"</h2>",sep="")
	matRes=NULL
	if (by=="ProductCode")
	{
	  for (product in levels(extendedData$ProductCode))
		{
			mat=NULL
			for (stat in res.ExtendedDataSummary)
			{
				rownames(stat)=levels(extendedData$ProductCode)
				mat=cbind(mat,t(stat[product,-1]))
			}
			colnames(mat)=names(res.ExtendedDataSummary)
			rownames(mat)=colnames(extendedData)[-c(1:4)]
			matRes=rbind(matRes,cbind(product=product,mat))
			html = paste(html,"<h3>",product,"</h3>",MatrixToHtml(matrix=mat),sep="",collapse="")
		}
	}
	if (by=="AttributeCode")
	{

		for (attribute in colnames(extendedData)[-c(1:4)])
		{
			mat=NULL
			for (stat in res.ExtendedDataSummary)
			{
				mat=cbind(mat,stat[,attribute])
			}
			colnames(mat)=names(res.ExtendedDataSummary)
			rownames(mat)=levels(as.factor(as.character(extendedData$ProductCode)))
			html = paste(html,"<h3>",attribute,"</h3>",MatrixToHtml(mat),sep="",collapse="")
			matRes=rbind(matRes,cbind(Attribute=attribute,mat))
		}
	}
	matRes[,-1]=as.numeric(matRes[,-1])
	# Ecriture dans fichier html
	html = paste(html,"</body></html>",sep="",collapse="")
	cat(html,file=paste(fileName,".html",sep=""), append=TRUE)
	return(matRes)
}