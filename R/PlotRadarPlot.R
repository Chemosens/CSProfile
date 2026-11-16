#'@import fmsb
#'@importFrom stats aggregate median
#'@importFrom graphics layout plot.new
PlotRadarPlot = function(extendedData, productVector, productColors, mainTitle="",titleLegend="", maxScale, minScale, statistic="mean",resAnova=NULL,legend=TRUE,printLimit=FALSE,plotType="base")
{
	# 3 attributs n?cessaires pour trac? un radarplot

	productVector=as.character(productVector)

	# Donn?es aggr?g?es par produit ou attribut
	selectedData=extendedData
	selectedIndividuals=list(productVector)
	# if (dim(extendedData)[1]<3)
	# {
		# stop(TS_GetLabel("InsufficientNumberOfAttributes"))
	# }

	# Choix de la statistique
	if(statistic=="mean")
	{
		aggregatedData=aggregate(selectedData,selectedIndividuals , mean,na.rm=TRUE)
	}
	 if (statistic == "median")
	 {
		 aggregatedData=aggregate(selectedData,selectedIndividuals , median,na.rm=TRUE)
	 }
	# if (statistic == "range")
	# {
		# aggregatedData=aggregate(data[,-c(1:4)], list(ProductCode = data$ProductCode), mean,na.rm=TRUE)
	# }
	rownames(aggregatedData)=aggregatedData[,1]
	aggregatedData=aggregatedData[,-1]
	colnamesAggregatedData=colnames(aggregatedData)
	if(!is.null(resAnova))
	{
		for(k in 1:length(colnamesAggregatedData))
		{

			statF=round(resAnova[[colnamesAggregatedData[k]]][[1]]$FProd,digits=2)
			stars=resAnova[[colnamesAggregatedData[k]]][[1]]$DiagProd
			colnamesAggregatedData[k]=paste(colnamesAggregatedData[k],"\n (F=",statF,stars,")",sep="")
		}
	}
	colnames(aggregatedData)=colnamesAggregatedData
	nCols=ncol(aggregatedData)
	productNames=rownames(aggregatedData)
	# Min et max de l'?chelle
	if (exists("minScale") && exists("maxScale") && is.numeric(minScale) && is.numeric(maxScale) && minScale < maxScale)
	{
		minScale=rep(minScale,nCols)
		maxScale=rep(maxScale,nCols)
		aggregatedData=rbind(minScale,aggregatedData)
		rownames(aggregatedData)[1]="min"
		aggregatedData=rbind(maxScale,aggregatedData)
		rownames(aggregatedData)[1]="max"

		maxmin=TRUE
	} else
	{
		maxmin=FALSE
	}
	# Trac? du graphique
#	LoadPackage("fmsb")
	if (plotType == "gg")
	{
	  library(ggplot2)

	  # Valeurs des produits (sans les lignes max/min ajoutées pour fmsb)
	  values <- as.matrix(aggregatedData[productNames, , drop = FALSE])
	  if (maxmin) {
	    minS <- minScale[1]; maxS <- maxScale[1]
	  } else {
	    minS <- min(values, na.rm = TRUE); maxS <- max(values, na.rm = TRUE)
	  }
	  attrs <- colnames(values)
	  nA <- length(attrs)
	  # Axes répartis dans le sens horaire en partant du haut
	  angles <- pi / 2 - 2 * pi * (seq_len(nA) - 1) / nA
	  toRadius <- function(v) (v - minS) / (maxS - minS)

	  # Grille : 4 polygones concentriques (comme seg=4) + rayons
	  df_grid <- do.call(rbind, lapply(1:4, function(k) data.frame(
	    level = k, x = (k / 4) * cos(angles), y = (k / 4) * sin(angles))))
	  df_spokes <- data.frame(x = 0, y = 0, xend = cos(angles), yend = sin(angles))

	  # Profils des produits
	  df_prof <- do.call(rbind, lapply(productNames, function(pn) {
	    r <- toRadius(as.numeric(values[pn, ]))
	    data.frame(Product = pn, Attribute = attrs, x = r * cos(angles), y = r * sin(angles))
	  }))
	  df_prof$Product <- factor(df_prof$Product, levels = productNames)

	  # Etiquettes des attributs, alignées selon leur côté
	  df_lab <- data.frame(label = attrs, x = 1.12 * cos(angles), y = 1.12 * sin(angles),
	                       hjust = ifelse(abs(cos(angles)) < 0.1, 0.5, ifelse(cos(angles) > 0, 0, 1)))

	  p <- ggplot() +
	    geom_polygon(data = df_grid, aes(x = x, y = y, group = level),
	                 fill = NA, color = "gray75", linewidth = 0.3) +
	    geom_segment(data = df_spokes, aes(x = x, y = y, xend = xend, yend = yend),
	                 color = "gray75", linewidth = 0.3) +
	    geom_polygon(data = df_prof, aes(x = x, y = y, group = Product, color = Product),
	                 fill = NA, linewidth = 0.8) +
	    geom_point(data = df_prof, aes(x = x, y = y, color = Product), size = 1.5) +
	    geom_text(data = df_lab, aes(x = x, y = y, label = label, hjust = hjust), size = 3) +
	    scale_color_manual(values = productColors[productNames]) +
	    coord_equal(clip = "off") +
	    scale_x_continuous(expand = expansion(mult = 0.35)) +
	    scale_y_continuous(expand = expansion(mult = 0.15)) +
	    labs(title = mainTitle, color = titleLegend) +
	    theme_void() +
	    theme(legend.position = if (legend) "right" else "none",
	          plot.title = element_text(hjust = 0.5, face = "bold"))
	  if (printLimit)
	  {
	    p <- p + annotate("text", x = 0, y = c(1, 0), label = c(maxS, minS), color = "gray50", size = 3)
	  }

	  print(p)
	  return(invisible(list(data = aggregatedData, plot = p)))
	}

	if(legend)
	{
		mat=matrix(1,nrow=1,ncol=4)
		mat[,4]=2
		layout(mat)
	}
	productColors2=productColors[productNames]
	radarchart(aggregatedData[,ncol(aggregatedData):1], maxmin=maxmin, axistype=0,axislabcol="grey", seg=4, plty=1, pty=32, plwd=1, cglcol="black", pcol=productColors2, title=mainTitle)
	if(printLimit)
	{
		text(0,1,maxScale,col="light grey")
		text(0,0,minScale,col="light grey")
	}
    if(legend)
	{
		par(mar=c(5,0,4,1))
		par(cex=0.9)
		plot.new()
		legend(0,1.04,names(productColors[productNames]),col=productColors[productNames],lwd=2,inset=0.01, title=titleLegend)
	}
	return(aggregatedData)
  #  graphics.off()
}