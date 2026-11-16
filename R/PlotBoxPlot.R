#'@importFrom graphics boxplot par text abline title segments points
#'@importFrom stats na.omit
#'@importFrom grDevices colorRampPalette rainbow
PlotBoxPlot=function(dataFrame,resAnova, minScore=NULL, maxScore=NULL, overallBoxplot=TRUE, plotMeans=TRUE, plotStats=TRUE, groupId="both",main="Distribution of scores",subtitle="",xlab="Product",ylab="Score", plotType="base", plotPoints=TRUE)
{

	X=dataFrame
	if(!exists("minScore") || is.null(minScore))
	{
		minScore=min(X[,"Y"])
	}
	if(!exists("maxScore") || is.null(maxScore))
	{
		maxScore=max(X[,"Y"])
	}
	if (is.null(resAnova))
	{
		groupId="None"
	}

	model=as.formula(paste("Y~ProductCode",sep=""))
	boxplotObject=boxplot(model, X,plot=FALSE,na.action=na.omit )
	productsMean=aggregate(model, X, mean,na.rm=TRUE)[,2]
	productsSd=aggregate(model, X, sd,na.rm=TRUE)[,2]
	nbObs=boxplotObject$n
	medians=boxplotObject$stats[3,]
	sortVector=sort.int(productsMean,index.return=TRUE)$ix
	sortedProductsMean=productsMean[sortVector]
	sortedMedians=medians[sortVector]
	sortedNbObs=nbObs[sortVector]
	sortedBoxplotStats=boxplotObject$stats[,sortVector]
	sortedBoxplotNames=boxplotObject$names[sortVector]
	sortedProductsSd=productsSd[sortVector]
	
	
	if (plotType == "gg") {
	  
	  library(ggplot2)
	  
	  # Données ordonnées
	  products <- sortedBoxplotNames
	  nbProducts <- length(products)
	  
	  df_plot <- X
	  df_plot$ProductCode <- factor(
	    df_plot$ProductCode,
	    levels = products
	  )
	  
	  # Groupes de significativité
	  df_groups <- data.frame(
	    ProductCode = products,
	    stringsAsFactors = FALSE
	  )
	  
	  # productGroups n'est calculé que dans la branche "base" : on le calcule ici
	  # (colonne 7 = ".group" d'emmeans ; on enlève les espaces de cadrage, ex. " 23 ")
	  if (nbProducts > 1 && !is.null(resAnova)) {
	    productGroups <- as.data.frame(resAnova[[1]]$MoyProd)
	    colnames(productGroups)[7] <- "Groups"

	    df_groups$Groups <- gsub(" ", "", as.character(productGroups[
	      match(products, as.character(productGroups[, "ProductCode"])),
	      "Groups"
	    ]))

	  } else {
	    df_groups$Groups <- "1"
	  }
	  
	  # Conversion des groupes en lettres
	  df_groups$Letters <- vapply(
	    df_groups$Groups,
	    function(g) {
	      if (is.na(g) || g == "") return("")
	      ids <- as.numeric(strsplit(g, "")[[1]])
	      paste(letters[ids], collapse = "")
	    },
	    character(1)
	  )
	  
	  # Palette des groupes
	  maxG <- max(
	    as.numeric(unlist(strsplit(
	      na.omit(df_groups$Groups), ""
	    ))),
	    na.rm = TRUE
	  )
	  
	  if (!is.finite(maxG)) maxG <- 1
	  
	  groupColors <- rainbow(maxG)
	  names(groupColors) <- letters[seq_len(maxG)]
	  
	  # Couleurs des produits
	  df_groups$Color <- vapply(
	    df_groups$Groups,
	    function(g) {
	      ids <- as.numeric(strsplit(g, "")[[1]])
	      ids <- ids[!is.na(ids)]
	      
	      if (length(ids) == 0) return("lightgray")
	      
	      colors <- groupColors[ids]
	      
	      if (length(colors) == 1) return(colors)
	      
	      # Couleur moyenne pour les produits appartenant
	      # à plusieurs groupes
	      grDevices::colorRampPalette(colors)(3)[2]
	    },
	    character(1)
	  )
	  
	  # Statistiques par produit
	  df_stats <- data.frame(
	    ProductCode = products,
	    Mean = sortedProductsMean[seq_len(nbProducts)],
	    N = sortedNbObs[seq_len(nbProducts)],
	    Median = sortedMedians,
	    Q1 = sortedBoxplotStats[2, seq_len(nbProducts)],
	    Q3 = sortedBoxplotStats[4, seq_len(nbProducts)]
	  )
	  
	  # match() plutôt que merge() : garde l'ordre et le facteur ProductCode
	  idx <- match(as.character(df_plot$ProductCode), products)
	  df_plot$Letters <- df_groups$Letters[idx]
	  df_plot$Color <- df_groups$Color[idx]
	  df_stats$Letters <- df_groups$Letters
	  df_stats$Color <- df_groups$Color
	  df_stats$ProductCode <- factor(df_stats$ProductCode, levels = products)
	  
	  # Position des lettres
	  df_stats$LetterY <- maxScore +
	    2 * (maxScore - minScore) / 100
	  
	  # Graphique
	  p <- ggplot(
	    df_plot,
	    aes(x = ProductCode, y = Y)
	  ) +
	    
	    geom_boxplot(
	      aes(fill = Color),
	      width = 0.65,
	      color = "black",
	      linewidth = 0.6,
	      outlier.shape = if (plotPoints) NA else 2,
	      na.rm = TRUE
	    ) +

	    scale_fill_identity() +

	    # Points individuels (jitter horizontal seulement, pour ne pas fausser les scores)
	    {
	      if (plotPoints) {
	        geom_jitter(
	          width = 0.15,
	          height = 0,
	          shape = 16,
	          size = 1.5,
	          alpha = 0.5,
	          color = "gray20",
	          na.rm = TRUE
	        )
	      }
	    } +
	    
	    # Moyennes
	    {
	      if (plotMeans) {
	        geom_point(
	          data = df_stats,
	          aes(x = ProductCode, y = Mean),
	          inherit.aes = FALSE,
	          shape = 3,
	          color = "blue",
	          size = 3
	        )
	      }
	    } +
	    
	    # Lettres de significativité
	    {
	      if (groupId %in% c(
	        "letters", "Letters",
	        "both", "Both", "ColorsAndLetters"
	      )) {
	        geom_text(
	          data = df_stats,
	          aes(
	            x = ProductCode,
	            y = LetterY,
	            label = Letters
	          ),
	          inherit.aes = FALSE,
	          size = 4
	        )
	      }
	    } +
	    
	    # Moyennes et effectifs
	    {
	      if (plotStats) {
	        list(geom_text(
	          data = df_stats,
	          aes(
	            x = ProductCode,
	            y = minScore -
	              5 * (maxScore - minScore) / 100,
	            label = paste0("m=", round(Mean, 3))
	          ),
	          inherit.aes = FALSE,
	          color = "blue",
	          size = 3
	        ),
	          geom_text(
	            data = df_stats,
	            aes(
	              x = ProductCode,
	              y = minScore -
	                8 * (maxScore - minScore) / 100,
	              label = paste0("n=", N)
	            ),
	            inherit.aes = FALSE,
	            color = "blue",
	            size = 3
	          ))
	      }
	    } +
	    
	    scale_y_continuous(
	      limits = c(
	        minScore - 10 * (maxScore - minScore) / 100,
	        maxScore + 5 * (maxScore - minScore) / 100
	      )
	    ) +
	    
	    labs(
	      title = main,
	      subtitle = subtitle,
	      x = xlab,
	      y = ylab
	    ) +
	    
	    theme_classic() +
	    
	    theme(
	      axis.text.x = element_text(
	        angle = 45,
	        hjust = 1,
	        vjust = 1
	      ),
	      legend.position = "none"
	    )
	  
	  # Boxplot global
	  if (overallBoxplot && nbProducts > 1) {
	    
	    df_overall <- data.frame(
	      ProductCode = factor("Overall",
	                           levels = c(products, "Overall")),
	      Y = X[, "Y"]
	    )
	    
	    p <- p +
	      geom_boxplot(
	        data = df_overall,
	        aes(x = ProductCode, y = Y),
	        inherit.aes = FALSE,
	        fill = "lightblue",
	        color = "black",
	        width = 0.65,
	        linewidth = 0.6,
	        outlier.shape = if (plotPoints) NA else 2,
	        na.rm = TRUE
	      ) +

	      {
	        if (plotPoints) {
	          geom_jitter(
	            data = df_overall,
	            aes(x = ProductCode, y = Y),
	            inherit.aes = FALSE,
	            width = 0.15,
	            height = 0,
	            shape = 16,
	            size = 1.5,
	            alpha = 0.5,
	            color = "gray20",
	            na.rm = TRUE
	          )
	        }
	      } +

	      geom_vline(
	        xintercept = nbProducts + 0.5,
	        color = "gray"
	      ) +
	      scale_x_discrete(limits = c(products, "Overall"))
	    
	  }
	  
	  print(p)

	  # Retour identique à la version base (stats + lettres des groupes)
	  statsMatrix <- matrix(sortedBoxplotStats, nrow = 5)
	  colnames(statsMatrix) <- products
	  if (overallBoxplot && nbProducts > 1) {
	    statsMatrix <- cbind(statsMatrix,
	                         boxplot(X[, "Y"], plot = FALSE)$stats)
	    colnames(statsMatrix)[ncol(statsMatrix)] <- "Overall"
	  }
	  rownames(statsMatrix) <- c("Min", "Q1", "Med", "Q3", "Max")
	  productLetter <- df_groups$Letters
	  names(productLetter) <- products
	  listResults <- list(stats = statsMatrix, groups = productLetter, plot = p)
	  return(invisible(listResults))
	}
	
	if(plotType=="base")
	{
	nbBoxplots=max(1,ncol(sortedBoxplotStats))
	nbBoxplotsWithoutOverall=nbBoxplots

	if (nbBoxplots<2)
	{
		overallBoxplot=FALSE
	}

	# Couleur des groupes
	boxPlotColors = rep("light gray",nbBoxplots)

	# trac? du boxplot de la distribution
	if (overallBoxplot == TRUE)
	{
		overallStats=boxplot(X[,"Y"], plot=FALSE,na.action=na.omit )
		sortedBoxplotStats=cbind(sortedBoxplotStats, overallStats$stats)
		sortedBoxplotNames=c(sortedBoxplotNames,"Overall")
		sortedProductsMean=c(sortedProductsMean,mean(X[,"Y"], na.rm=TRUE))
		boxPlotColors=c(boxPlotColors, "light blue")
		nbBoxplots = nbBoxplots+1
		sortedNbObs=c(sortedNbObs,sum(sortedNbObs))
	}
	colnames(sortedBoxplotStats)=sortedBoxplotNames
	rownames(sortedBoxplotStats)=c("Min","Q1","Med","Q3","Max")
	# Groupes des produits
	if(length(sortedBoxplotNames)>1 && !is.null(resAnova))
	{
		productGroups= resAnova[[1]]$MoyProd
		colnames(productGroups)[7]="Groups"

	} else
	{
		productGroups=list()
		productGroups[["Groups"]]=c("a")
	}

	# Trac? des boxplots produits
	xlabel=xlab
	ylabel=ylab
	boxplot(sortedBoxplotStats, boxfill=boxPlotColors,fg="red", pch=2, xlab=xlabel, ylab=ylabel,
				   lwd="2", names=rep("",nbBoxplots), medlwd="2", boxlwd="1", staplelwd="1", whisklty=3, whisklwd=1,
				   outlwd="1", ylim=c(minScore-7*(maxScore-minScore)/100,maxScore+3*(maxScore-minScore)/100),border=c("black"), xlim=c(0.5,nbBoxplots)+0.25)

	# Etiquette des produits
	text(c(1:length(sortedBoxplotNames)), par("usr")[3]-0.2, labels = sortedBoxplotNames, srt = 45, adj = c(1,1), xpd = TRUE, cex=1.1)

	# L?gende round
	yVar=ylab;xVar=xlab
	mainTitle=paste(ylab,": ","Distribution of", " ", yVar, " ", "by", " ",xVar,sep="")
	subTitle=NULL
	if(groupId %in% c("Colors","colors"))
	{
		subTitle=paste(subTitle, "Group Means Color",sep="")
	}
	if(groupId %in% c("both","Both","ColorsAndLetters"))
	{
		subTitle=paste(subTitle, "Group Means Or Color",sep="")
	}
	if(groupId %in% c("letters","Letters"))
	{
		subTitle=paste(subTitle, "Group Means",sep="")
	}
	title(main=mainTitle,sub=subTitle,cex.sub=0.8)

	if (overallBoxplot == TRUE)
	{
		abline(v=nbBoxplots-0.5,col="gray")
	}

	productLetter=NULL

	# D?grad?
	if(groupId %in% c("colors","Colors") || groupId %in% c("both","ColorsAndLetters"))
	{
		products=productGroups[,"ProductCode"]
		productLetter=rep("",nbBoxplotsWithoutOverall)
		# Nombre de groupes
		maxG=0
		for(i in 1:nbBoxplotsWithoutOverall)
		{
			letter=""
			gg=strsplit(productGroups[productGroups[,"ProductCode"]==products[i],"Groups"],"")
			for (ggg in gg[[1]])
			{
				if (ggg!=" ")
				{
					letter=paste(letter,letters[as.numeric(ggg)],sep="")
					maxG=max(as.numeric(ggg),maxG)
				}
				}
			productLetter[i]=letter
		}
		names(productLetter)=products
		nbGroups=maxG
		groups=letters[1:nbGroups]

		# Couleur des groupes
		groupColors = rainbow(nbGroups)
		names(groupColors)=groups

		# Trace du rectangle degrade
		for (i in 1:nbBoxplotsWithoutOverall)
		{
			currentProductGroup=productLetter[sortedBoxplotNames[i]]
			leftColor=groupColors[[substring(currentProductGroup,1,1)]]
			indleft=which(groupColors==leftColor)[[1]]
			rightColor=groupColors[[substring(currentProductGroup,nchar(as.character(currentProductGroup)),1000)]]
			indright=which(groupColors==rightColor)[[1]]
			if(nchar(currentProductGroup)%in% c(2,3))
			{
				if(nchar(currentProductGroup)==2)
				{
				vecColor=colorRampPalette(c(leftColor,rightColor))(100)
				}
				else
				{
				middleColor=groupColors[[substring(currentProductGroup,2,2)]]
				vecColor=colorRampPalette(c(leftColor,middleColor,rightColor))(100)
				}
			} else
			{
				vecColor=groupColors[indleft:indright]
			}
			PlotMultiColorRectangle(xleft=i-0.4,ybottom=sortedBoxplotStats[2,i],xright=i+0.4,ytop=sortedBoxplotStats[4,i],vecColor)

			# Mediane
			segments(i-0.4,sortedMedians[i],i+0.4,sortedMedians[i],col="black")
		}
	}

	# Affichage des lettres des groupes
	if(groupId %in% c("letters","Letters") || groupId %in% c("both","ColorsAndLetters"))
	{
		for(i in 1:nbBoxplotsWithoutOverall)
		{
			text(i,maxScore+2*(maxScore-minScore)/100,productLetter[sortedBoxplotNames[i]])
		}
	}

	# Trac? des moyennes
	if (plotMeans==TRUE)
	{
		for(i in 1:nbBoxplots)
		{
			points(i, sortedProductsMean[i],pch=3,lwd=1,col="blue")
		}
	}

	# Statistiques
	if (plotStats==TRUE)
	{
		for(i in 1:nbBoxplots)
		{
			text(i,minScore-5*(maxScore-minScore)/100,label=paste("m=",round(sortedProductsMean[i],3),sep=""),col="blue",cex=0.7)
			text(i,minScore-8*(maxScore-minScore)/100,label=paste("n=",sortedNbObs[i],sep=""),col="blue",cex=0.7)
		}
	}
	listResults=list(sortedBoxplotStats,productLetter)
	names(listResults)=c("stats","groups")
	return(listResults)
	}
}
