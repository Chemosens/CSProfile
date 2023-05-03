#'@importFrom graphics rect segments
PlotMultiColorRectangle=function(xleft,ybottom,xright,ytop,colorsVector)
{
		inc=(xright-xleft)/length(colorsVector)

		# 1 rectangle par couleur
		for (i in 1:100)
		{
			rect(xleft+(i-1)*inc, ybottom, xleft+i*inc, ytop-0.01, col=colorsVector[i],border="transparent")
		}

		# Contours du rectangle
		segments(xleft,ybottom,xright,ybottom,col="black")
		segments(xleft,ytop,xright,ytop,col="black")
		segments(xleft,ybottom,xleft,ytop,col="black")
		segments(xright,ytop,xright,ybottom,col="black")
}