#' This function allows to draw spectra according to a group and potential smoothing
#' @param x abscissa of the spectra
#' @param y ordinate of the spectra
#' @param group group for each spectra
#' @param size size of the text
#' @param integrationTable NULL
#' @param xlim vector of two numbers representing the limits of x axis.
#' @param ylim vector of two numbers representing the limits of y axis.
#' @param xlab character indicating the label on the x axis
#' @param ylab character indicating the label on the y axis
#' @param legend if TRUE the legend is displayed
#' @param main title of the graph
#'@param ... Further parameters in plots such as xlim,xlab,ylim,ylab
#' @export
#' @import ggplot2
plot_bar=function(x,y,group=NULL,integrationTable=NULL,xlim=NULL,ylim=NULL,main="Barplot",xlab="Name",ylab="Intensity",legend=TRUE,size=1,...)
{
  ind=!is.na(x)&!is.na(y)
  x=x[ind]
  y=y[ind]
  group=group[ind]
   if(is.null(group)){ df_gg=data.frame(x=x,y=y)
  }
  else{ df_gg=data.frame(x=x,y=y,group=group)
  }
  
  if(is.null(xlim)){xlim=c(min(df_gg[,"x"]),max(df_gg[,"x"]))}
  if(is.null(ylim)){ylim=c(min(df_gg[,"y"]),max(df_gg[,"y"]))}
  if(is.null(group))
    {p<-p<-ggplot(df_gg,aes(x=x,y=y))}
    else{
      p<-ggplot(df_gg,aes(x=x,y=y,group=group,fill=as.factor(group)))
    }
    p<-p+geom_col(na.rm=T,position="dodge")

  p<-p+labs(title=main,x=xlab,y=ylab)
 # if(!is.null(integrationTable))
  # {
  #    decreasingIntIndex=order(y,decreasing=T)
  #    orderedSumy=y[decreasingIntIndex]
  #    orderedx=x[decreasingIntIndex]
  #    orderedIons=rep(NA,length(decreasingIntIndex))
  #    for(label in integrationTable[,"name"])
  #    {
  #      x_index=which(
  #        orderedx>=integrationTable[integrationTable[,"name"]==label,"inf"] 
  #        &orderedx<=integrationTable[integrationTable[,"name"]==label,"sup"]
  #      )
  #      orderedIons[x_index]=label
  #      df_i=data.frame(x=orderedx[x_index],y=orderedSumy[x_index])
  #      
  #      
  #      
  #      p<- p + geom_line(data=df_i,aes(x=x,y=y),color="green")
  #      # p<- p + geom_point(data=df_i,aes(x=x,y=y,text=label),color="green")
  #    }
  # #   orderdf=data.frame(x=orderedx,Sumy=orderedSumy,orderedIons=orderedIons)
  # #   return(list(df=orderdf[orderdf[,"Sumy"]>threshold,],p=p))
  #    return(p)
  #  }
  # else
  # {
  
  if(!legend)
  {
    p=p+ theme(legend.position = "none",axis.text.x = element_text( angle=45,hjust=1,size=size))
  }
  if(legend)
  {
    p=p+ theme(axis.text.x = element_text( angle=45,hjust=1,size=size))
    
  }
  
    return(p)
  #}
  
}