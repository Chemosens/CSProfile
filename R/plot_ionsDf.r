#' @import ggplot2
plot_ions_df=function(ions_df,xmin=NULL,xmax=NULL,ymin=NULL,ymax=NULL,legend=TRUE,smoothing="none",main="Ions",xlab="x",ylab="Intensity")
{
  x=y=group=NULL
  if(is.null(xmin)){xmin=min(ions_df$x)}
  if(is.null(xmax)){xmax=max(ions_df$x)}
  if(is.null(ymin)){ymin=min(ions_df$y)}
  if(is.null(ymax)){ymax=max(ions_df$y)}
  if(!"group" %in% colnames(ions_df))
  {
    p1=ggplot(data=ions_df, aes(x=x, y=y,show.legend = FALSE))
  }
  else
  {
    p1=ggplot(data=ions_df, aes(x=x, y=y,color=group,show.legend = FALSE))
  }
  p1=p1+ 
    xlim(xmin,xmax)+
    ylim(ymin,ymax)+
    geom_line(size=0.5) +ggtitle(main) + xlab(xlab) +theme_bw()
  if(!legend)
  {
    p1=p1+ theme(legend.position = "none")
  }
  return(p1)
}