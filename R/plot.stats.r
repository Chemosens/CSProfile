plot.stats=function(df_stats,size=1,selec=NULL)
{
    name=NULL    
    if(!is.null(selec)){df_stats=df_stats[df_stats[,"name"]%in%selec,]}
    p<-ggplot(df_stats,aes(x=name,y=mean,fill=name))
    p<-p+geom_col(na.rm=T,position="dodge")
    p<-p+geom_errorbar(aes(ymin=min,ymax=max),colour="black")
    p<-p+theme(legend.position = "none",axis.text.x = element_text( angle=45,hjust=1,size=size))
    return(p)
}

