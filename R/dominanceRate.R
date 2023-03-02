#' @title Compute the dominance rate.
#' @description Compute the dominance rate.
#' @param dominances Dataframe with columns: rep/product/subject/descriptor/time/score(0/1)/period
#' @param alpha Alpha risk for the binomial test.
#' @param significance If TRUE, returns the significance
#' @param percentage If TRUE, returns the percentage, ifelse the absolute count
#' @return A list with dominanceRate, significanceLevel and parameters?
#' @export
#' @importFrom stats qnorm prop.test
dominanceRate=function(dominances, significance=TRUE,boldComparison=FALSE,percentage=TRUE,alpha=0.05) {
  
  getSignificanceLevel=function(nbDescriptors, nbObservations, alpha)
  {
    p0=1/nbDescriptors;
    z=round(qnorm(1-alpha),3); 
    if (nbObservations > 0) {
      ps=p0+z*sqrt(p0*(1-p0)/nbObservations)
    } else {
      ps=0
    }
    return (ps)
  }
  
  nbDescriptors=length(unique(dominances$descriptor))
  nbProducts=length(unique(dominances$product))
  if(nbProducts==1){boldComparison=FALSE}
  # Nombre de juges
  l1=aggregate(score~subject+rep+product,dominances,max)
  l2=aggregate(score~rep+product,l1,sum)
  colnames(l2)[3]="nbSubjects"
  
  # Seuil de significativité par produit/rep
  if(significance)
  {
    l2$significanceLevel=round(sapply(l2$nbSubjects, function(x) getSignificanceLevel(nbDescriptors, x, alpha)*100),2)
    l2$chanceLevel=round(100/nbDescriptors,2)
  }
  
  # Pourcentages de dominances toutes périodes confondues
  m=aggregate(score~rep+product+period+time+descriptor,dominances,sum)
  m=merge(m,l2)
  m=m[m$nbSubjects>0,]
  if(percentage)
  {
    m$percentage=m$score/m$nbSubjects*100
    if(significance)
    {
      m$percentageAboveSignificance=m$percentage-m$significanceLevel
      m[m$percentageAboveSignificance<0,"percentageAboveSignificance"]=0
    }
  
  }
  else
  {
    m$percentage=m$score
  }
   
  m=m[order(m$rep,m$product,m$period,m$time,m$descriptor), ]
  
  
  # Dominance moyenne sur les autres produits
  
  m3=NULL
  if(boldComparison)
  {
    for (p in unique(m$product)) {

     m2a=aggregate(score~rep+period+time+descriptor,m[m$product!=p,],sum,na.rm=TRUE)
     colnames(m2a)[5]="scoreRefOther"
     m2a$product=p
     #m3=rbind(m3, m2)
     #m=merge(m, m2,all.x=TRUE,by=c("rep", "period", "time", "descriptor", "product"))
     m2b=aggregate(nbSubjects~rep+period+time+descriptor,m[m$product!=p,],sum,na.rm=TRUE)
     colnames(m2b)[5]="nbSubjectsRefOther"
     m2b$product=p
     #m=merge(m, m2,all.x=TRUE,by=c("rep", "period", "time", "descriptor", "product"))
     m3=rbind(m3,merge(m2a, m2b,all.x=TRUE,by=c("rep", "period", "time", "descriptor", "product")))
  }
 
  m=merge(m, m3,all.x=TRUE,by=c("rep", "period", "time", "descriptor", "product"))
  }
  
  # Dominance moyenne
  m2=aggregate(score~rep+period+time+descriptor,m,sum)
  colnames(m2)[5]="scoreRef"
  m=merge(m, m2,all.x=TRUE)
  m2=aggregate(nbSubjects~rep+period+time+descriptor,m,sum,na.rm=TRUE)
  colnames(m2)[5]="nbSubjectsRef"
  m=merge(m, m2,all.x=TRUE)
  
  f1=function(x) {
    if (x["scoreRef"]>0) {
      res=prop.test(c(x["score"],x["nbSubjects"]),c(x["scoreRef"],x["nbSubjectsRef"]),alternative = "two.sided", conf.level=alpha)
      return (res$p.value)
    } else {
      return (1)
    }
  }
  a=apply(m[,c("score","nbSubjects","scoreRef","nbSubjectsRef")],1,f1)
  m$meanComparison=a
  
  # f2=function(x) {
  #   if (x["scoreRefOther"]>0) {
  #     res=prop.test(c(x["score"],x["nbSubjects"]),c(x["scoreRefOther"],x["nbSubjectsRefOther"]),alternative = "two.sided", conf.level=alpha)
  #     return (res$p.value)
  #   } else {
  #     return (1)
  #   }
  # }
  # a=apply(m[,c("score","nbSubjects","scoreRefOther","nbSubjectsRefOther")],1,f2)
  # m$meanComparisonOther=a
  #TOFIX : si x > n dans prop.test
  
  # TODO : à vérifier
  
  ## Calcul des paramètres

  if(significance)
  {
    # Pourcentage maximal de dominance
    maxPercentage = aggregate(percentage~rep+product+descriptor+period, m[m$period>0,], max)
    colnames(maxPercentage)[5]="pmax"
    
    # Premier temps auquel la significativité est atteinte
    
    mm=merge(m,maxPercentage)
    mm3=mm[mm$pmax==mm$percentage,]
    mm4=mm[mm$significanceLevel<=mm$percentage,]
    
    # Premier temps auquel pmax est atteint
    timeOfMaxPercentage=aggregate(time~rep+product+descriptor+period, mm3,max)
    colnames(timeOfMaxPercentage)[5]="tmax"
    
    # Durée pendant laquelle pmax est atteint
    durationOfMaxPercentage=aggregate(time~rep+product+descriptor+period, mm3,length)
    colnames(durationOfMaxPercentage)[5]="dmax"
    
    # Aire sous la courbe
    auc=aggregate(percentage~rep+product+descriptor+period, mm, sum)
    colnames(auc)[5]="auc"
    timeOfSignifPercentage=aggregate(time~rep+product+descriptor+period, mm4,max)
    colnames(timeOfSignifPercentage)[5]="tsig"
    # Durée pendant laquelle la significativité est atteinte
    durationOfSignifPercentage=aggregate(time~rep+product+descriptor+period, mm4,length)
    colnames(durationOfSignifPercentage)[5]="dsig"
    
    # Aire sous la courbe au dessus de la dominance
    ass=aggregate(percentageAboveSignificance~rep+product+descriptor+period, mm, sum)
    colnames(ass)[5]="ass"
    parameters=merge(maxPercentage,timeOfSignifPercentage,all.x=TRUE)
    parameters=merge(parameters,durationOfSignifPercentage,all.x=TRUE)
    parameters=merge(parameters,ass,all.x=TRUE)
    parameters=merge(parameters,timeOfMaxPercentage,all.x=TRUE)
    parameters=merge(parameters,durationOfMaxPercentage,all.x=TRUE)
    parameters=merge(parameters,auc,all.x=TRUE)
    parameters$ass=round(parameters$ass,2)
  }
  else{parameters=NULL;l2=NA}
  
  # Durée pendant laquelle la proportion est différente de la moyenne
  if(boldComparison)
  {
    times=sort(unique(m$time))
    inc=times[2]-times[1]
    durationOfDifference=aggregate(time~rep+product+descriptor+period, m[m$meanComparison<=alpha,],function(x){length(x[!is.na(x)])})
    colnames(durationOfDifference)[5]="ddif"
    durationOfDifference$ddif=durationOfDifference$ddif*inc
  }
  # Tableau des paramètres
  if(significance)
  {
    parameters=merge(parameters,durationOfSignifPercentage,all.x=TRUE)
    parameters=merge(parameters,ass,all.x=TRUE)
    parameters=merge(parameters,timeOfMaxPercentage,all.x=TRUE)
    parameters=merge(parameters,durationOfMaxPercentage,all.x=TRUE)
    parameters=merge(parameters,auc,all.x=TRUE)
    parameters$ass=round(parameters$ass,2)
  }
  if(boldComparison)
  {
    
    parameters=merge(parameters,durationOfDifference,all.x=TRUE)
    
  }
  
  return (list(dominanceRate=m, significanceLevel=l2, parameters=parameters))
  
}