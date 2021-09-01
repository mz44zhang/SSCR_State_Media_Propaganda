#function of plotting topic differences
topic.difference.ranking.reg<-list()
for (i in 1:30) {
  topic.difference.ranking.reg[[i]]<-lm(stm_30_theta_[,i]~stm_30_theta_[,32])
  }
topic.difference.engagement.reg<-list()
for (i in 1:30) {
  topic.difference.engagement.reg[[i]]<-lm(stm_30_theta_[,i]~stm_30_theta_[,33])
}

# new ---------------------------------------------------------------------

plot.topic.difference(topic.difference.ranking.reg,c(-0.05,0.05),
                      "Differences in topical prevalence low vs. high bureaucratic rank")
plot.topic.difference(topic.difference.engagement.reg,c(-0.05,0.05),
                      "Differences in topical prevalence low vs. high engagement level")

ci_ranking = get_ci(topic.difference.ranking.reg)
ci_engagement = get_ci(topic.difference.engagement.reg)
# functions ---------------------------------------------------------------


plot.topic.difference<-function(reglist,xlim,xlab){
  cis<-list()
  means<-list()
  for (i in 1:30) {
    cis[[i]]<-confint(reglist[[i]])[c(2,4)]
    means[[i]]<-mean(cis[[i]])
  }
  ylim <- c(0, length(topics)+1)
  xlim <- xlim
  xlab <- xlab
  ylab <- ""
  plot(0, 0,col="white",xlim=xlim, ylim=ylim, main="",
       xlab=xlab, ylab=ylab,yaxt="n")
  lines(c(0,0), c(0, length(topics)+2), lty=2)
  
  labels = topics
  it <- length(topics)
  for(i in 1:length(topics)){
    points(means[[i]], it, pch=16)
    lines(c(cis[[i]][1],cis[[i]][2]),c(it,it))
    if(means[[i]]>=0){
      axis(2,at=it, labels=stringr::str_wrap(labels[i],width=20),las=1, cex=0.1, tick=F, pos=cis[[i]][1])
    }
    else
    {
      axis(4,at=it, labels=stringr::str_wrap(labels[i],width=20),las=1, cex=0.1, tick=F, pos=cis[[i]][2])
      
    }
    it = it-1
  }
}

get_ci<-function(reglist){
  cis<-list()
  means<-list()
  for (i in 1:30) {
    cis[[i]]<-confint(reglist[[i]])[c(2,4)]
    means[[i]]<-mean(cis[[i]])
  }
  return(cis)
}
