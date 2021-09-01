# figure 2 trend of all tweets --------------------------------------------

tweets_all$date # "2019-06-01" - "2020-01-31"
View(tweets_trend)

tweets_trend = tweets_all %>% group_by(date) %>% dplyr::summarise(total_tweets = length(tweet))


tweets_trend_plot = ggplot(tweets_trend, aes(x=factor(date),y=total_tweets,group = 1)) +
  geom_point(size=0.2) + geom_smooth(method = "loess",span = 0.1,size = 0.5)

tweets_trend_plot = tweets_trend_plot +scale_x_discrete(breaks = c("2019-06-01","2019-07-01",
                                                                   "2019-08-01","2019-09-01",
                                                                   "2019-10-01","2019-11-01",
                                                                   "2019-12-01","2020-01-01",
                                                                   "2020-01-31"),
                                                        labels =c("06-01","07-01",
                                                                  "08-01","09-01",
                                                                  "10-01","11-01",
                                                                  "12-01","01-01","01-31")) + 
  xlab("date")+ylab("daily volume of tweets from all state media") +
  theme(axis.text.x=element_text(angle = 45, size=15),axis.title=element_text(size=15),axis.text.y=element_text(size=15))

ggsave("Daily Volume of Tweets From All State Media.png",width = 12,height = 7,units = "in",plot = tweets_trend_plot)


# Figure 3 Temporal trends of the 30 topics -------------------------------
stm30_theta = stm.out_media_all$theta
stm_30_theta_<-as.data.frame((stm30_theta))
stm_30_theta_<-cbind(stm_30_theta_,meta_media_all$date)
stm_30_theta_<-cbind(stm_30_theta_,meta_media_all$ranking)
stm_30_theta_<-cbind(stm_30_theta_,meta_media_all$engagement_level)
stm_30_theta_<-cbind(stm_30_theta_,meta_media_all$number_of_day)


tweets_trend$date
theta_all_date=matrix(0,ncol=30,nrow=nrow(tweets_trend))

for (i in tweets_trend$date) {
  subset=stm_30_theta_[which(stm_30_theta_$`meta_media_all$date` == i),1:30]
  days_interval = as.numeric(as.Date(i) - as.Date("2019-05-31"))
  theta_all_date[days_interval,] = as.vector(colSums(as.matrix(subset)))
  theta_all_date[days_interval,]=theta_all_date[days_interval,]/sum(theta_all_date[days_interval,])
  }

days<-tweets_trend$date
all_trend<-data.frame(topic.prevalence=0,date=0,topic=0)

for (i in 1:30)
  {topici=data.frame(topic.prevalence=theta_all_date[,i],date=days,
                     topic=topic.labels[i])
  all_trend<-rbind(all_trend,topici)
}
all_trend=all_trend[-1,]
all_trend$topic<-as.factor(all_trend$topic)

topic.trend.plot<-ggplot(all_trend,aes(x=date,y=topic.prevalence,group=1))+
  stat_smooth(method = "lm", se = F)+
  facet_wrap(~topic,nrow=10,ncol=3,scales="free")+geom_point(size=0.1)+geom_line(color="darkblue",size=0.1)+
  ylab("Topic prevalence")+xlab("Date")+scale_x_discrete(breaks = c("2019-07-01",
                                                                    "2019-09-01",
                                                                    "2019-11-01",
                                                                    "2020-01-01"),
                                                         labels =c("07-01",
                                                                   "09-01",
                                                                   "11-01",
                                                                   "01-01"))
