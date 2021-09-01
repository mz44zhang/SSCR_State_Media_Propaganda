# now run the STM ---------------------------------------------------------
processed_media_all <- textProcessor(new_tweets_hk$new_tweets, metadata=new_tweets_hk)
out_media_all <- prepDocuments(processed_media_all$documents, processed_media_all$vocab, processed_media_all$meta,lower.thresh = 1)
meta_media_all <-out_media_all$meta
stm.out_media_all <- stm(out_media_all$documents, out_media_all$vocab, K=30,
                         prevalence=~s(number_of_day)+ranking+engagement_level,
                         data=out_media_all$meta, max.em.its = 275,seed=286430)

summary(stm.out_media_all,n=10)
labelTopics(stm.out_media_all,n=10)


# table 2 topic summary - with top 10 word in each topic & topic percentage --------
labelTopics(stm.out_media_all,n=10)
stm_topicproportion<-colSums(stm.out_media_all$theta)/nrow(stm.out_media_all$theta)
data.frame(proportion = stm_topicproportion)



# topic labels
topic.labels = c("topic_1","topic_2","topic_3","topic_4","topic_5",
                 "topic_6","topic_7","topic_8","topic_9","topic_10",
                 "topic_11","topic_12","topic_13","topic_14","topic_15",
                 "topic_16","topic_17","topic_18","topic_19","topic_20",
                 "topic_21","topic_22","topic_23","topic_24","topic_25",
                 "topic_26","topic_27","topic_28","topic_29","topic_30")

topics = topic.labels



# topics & terms by proporton
plot.STM(stm.out_media_all,type = "summary", text.cex=.8, n=10)

# plot topic occurrence
corr.out_media_all <- topicCorr(stm.out_media_all,method = "huge")
plot_topic_occur = plot.topicCorr(corr.out_media_all,vertex.color = "grey")


# representative tweets
representative.answers<-list()
for (i in 1:30){
  representative.answers[[i]]<-findThoughts(stm.out_media_all,
                                            text=meta_media_all$new_tweets,
                                            n=3,topics=i)$docs[[1]]
}

representative.answers[[1]]


par(mfrow = c(3, 2),mar = c(1, 1, 1, 1))
for (i in 1:6) {
  c=paste0("Topic ",i)
  plotQuote(representative.answers[[i]], width = 50, main = c)
}

for (i in 7:12) {
  c=paste0("Topic ",i)
  plotQuote(representative.answers[[i]], width = 50, main = c)
}

for (i in 13:18) {
  c=paste0("Topic ",i)
  plotQuote(representative.answers[[i]], width = 50, main = c)
}

for (i in 19:24) {
  c=paste0("Topic ",i)
  plotQuote(representative.answers[[i]], width = 50, main = c)
}

for (i in 25:30) {
  c=paste0("Topic ",i)
  plotQuote(representative.answers[[i]], width = 50, main = c)
}
