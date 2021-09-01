new_tweets_hk$date %>% max()

save(new_tweets_hk,file= "new_tweets_hk.RData")

new_tweets_hk_nov = new_tweets_hk[new_tweets_hk$date <= "2019-11-30",]
new_tweets_hk_nov$date %>% max()

processed_media_all_nov <- textProcessor(new_tweets_hk_nov$new_tweets, metadata=new_tweets_hk_nov)
out_media_all_nov <- prepDocuments(processed_media_all_nov$documents, 
                                   processed_media_all_nov$vocab, 
                                   processed_media_all_nov$meta,lower.thresh = 1)
meta_media_all_nov <-out_media_all_nov$meta
stm.out_media_all_nov <- stm(out_media_all_nov$documents, out_media_all_nov$vocab, K=30,
                         prevalence=~s(number_of_day)+ranking+engagement_level,
                         data=out_media_all_nov$meta, max.em.its = 275,seed=286430)

summary(stm.out_media_all_nov,n=10)
