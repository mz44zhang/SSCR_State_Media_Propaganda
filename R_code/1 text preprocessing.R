load("tweets_all.RData")
load("stm.out_media_all.RData")
options(scipen = 200)
install.packages(c("stm","lda","dplyr","stringr","Hmisc","devtools","tm"))
install_github("trinker/qdapRegex")
install.packages("ggpubr")
library(stm)
library(lda)
library(dplyr)
library(stringr)
library(Hmisc)
library(devtools)

library(qdapRegex)
library(tm)
library(grid)
library(ggpubr)
library(gridExtra)
library(huge)

# remove all inrelevant tweets ------------------------
# 1. first: do not delete hashtag; for the original tweet_no_url, detect if tweet_no_url contains 
# bishkek|southkorea|northkorea|hkex|corona|infection|pneumonia|exhibit
# stock|e-commerce|ipo|giant|ocean park|fifa|world cup|painting
# fivb|jewel|sports|sotheby|qing dynasty|dprk
new_remove_tweets = tweets_all %>% filter(str_detect(tweet_no_url,
                                                     regex(c("bishkek|southkorea|northkorea|hkex|corona|infection|pneumonia|exhibit|stock|e-commerce|ipo|giant|ocean park|fifa|world cup|painting|fivb|jewel|sports|sotheby|qing dynasty|dprk"),ignore_case = TRUE)))
new_remain_tweets_hashtag =tweets_all %>% anti_join(new_remove_tweets)

# select hk related tweets before cleaning hashtags 
new_tweets_hk <- new_remain_tweets_hashtag %>% 
  filter(str_detect(tweet_no_url,
                    regex(c("hongkong|hk|hong kong"),ignore_case = TRUE)))

# preprocess the text  ----------------------------------------------------

new_tweets_hk$tweet_clean=gsub('@\\S+', '',new_tweets_hk$tweet_no_url)
new_tweets_hk$tweet_clean=rm_url(new_tweets_hk$tweet_clean,pattern = pastex("@rm_twitter_url","@rm_url"))
new_tweets_hk$tweet_clean=gsub("pic.twitter.com\\S+\\s*", "", new_tweets_hk$tweet_clean)
new_tweets_hk$tweet_clean=gsub('\\b+RT', '',new_tweets_hk$tweet_clean)
new_tweets_hk$tweet_clean=gsub('#\\S+', '',new_tweets_hk$tweet_clean)
new_tweets_hk$tweet_clean=gsub('[[:cntrl:]]', '',new_tweets_hk$tweet_clean)
new_tweets_hk$tweet_clean=gsub("\\d", '',new_tweets_hk$tweet_clean)
new_tweets_hk$tweet_clean=tolower(new_tweets_hk$tweet_clean)

# generate new aim variables ----------------------------------------------

media_high<-c("China Xinhua News","People's Daily, China","Guangming Daily","CCTV","China Plus News","CGTN")
media_low<-c("Beijing Review","Caixin Global","China Daily","China News 中国新闻网","China.org.cn","Global Times")

new_tweets_hk$ranking= ifelse(new_tweets_hk$name %in% media_high, "high","low")
new_tweets_hk$ranking<-as.factor(new_tweets_hk$ranking)

# media engagement ratio 
new_tweets_hk$follower[new_tweets_hk$name=="CCTV"]=914080
new_tweets_hk$follower[new_tweets_hk$name=="People's Daily, China"]=7092189
new_tweets_hk$follower[new_tweets_hk$name=="Global Times"]=1650012
new_tweets_hk$follower[new_tweets_hk$name=="China Xinhua News"]=12636846
new_tweets_hk$follower[new_tweets_hk$name=="China Daily"]=4304653
new_tweets_hk$follower[new_tweets_hk$name=="China News 中国新闻网"]=619567
new_tweets_hk$follower[new_tweets_hk$name=="China.org.cn"]=1117624
new_tweets_hk$follower[new_tweets_hk$name=="CGTN"]=14050523
new_tweets_hk$follower[new_tweets_hk$name=="China Plus News"]=770554
new_tweets_hk$follower[new_tweets_hk$name=="Caixin Global"]=62753
new_tweets_hk$follower[new_tweets_hk$name=="Guangming Daily"]=240694
new_tweets_hk$follower[new_tweets_hk$name=="Sixth Tone"]=74762
new_tweets_hk$follower[new_tweets_hk$name=="Beijing Review"]=85722

new_tweets_hk$engagement_ratio=(new_tweets_hk$retweets_count+new_tweets_hk$replies_count+new_tweets_hk$likes_count)/new_tweets_hk$follower

new_tweets_hk$engagement_level[new_tweets_hk$engagement_ratio>=median(new_tweets_hk$engagement_ratio)]=c("High engagement")
new_tweets_hk$engagement_level[new_tweets_hk$engagement_ratio<median(new_tweets_hk$engagement_ratio)]=c("low engagement")
new_tweets_hk$engagement_level<-as.factor(new_tweets_hk$engagement_level)

# remove stopwords HK -----------------------------------------------------

stopword_HK<-c("HongKong","Hong","Kong","HKSAR","HK","hong","kong","hongkong","hksar","hk")
new_tweets_hk$new_tweets<-tm::removeWords(new_tweets_hk$tweet_clean,stopword_HK)

new_tweets_hk$new_date=format(as.Date(new_tweets_hk$date), "%Y/%m/%d")
new_tweets_hk$first_day=format(as.Date("2019-05-31"),"%Y/%m/%d")
new_tweets_hk$number_of_day=as.numeric(as.POSIXct(new_tweets_hk$new_date,format="%Y/%m/%d")-as.POSIXct(new_tweets_hk$first_day, format="%Y/%m/%d"))
names(new_tweets_hk)

new_tweets_hk$engagement_level %>% summary()
