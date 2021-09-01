library(dplyr)
options(scipen = 200)

nrow(new_tweets_hk) # 12728 total tweets

table(new_tweets_hk$ranking)
# high ranking  low ranking 
#  4674            8054 

summary(new_tweets_hk$engagement_ratio) # Mean: 0.000034027 Median: 0.000015017
table(new_tweets_hk$engagement_level)

# High engagement  low engagement 
#    6366            6362 