library(tidyverse)
library(jsonlite)
library(sentimentr)


#Change name of file as needed

file = "D:/Users/Kurt/Documents/Research/Dissertation/Data/tweets/01152020/economy01152020.json"


#Import file as a dataframe, limit to first 2000 tweets

current_day = fromJSON(file) %>% as.data.frame()

current_day = current_day[1:2000,]


#Select only the handles and the text of each tweet

handles_text = current_day %>% select(5, 6)


#Identify separate sentences in each tweet

tweet_text_sentences = get_sentences(handles_text$tweets.text)


#Score the sentiment of each sentence and then average them together to form a single sentiment score for each tweet

current_day_sentiment_scores = sentiment_by(tweet_text_sentences) %>%
  mutate(tweets.screen_name = handles_text$tweets.screen_name) %>% 
  select(-element_id)


#Reorder columns for readability

current_day_sentiment_scores = current_day_sentiment_scores[,c(4,3,2,1)]
  

#Calculate sentiment for the day's corpus

current_day_corpus_sentiment = mean(current_day_sentiment_scores$ave_sentiment)

current_day_corpus_sentiment


#Remove bots from individual analysis

current_day_bots_removed = subset(current_day, !(current_day$tweets.screen_name %in% bots$user.screen_name))


#Select only the handles and the text of each tweet

handles_text = current_day_bots_removed %>% select(5, 6)


#Identify separate sentences in each tweet

tweet_text_sentences = get_sentences(handles_text$tweets.text)


#Score the sentiment of each sentence and then average them together to form a single sentiment score for each tweet

current_day_sentiment_scores = sentiment_by(tweet_text_sentences) %>%
  mutate(tweets.screen_name = handles_text$tweets.screen_name) %>% 
  select(-element_id)


#Reorder columns for readability

current_day_sentiment_scores = current_day_sentiment_scores[,c(4,3,2,1)]


#Calculate sentiment for the day's non-bot individuals
  
sentiment_by_individual = current_day_sentiment_scores %>% group_by(tweets.screen_name) %>%
  summarise(individual_sentiment = mean(ave_sentiment))

current_day_individual_sentiment = mean(sentiment_by_individual$individual_sentiment)

current_day_individual_sentiment


#Remove verified users from individual analysis


current_day_bots_removed_no_verifieds = subset(current_day_bots_removed, tweets.verified == FALSE)


#Select only the handles and the text of each tweet

handles_text = current_day_bots_removed_no_verifieds %>% select(5, 6)


#Identify separate sentences in each tweet

tweet_text_sentences = get_sentences(handles_text$tweets.text)


#Score the sentiment of each sentence and then average them together to form a single sentiment score for each tweet

current_day_sentiment_scores = sentiment_by(tweet_text_sentences) %>%
  mutate(tweets.screen_name = handles_text$tweets.screen_name) %>% 
  select(-element_id)


#Reorder columns for readability

current_day_sentiment_scores = current_day_sentiment_scores[,c(4,3,2,1)]


#Calculate sentiment for the day's non-bot, not verified individuals

sentiment_by_individual = current_day_sentiment_scores %>% group_by(tweets.screen_name) %>%
  summarise(individual_sentiment = mean(ave_sentiment))

current_day_individual_sentiment_no_verifieds = mean(sentiment_by_individual$individual_sentiment)

current_day_individual_sentiment_no_verifieds


#Select only bots from individual analysis

current_day_bots = subset(current_day, (current_day$tweets.screen_name %in% bots$user.screen_name))


#Select only the handles and the text of each tweet

handles_text = current_day_bots %>% select(5, 6)


#Identify separate sentences in each tweet

tweet_text_sentences = get_sentences(handles_text$tweets.text)


#Score the sentiment of each sentence and then average them together to form a single sentiment score for each tweet

current_day_sentiment_scores = sentiment_by(tweet_text_sentences) %>%
  mutate(tweets.screen_name = handles_text$tweets.screen_name) %>% 
  select(-element_id)


#Reorder columns for readability

current_day_sentiment_scores = current_day_sentiment_scores[,c(4,3,2,1)]


#Calculate sentiment for the day's non-bot individuals

sentiment_by_individual = current_day_sentiment_scores %>% group_by(tweets.screen_name) %>%
  summarise(individual_sentiment = mean(ave_sentiment))

current_day_bot_sentiment = mean(sentiment_by_individual$individual_sentiment)

current_day_bot_sentiment
