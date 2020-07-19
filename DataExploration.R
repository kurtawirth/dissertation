#Load packages

library(RColorBrewer)

library(wordcloud2)

library(tidyverse)

library(jsonlite)

library(lubridate)

library(wordcloud)

library(ggwordcloud)

library(tm)

#Combine all Trump data

setwd("D:/Users/Kurt/Documents/Research/Dissertation/Data/tweets")

file_list <- list.files(pattern = "trump", recursive = TRUE)

for (file in file_list){
  
  # if the merged dataset doesn't exist, create it
  if (!exists("dataset")){
    dataset = fromJSON(file) %>% as.data.frame()
  }
  
  # if the merged dataset does exist, append to it
  if (exists("dataset")){
    temp_dataset = fromJSON(file) %>% as.data.frame()
    dataset = rbind(dataset, temp_dataset)
    rm(temp_dataset)
  }
  
}


#Create corpus

tweets_and_dates = dataset %>% select(tweets.timestamp, tweets.text) %>% mutate(tweets.timestamp = as.Date(tweets.timestamp))

tweet_corpus = tweets_and_dates %>% select(tweets.text)


#Create wordcloud

wordcount = data.frame(table(unlist(strsplit(tolower(tweet_corpus$tweets.text), " ")))) %>% mutate(Var1 = as.character(Var1))

wordcount = wordcount %>% mutate(Var1 = removePunctuation(Var1, preserve_intra_word_contractions = FALSE, preserve_intra_word_dashes = FALSE))

wordcount = data.frame(table(unlist(strsplit(tolower(wordcount$Var1), " ")))) %>% mutate(Var1 = as.character(Var1))

wordcount_test = wordcount[c(1:10)]

ggplot(wordcount[1:200,], aes(label = Var1, size = Freq)) +
  geom_text_wordcloud() +
  scale_size_area(max_size = 20) +
  theme_minimal()

#Explore bot tweets

load("D:/Users/Kurt/Documents/Research/Dissertation/Data/botscan/02282020/trump_botscan.RData")

sorted = diss_botscan_results_trump_022820[order(diss_botscan_results_trump_022820$cap.universal, decreasing = TRUE),] %>% select(cap.universal, user.screen_name)

sorted = sorted[c(1:50),]

