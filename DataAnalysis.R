library(tidyverse)
library(zoo)
library(stargazer)

#Import file

rawdata = read.csv("D:/Users/Kurt/Documents/Research/Dissertation/rawdata_complete.csv")


#Convert "Date" column to date format

rawdata$date = as.Date(rawdata$date, "%m/%d/%Y")


#Strip down file for relevant information and eliminate weekends

data = rawdata %>% select(date, trump_approval_poll, economy_poll, trump_twitter_corpus_sentiment, trump_twitter_individual_sentiment, trump_twitter_individual_sentiment_no_verified,
              trump_twitter_bot_sentiment, economy_twitter_corpus_sentiment, economy_twitter_individual_sentiment, economy_twitter_individual_sentiment_no_verified,
              economy_twitter_bot_sentiment) %>% filter(!is.na(trump_approval_poll))


#Create Trump poll approval 3-day rolling average change variable

data = data %>% mutate(trump_approval_poll_previousday = lag(trump_approval_poll, order_by = date))

data = data %>% mutate(trump_approval_poll_change = trump_approval_poll - trump_approval_poll_previousday)

data = data %>% mutate(trump_approval_poll_change_3davg = rollmean(trump_approval_poll_change, 3, fill = NA_real_)) %>% select(date, trump_approval_poll, trump_approval_poll_change_3davg, economy_poll, trump_twitter_corpus_sentiment, trump_twitter_individual_sentiment, trump_twitter_individual_sentiment_no_verified,
                                                                                                                           trump_twitter_bot_sentiment, economy_twitter_corpus_sentiment, economy_twitter_individual_sentiment, economy_twitter_individual_sentiment_no_verified,
                                                                                                                           economy_twitter_bot_sentiment)

data = data %>% mutate(trump_approval_poll_change_3davg = lag(trump_approval_poll_change_3davg))


#Create economy poll approval 3-day rolling average change variable

data = data %>% mutate(economy_poll_previousday = lag(economy_poll, order_by = date))

data = data %>% mutate(economy_poll_change = economy_poll - economy_poll_previousday)

data = data %>% mutate(economy_poll_change_3davg = rollmean(economy_poll_change, 3, fill = NA_real_)) %>% select(date, trump_approval_poll, trump_approval_poll_change_3davg, economy_poll, economy_poll_change_3davg, trump_twitter_corpus_sentiment, trump_twitter_individual_sentiment, trump_twitter_individual_sentiment_no_verified,
                                                                                                                         trump_twitter_bot_sentiment, economy_twitter_corpus_sentiment, economy_twitter_individual_sentiment, economy_twitter_individual_sentiment_no_verified,
                                                                                                                         economy_twitter_bot_sentiment)

data = data %>% mutate(economy_poll_change_3davg = lag(economy_poll_change_3davg))


#Create Trump Twitter corpus sentiment 3-day rolling average change variable

data = data %>% mutate(trump_corpus_sentiment_previousday = lag(trump_twitter_corpus_sentiment, order_by = date))

data = data %>% mutate(trump_corpus_sentiment_change = trump_twitter_corpus_sentiment - trump_corpus_sentiment_previousday)

data = data %>% mutate(trump_corpus_sentiment_change_3davg = rollmean(trump_corpus_sentiment_change, 3, fill = NA_real_)) %>% select(date, trump_approval_poll, trump_approval_poll_change_3davg, economy_poll, economy_poll_change_3davg, trump_twitter_corpus_sentiment, trump_corpus_sentiment_change_3davg, trump_twitter_individual_sentiment, trump_twitter_individual_sentiment_no_verified,
                                                                                                                         trump_twitter_bot_sentiment, economy_twitter_corpus_sentiment, economy_twitter_individual_sentiment, economy_twitter_individual_sentiment_no_verified,
                                                                                                                         economy_twitter_bot_sentiment)

data = data %>% mutate(trump_corpus_sentiment_change_3davg = lag(trump_corpus_sentiment_change_3davg))


#Create Trump Twitter individual sentiment 3-day rolling average change variable

data = data %>% mutate(trump_individual_sentiment_previousday = lag(trump_twitter_individual_sentiment, order_by = date))

data = data %>% mutate(trump_individual_sentiment_change = trump_twitter_individual_sentiment - trump_individual_sentiment_previousday)

data = data %>% mutate(trump_individual_sentiment_change_3davg = rollmean(trump_individual_sentiment_change, 3, fill = NA_real_)) %>% select(date, trump_approval_poll, trump_approval_poll_change_3davg, economy_poll, economy_poll_change_3davg, trump_twitter_corpus_sentiment, trump_corpus_sentiment_change_3davg, trump_twitter_individual_sentiment, trump_individual_sentiment_change_3davg, trump_twitter_individual_sentiment_no_verified,
                                                                                                                               trump_twitter_bot_sentiment, economy_twitter_corpus_sentiment, economy_twitter_individual_sentiment, economy_twitter_individual_sentiment_no_verified,
                                                                                                                               economy_twitter_bot_sentiment)

data = data %>% mutate(trump_individual_sentiment_change_3davg = lag(trump_individual_sentiment_change_3davg))


#Create Trump Twitter individual, no verified sentiment 3-day rolling average change variable

data = data %>% mutate(trump_noverified_sentiment_previousday = lag(trump_twitter_individual_sentiment_no_verified, order_by = date))

data = data %>% mutate(trump_noverified_sentiment_change = trump_twitter_individual_sentiment_no_verified - trump_noverified_sentiment_previousday)

data = data %>% mutate(trump_noverified_sentiment_change_3davg = rollmean(trump_noverified_sentiment_change, 3, fill = NA_real_)) %>% select(date, trump_approval_poll, trump_approval_poll_change_3davg, economy_poll, economy_poll_change_3davg, trump_twitter_corpus_sentiment, trump_corpus_sentiment_change_3davg, trump_twitter_individual_sentiment, trump_individual_sentiment_change_3davg, trump_twitter_individual_sentiment_no_verified, trump_noverified_sentiment_change_3davg,
                                                                                                                                         trump_twitter_bot_sentiment, economy_twitter_corpus_sentiment, economy_twitter_individual_sentiment, economy_twitter_individual_sentiment_no_verified,
                                                                                                                                         economy_twitter_bot_sentiment)

data = data %>% mutate(trump_noverified_sentiment_change_3davg = lag(trump_noverified_sentiment_change_3davg))


#Create Trump Twitter bot sentiment 3-day rolling average change variable

data = data %>% mutate(trump_bot_sentiment_previousday = lag(trump_twitter_bot_sentiment, order_by = date))

data = data %>% mutate(trump_bot_sentiment_change = trump_twitter_bot_sentiment - trump_bot_sentiment_previousday)

data = data %>% mutate(trump_bot_sentiment_change_3davg = rollmean(trump_bot_sentiment_change, 3, fill = NA_real_)) %>% select(date, trump_approval_poll, trump_approval_poll_change_3davg, economy_poll, economy_poll_change_3davg, trump_twitter_corpus_sentiment, trump_corpus_sentiment_change_3davg, trump_twitter_individual_sentiment, trump_individual_sentiment_change_3davg, trump_twitter_individual_sentiment_no_verified, trump_noverified_sentiment_change_3davg,
                                                                                                                                       trump_twitter_bot_sentiment, trump_bot_sentiment_change_3davg, economy_twitter_corpus_sentiment, economy_twitter_individual_sentiment, economy_twitter_individual_sentiment_no_verified,
                                                                                                                                       economy_twitter_bot_sentiment)

data = data %>% mutate(trump_bot_sentiment_change_3davg = lag(trump_bot_sentiment_change_3davg))


#Create economy Twitter corpus sentiment 3-day rolling average change variable

data = data %>% mutate(economy_corpus_sentiment_previousday = lag(economy_twitter_corpus_sentiment, order_by = date))

data = data %>% mutate(economy_corpus_sentiment_change = economy_twitter_corpus_sentiment - economy_corpus_sentiment_previousday)

data = data %>% mutate(economy_corpus_sentiment_change_3davg = rollmean(economy_corpus_sentiment_change, 3, fill = NA_real_)) %>% select(date, trump_approval_poll, trump_approval_poll_change_3davg, economy_poll, economy_poll_change_3davg, trump_twitter_corpus_sentiment, trump_corpus_sentiment_change_3davg, trump_twitter_individual_sentiment, trump_individual_sentiment_change_3davg, trump_twitter_individual_sentiment_no_verified, trump_noverified_sentiment_change_3davg,
                                                                                                                                     trump_twitter_bot_sentiment, trump_bot_sentiment_change_3davg, economy_twitter_corpus_sentiment, economy_corpus_sentiment_change_3davg, economy_twitter_individual_sentiment, economy_twitter_individual_sentiment_no_verified,
                                                                                                                                     economy_twitter_bot_sentiment)

data = data %>% mutate(economy_corpus_sentiment_change_3davg = lag(economy_corpus_sentiment_change_3davg))


#Create economy Twitter individual sentiment 3-day rolling average change variable

data = data %>% mutate(economy_individual_sentiment_previousday = lag(economy_twitter_individual_sentiment, order_by = date))

data = data %>% mutate(economy_individual_sentiment_change = economy_twitter_individual_sentiment - economy_individual_sentiment_previousday)

data = data %>% mutate(economy_individual_sentiment_change_3davg = rollmean(economy_individual_sentiment_change, 3, fill = NA_real_)) %>% select(date, trump_approval_poll, trump_approval_poll_change_3davg, economy_poll, economy_poll_change_3davg, trump_twitter_corpus_sentiment, trump_corpus_sentiment_change_3davg, trump_twitter_individual_sentiment, trump_individual_sentiment_change_3davg, trump_twitter_individual_sentiment_no_verified, trump_noverified_sentiment_change_3davg,
                                                                                                                                     trump_twitter_bot_sentiment, trump_bot_sentiment_change_3davg, economy_twitter_corpus_sentiment, economy_corpus_sentiment_change_3davg, economy_twitter_individual_sentiment, economy_individual_sentiment_change_3davg, economy_twitter_individual_sentiment_no_verified,
                                                                                                                                     economy_twitter_bot_sentiment)

data = data %>% mutate(economy_individual_sentiment_change_3davg = lag(economy_individual_sentiment_change_3davg))


#Create economy Twitter individual, no verified sentiment 3-day rolling average change variable

data = data %>% mutate(economy_noverified_sentiment_previousday = lag(economy_twitter_individual_sentiment_no_verified, order_by = date))

data = data %>% mutate(economy_noverified_sentiment_change = economy_twitter_individual_sentiment_no_verified - economy_noverified_sentiment_previousday)

data = data %>% mutate(economy_noverified_sentiment_change_3davg = rollmean(economy_noverified_sentiment_change, 3, fill = NA_real_)) %>% select(date, trump_approval_poll, trump_approval_poll_change_3davg, economy_poll, economy_poll_change_3davg, trump_twitter_corpus_sentiment, trump_corpus_sentiment_change_3davg, trump_twitter_individual_sentiment, trump_individual_sentiment_change_3davg, trump_twitter_individual_sentiment_no_verified, trump_noverified_sentiment_change_3davg,
                                                                                                                                           trump_twitter_bot_sentiment, trump_bot_sentiment_change_3davg, economy_twitter_corpus_sentiment, economy_corpus_sentiment_change_3davg, economy_twitter_individual_sentiment, economy_individual_sentiment_change_3davg, economy_twitter_individual_sentiment_no_verified, economy_noverified_sentiment_change_3davg,
                                                                                                                                           economy_twitter_bot_sentiment)

data = data %>% mutate(economy_noverified_sentiment_change_3davg = lag(economy_noverified_sentiment_change_3davg))



#Create economy Twitter bot sentiment 3-day rolling average change variable

data = data %>% mutate(economy_bot_sentiment_previousday = lag(economy_twitter_bot_sentiment, order_by = date))

data = data %>% mutate(economy_bot_sentiment_change = economy_twitter_bot_sentiment - economy_bot_sentiment_previousday)

data = data %>% mutate(economy_bot_sentiment_change_3davg = rollmean(economy_bot_sentiment_change, 3, fill = NA_real_)) %>% select(date, trump_approval_poll, trump_approval_poll_change_3davg, economy_poll, economy_poll_change_3davg, trump_twitter_corpus_sentiment, trump_corpus_sentiment_change_3davg, trump_twitter_individual_sentiment, trump_individual_sentiment_change_3davg, trump_twitter_individual_sentiment_no_verified, trump_noverified_sentiment_change_3davg,
                                                                                                                                             trump_twitter_bot_sentiment, trump_bot_sentiment_change_3davg, economy_twitter_corpus_sentiment, economy_corpus_sentiment_change_3davg, economy_twitter_individual_sentiment, economy_individual_sentiment_change_3davg, economy_twitter_individual_sentiment_no_verified, economy_noverified_sentiment_change_3davg,
                                                                                                                                             economy_twitter_bot_sentiment, economy_bot_sentiment_change_3davg)

data = data %>% mutate(economy_bot_sentiment_change_3davg = lag(economy_bot_sentiment_change_3davg))


#Create lag variables for Trump poll data

data = data %>% mutate(trump_approval_poll_change_3davg_4dlag = lag(trump_approval_poll_change_3davg, 4), 
                       trump_approval_poll_change_3davg_7dlag = lag(trump_approval_poll_change_3davg, 7),
                       trump_approval_poll_change_3davg_14dlag = lag(trump_approval_poll_change_3davg, 14)) 


#Create lag variables for economy poll data

data = data %>% mutate(economy_poll_change_3davg_4dlag = lag(economy_poll_change_3davg, 4), 
                       economy_poll_change_3davg_7dlag = lag(economy_poll_change_3davg, 7),
                       economy_poll_change_3davg_14dlag = lag(economy_poll_change_3davg, 14)) 


#Create lag variables for Trump Twitter corpus sentiment

data = data %>% mutate(trump_corpus_sentiment_change_3davg_4dlag = lag(trump_corpus_sentiment_change_3davg, 4), 
                        trump_corpus_sentiment_change_3davg_7dlag = lag(trump_corpus_sentiment_change_3davg, 7),
                        trump_corpus_sentiment_change_3davg_14dlag = lag(trump_corpus_sentiment_change_3davg, 14)) 


#Create lag variables for Trump Twitter individual sentiment

data = data %>% mutate(trump_individual_sentiment_change_3davg_4dlag = lag(trump_individual_sentiment_change_3davg, 4), 
                        trump_individual_sentiment_change_3davg_7dlag = lag(trump_individual_sentiment_change_3davg, 7),
                        trump_individual_sentiment_change_3davg_14dlag = lag(trump_individual_sentiment_change_3davg, 14)) 


#Create lag variables for Trump Twitter individual, no verified sentiment

data = data %>% mutate(trump_noverified_sentiment_change_3davg_4dlag = lag(trump_noverified_sentiment_change_3davg, 4), 
                       trump_noverified_sentiment_change_3davg_7dlag = lag(trump_noverified_sentiment_change_3davg, 7),
                       trump_noverified_sentiment_change_3davg_14dlag = lag(trump_noverified_sentiment_change_3davg, 14)) 


#Create lag variables for Trump Twitter bot sentiment

data = data %>% mutate(trump_bot_sentiment_change_3davg_4dlag = lag(trump_bot_sentiment_change_3davg, 4), 
                       trump_bot_sentiment_change_3davg_7dlag = lag(trump_bot_sentiment_change_3davg, 7),
                       trump_bot_sentiment_change_3davg_14dlag = lag(trump_bot_sentiment_change_3davg, 14)) 


#Create lag variables for economy Twitter corpus sentiment

data = data %>% mutate(economy_corpus_sentiment_change_3davg_4dlag = lag(economy_corpus_sentiment_change_3davg, 4), 
                       economy_corpus_sentiment_change_3davg_7dlag = lag(economy_corpus_sentiment_change_3davg, 7),
                       economy_corpus_sentiment_change_3davg_14dlag = lag(economy_corpus_sentiment_change_3davg, 14)) 


#Create lag variables for economy Twitter individual sentiment

data = data %>% mutate(economy_individual_sentiment_change_3davg_4dlag = lag(economy_individual_sentiment_change_3davg, 4), 
                       economy_individual_sentiment_change_3davg_7dlag = lag(economy_individual_sentiment_change_3davg, 7),
                       economy_individual_sentiment_change_3davg_14dlag = lag(economy_individual_sentiment_change_3davg, 14)) 


#Create lag variables for economy Twitter, no verified individual sentiment

data = data %>% mutate(economy_noverified_sentiment_change_3davg_4dlag = lag(economy_noverified_sentiment_change_3davg, 4), 
                       economy_noverified_sentiment_change_3davg_7dlag = lag(economy_noverified_sentiment_change_3davg, 7),
                       economy_noverified_sentiment_change_3davg_14dlag = lag(economy_noverified_sentiment_change_3davg, 14)) 


#Create lag variables for economy Twitter bot sentiment

data = data %>% mutate(economy_bot_sentiment_change_3davg_4dlag = lag(economy_bot_sentiment_change_3davg, 4), 
                       economy_bot_sentiment_change_3davg_7dlag = lag(economy_bot_sentiment_change_3davg, 7),
                       economy_bot_sentiment_change_3davg_14dlag = lag(economy_bot_sentiment_change_3davg, 14))


#Create final dataframe

data = data %>% select(date, trump_approval_poll, trump_approval_poll_change_3davg, trump_approval_poll_change_3davg_4dlag, trump_approval_poll_change_3davg_7dlag, 
                       trump_approval_poll_change_3davg_14dlag, economy_poll, economy_poll_change_3davg, economy_poll_change_3davg_4dlag, 
                       economy_poll_change_3davg_7dlag, economy_poll_change_3davg_14dlag, trump_twitter_corpus_sentiment, 
                       trump_corpus_sentiment_change_3davg, trump_corpus_sentiment_change_3davg_4dlag, trump_corpus_sentiment_change_3davg_7dlag, 
                       trump_corpus_sentiment_change_3davg_14dlag, trump_twitter_individual_sentiment, trump_individual_sentiment_change_3davg, 
                       trump_individual_sentiment_change_3davg_4dlag, trump_individual_sentiment_change_3davg_7dlag, trump_individual_sentiment_change_3davg_14dlag, 
                       trump_twitter_individual_sentiment_no_verified, trump_noverified_sentiment_change_3davg, trump_noverified_sentiment_change_3davg_4dlag, 
                       trump_noverified_sentiment_change_3davg_7dlag, trump_noverified_sentiment_change_3davg_14dlag, trump_twitter_bot_sentiment, 
                       trump_bot_sentiment_change_3davg, trump_bot_sentiment_change_3davg_4dlag, trump_bot_sentiment_change_3davg_7dlag, 
                       trump_bot_sentiment_change_3davg_14dlag, economy_twitter_corpus_sentiment, economy_corpus_sentiment_change_3davg, 
                       economy_corpus_sentiment_change_3davg_4dlag, economy_corpus_sentiment_change_3davg_7dlag, economy_corpus_sentiment_change_3davg_14dlag, 
                       economy_twitter_individual_sentiment, economy_individual_sentiment_change_3davg, economy_individual_sentiment_change_3davg_4dlag, 
                       economy_individual_sentiment_change_3davg_7dlag, economy_individual_sentiment_change_3davg_14dlag, 
                       economy_twitter_individual_sentiment_no_verified, economy_noverified_sentiment_change_3davg, economy_noverified_sentiment_change_3davg_4dlag, 
                       economy_noverified_sentiment_change_3davg_7dlag, economy_noverified_sentiment_change_3davg_14dlag, economy_twitter_bot_sentiment, 
                       economy_bot_sentiment_change_3davg, economy_bot_sentiment_change_3davg_4dlag, economy_bot_sentiment_change_3davg_7dlag, 
                       economy_bot_sentiment_change_3davg_14dlag)


#Run model: Corpus, Trump, 0d lag

lm_trump_corpus_zero = lm(trump_approval_poll_change_3davg ~ trump_corpus_sentiment_change_3davg, data = data)

sink("D:/Users/Kurt/Documents/Research/Dissertation/AnalysisResults/summary_trump_corpus_0d.txt")

summary_trump_corpus_zero = summary(lm_trump_corpus_zero)

print(summary_trump_corpus_zero)

sink()

#Estimate = How much a change in polling we would expect to see if sentiment changed by 1
#Std. Error = Estimate of uncertainty/variation
#Pr = "p value", want it to be at .05. Answers: what percentage of time would we expect to see this sort of result by chance?
#Residual standard error: How much is a typical prediction off by?
#Multiple R-squared: Ranges between 0-1, want it to be closer to 1 for a strong prediction
#Adjusted R-squared: Usually ranges between 0-1, same - want it to be closer to 1
#F-statistic basically just saying is the WHOLE model with all of the variables is better than zero?

#plot(lm_trump_corpus_zero)


#Run model: Corpus, Trump, 4d lag

lm_trump_corpus_4d = lm(trump_approval_poll_change_3davg ~ trump_corpus_sentiment_change_3davg_4dlag, data = data)

sink("D:/Users/Kurt/Documents/Research/Dissertation/AnalysisResults/summary_trump_corpus_4d.txt")

summary_trump_corpus_4d = summary(lm_trump_corpus_4d)

print(summary_trump_corpus_4d)

sink()


#Run model: Corpus, Trump, 7d lag

lm_trump_corpus_7d = lm(trump_approval_poll_change_3davg ~ trump_corpus_sentiment_change_3davg_7dlag, data = data)

sink("D:/Users/Kurt/Documents/Research/Dissertation/AnalysisResults/summary_trump_corpus_7d.txt")

summary_trump_corpus_7d = summary(lm_trump_corpus_7d)

print(summary_trump_corpus_7d)

sink()


#Run model: Corpus, Trump, 14d lag

lm_trump_corpus_14d = lm(trump_approval_poll_change_3davg ~ trump_corpus_sentiment_change_3davg_14dlag, data = data)

sink("D:/Users/Kurt/Documents/Research/Dissertation/AnalysisResults/summary_trump_corpus_14d.txt")

summary_trump_corpus_14d = summary(lm_trump_corpus_14d)

print(summary_trump_corpus_14d)

sink()


#Run model: Individual, Trump, 0d lag

lm_trump_individual_0d = lm(trump_approval_poll_change_3davg ~ trump_individual_sentiment_change_3davg, data = data)

sink("D:/Users/Kurt/Documents/Research/Dissertation/AnalysisResults/summary_trump_individual_0d.txt")

summary_trump_individual_0d = summary(lm_trump_individual_0d)

print(summary_trump_individual_0d)

sink()


#Run model: Individual, Trump, 4d lag

lm_trump_individual_4d = lm(trump_approval_poll_change_3davg ~ trump_individual_sentiment_change_3davg_4dlag, data = data)

sink("D:/Users/Kurt/Documents/Research/Dissertation/AnalysisResults/summary_trump_individual_4d.txt")

summary_trump_individual_4d = summary(lm_trump_individual_4d)

print(summary_trump_individual_4d)

sink()


#Run model: Individual, Trump, 7d lag

lm_trump_individual_7d = lm(trump_approval_poll_change_3davg ~ trump_individual_sentiment_change_3davg_7dlag, data = data)

sink("D:/Users/Kurt/Documents/Research/Dissertation/AnalysisResults/summary_trump_individual_7d.txt")

summary_trump_individual_7d = summary(lm_trump_individual_7d)

print(summary_trump_individual_7d)

sink()


#Run model: Individual, Trump, 14d lag

lm_trump_individual_14d = lm(trump_approval_poll_change_3davg ~ trump_individual_sentiment_change_3davg_14dlag, data = data)

sink("D:/Users/Kurt/Documents/Research/Dissertation/AnalysisResults/summary_trump_individual_14d.txt")

summary_trump_individual_14d = summary(lm_trump_individual_14d)

print(summary_trump_individual_14d)

sink()


#Run model: Individual no verified, Trump, 0d lag

lm_trump_noverified_0d = lm(trump_approval_poll_change_3davg ~ trump_noverified_sentiment_change_3davg, data = data)

sink("D:/Users/Kurt/Documents/Research/Dissertation/AnalysisResults/summary_trump_noverified_0d.txt")

summary_trump_noverified_0d = summary(lm_trump_noverified_0d)

print(summary_trump_noverified_0d)

sink()


#Run model: Individual no verified, Trump, 4d lag

lm_trump_noverified_4d = lm(trump_approval_poll_change_3davg ~ trump_noverified_sentiment_change_3davg_4dlag, data = data)

sink("D:/Users/Kurt/Documents/Research/Dissertation/AnalysisResults/summary_trump_noverified_4d.txt")

summary_trump_noverified_4d = summary(lm_trump_noverified_4d)

print(summary_trump_noverified_4d)

sink()


#Run model: Individual no verified, Trump, 7d lag

lm_trump_noverified_7d = lm(trump_approval_poll_change_3davg ~ trump_noverified_sentiment_change_3davg_7dlag, data = data)

sink("D:/Users/Kurt/Documents/Research/Dissertation/AnalysisResults/summary_trump_noverified_7d.txt")

summary_trump_noverified_7d = summary(lm_trump_noverified_7d)

print(summary_trump_noverified_7d)

sink()


#Run model: Individual no verified, Trump, 14d lag

lm_trump_noverified_14d = lm(trump_approval_poll_change_3davg ~ trump_noverified_sentiment_change_3davg_14dlag, data = data)

sink("D:/Users/Kurt/Documents/Research/Dissertation/AnalysisResults/summary_trump_noverified_14d.txt")

summary_trump_noverified_14d = summary(lm_trump_noverified_14d)

print(summary_trump_noverified_14d)

sink()


#Run model: Corpus, economy, 0d lag

lm_economy_corpus_zero = lm(economy_poll_change_3davg ~ economy_corpus_sentiment_change_3davg, data = data)

sink("D:/Users/Kurt/Documents/Research/Dissertation/AnalysisResults/summary_economy_corpus_0d.txt")

summary_economy_corpus_zero = summary(lm_economy_corpus_zero)

print(summary_economy_corpus_zero)

sink()


#Run model: Corpus, economy, 4d lag

lm_economy_corpus_4d = lm(economy_poll_change_3davg ~ economy_corpus_sentiment_change_3davg_4dlag, data = data)

sink("D:/Users/Kurt/Documents/Research/Dissertation/AnalysisResults/summary_economy_corpus_4d.txt")

summary_economy_corpus_4d = summary(lm_economy_corpus_4d)

print(summary_economy_corpus_4d)

sink()


#Run model: Corpus, economy, 7d lag

lm_economy_corpus_7d = lm(economy_poll_change_3davg ~ economy_corpus_sentiment_change_3davg_7dlag, data = data)

sink("D:/Users/Kurt/Documents/Research/Dissertation/AnalysisResults/summary_economy_corpus_7d.txt")

summary_economy_corpus_7d = summary(lm_economy_corpus_7d)

print(summary_economy_corpus_7d)

sink()


#Run model: Corpus, economy, 14d lag

lm_economy_corpus_14d = lm(economy_poll_change_3davg ~ economy_corpus_sentiment_change_3davg_14dlag, data = data)

sink("D:/Users/Kurt/Documents/Research/Dissertation/AnalysisResults/summary_economy_corpus_14d.txt")

summary_economy_corpus_14d = summary(lm_economy_corpus_14d)

print(summary_economy_corpus_14d)

sink()


#Run model: Individual, economy, 0d lag

lm_economy_individual_0d = lm(economy_poll_change_3davg ~ economy_individual_sentiment_change_3davg, data = data)

sink("D:/Users/Kurt/Documents/Research/Dissertation/AnalysisResults/summary_economy_individual_0d.txt")

summary_economy_individual_0d = summary(lm_economy_individual_0d)

print(summary_economy_individual_0d)

sink()


#Run model: Individual, economy, 4d lag

lm_economy_individual_4d = lm(economy_poll_change_3davg ~ economy_individual_sentiment_change_3davg_4dlag, data = data)

sink("D:/Users/Kurt/Documents/Research/Dissertation/AnalysisResults/summary_economy_individual_4d.txt")

summary_economy_individual_4d = summary(lm_economy_individual_4d)

print(summary_economy_individual_4d)

sink()


#Run model: Individual, economy, 7d lag

lm_economy_individual_7d = lm(economy_poll_change_3davg ~ economy_individual_sentiment_change_3davg_7dlag, data = data)

sink("D:/Users/Kurt/Documents/Research/Dissertation/AnalysisResults/summary_economy_individual_7d.txt")

summary_economy_individual_7d = summary(lm_economy_individual_7d)

print(summary_economy_individual_7d)

sink()


#Run model: Individual, economy, 14d lag

lm_economy_individual_14d = lm(economy_poll_change_3davg ~ economy_individual_sentiment_change_3davg_14dlag, data = data)

sink("D:/Users/Kurt/Documents/Research/Dissertation/AnalysisResults/summary_economy_individual_14d.txt")

summary_economy_individual_14d = summary(lm_economy_individual_14d)

print(summary_economy_individual_14d)

sink()


#Run model: Individual no verified, economy, 0d lag

lm_economy_noverified_0d = lm(economy_poll_change_3davg ~ economy_noverified_sentiment_change_3davg, data = data)

sink("D:/Users/Kurt/Documents/Research/Dissertation/AnalysisResults/summary_economy_noverified_0d.txt")

summary_economy_noverified_0d = summary(lm_economy_noverified_0d)

print(summary_economy_noverified_0d)

sink()


#Run model: Individual no verified, economy, 4d lag

lm_economy_noverified_4d = lm(economy_poll_change_3davg ~ economy_noverified_sentiment_change_3davg_4dlag, data = data)

sink("D:/Users/Kurt/Documents/Research/Dissertation/AnalysisResults/summary_economy_noverified_4d.txt")

summary_economy_noverified_4d = summary(lm_economy_noverified_4d)

print(summary_economy_noverified_4d)

sink()


#Run model: Individual no verified, economy, 7d lag

lm_economy_noverified_7d = lm(economy_poll_change_3davg ~ economy_noverified_sentiment_change_3davg_7dlag, data = data)

sink("D:/Users/Kurt/Documents/Research/Dissertation/AnalysisResults/summary_economy_noverified_7d.txt")

summary_economy_noverified_7d = summary(lm_economy_noverified_7d)

print(summary_economy_noverified_7d)

sink()


#Run model: Individual no verified, economy, 14d lag

lm_economy_noverified_14d = lm(economy_poll_change_3davg ~ economy_noverified_sentiment_change_3davg_14dlag, data = data)

sink("D:/Users/Kurt/Documents/Research/Dissertation/AnalysisResults/summary_economy_noverified_14d.txt")

summary_economy_noverified_14d = summary(lm_economy_noverified_14d)

print(summary_economy_noverified_14d)

sink()


#Create lag variables (-4 through +4 days) for Trump sentiment change

data = data %>% mutate(trump_individual_sentiment_change_3davg_neg4dlag = lead(trump_individual_sentiment_change_3davg, 4), 
                       trump_individual_sentiment_change_3davg_neg3dlag = lead(trump_individual_sentiment_change_3davg, 3),
                       trump_individual_sentiment_change_3davg_neg2dlag = lead(trump_individual_sentiment_change_3davg, 2),
                       trump_individual_sentiment_change_3davg_neg1dlag = lead(trump_individual_sentiment_change_3davg, 1),
                       trump_individual_sentiment_change_3davg_1dlag = lag(trump_individual_sentiment_change_3davg, 1),
                       trump_individual_sentiment_change_3davg_2dlag = lag(trump_individual_sentiment_change_3davg, 2),
                       trump_individual_sentiment_change_3davg_3dlag = lag(trump_individual_sentiment_change_3davg, 3)) %>%
                       select(date, trump_approval_poll, trump_approval_poll_change_3davg, trump_approval_poll_change_3davg_4dlag, trump_approval_poll_change_3davg_7dlag, 
                              trump_approval_poll_change_3davg_14dlag, economy_poll, economy_poll_change_3davg, economy_poll_change_3davg_4dlag, 
                              economy_poll_change_3davg_7dlag, economy_poll_change_3davg_14dlag, trump_twitter_corpus_sentiment, 
                              trump_corpus_sentiment_change_3davg, trump_corpus_sentiment_change_3davg_4dlag, trump_corpus_sentiment_change_3davg_7dlag, 
                              trump_corpus_sentiment_change_3davg_14dlag, trump_twitter_individual_sentiment, trump_individual_sentiment_change_3davg,
                              trump_individual_sentiment_change_3davg_neg4dlag, trump_individual_sentiment_change_3davg_neg3dlag,
                              trump_individual_sentiment_change_3davg_neg2dlag, trump_individual_sentiment_change_3davg_neg1dlag,
                              trump_individual_sentiment_change_3davg_1dlag, trump_individual_sentiment_change_3davg_2dlag, trump_individual_sentiment_change_3davg_3dlag,
                              trump_individual_sentiment_change_3davg_4dlag, trump_individual_sentiment_change_3davg_7dlag, trump_individual_sentiment_change_3davg_14dlag, 
                              trump_twitter_individual_sentiment_no_verified, trump_noverified_sentiment_change_3davg, trump_noverified_sentiment_change_3davg_4dlag, 
                              trump_noverified_sentiment_change_3davg_7dlag, trump_noverified_sentiment_change_3davg_14dlag, trump_twitter_bot_sentiment, 
                              trump_bot_sentiment_change_3davg, trump_bot_sentiment_change_3davg_4dlag, trump_bot_sentiment_change_3davg_7dlag, 
                              trump_bot_sentiment_change_3davg_14dlag, economy_twitter_corpus_sentiment, economy_corpus_sentiment_change_3davg, 
                              economy_corpus_sentiment_change_3davg_4dlag, economy_corpus_sentiment_change_3davg_7dlag, economy_corpus_sentiment_change_3davg_14dlag, 
                              economy_twitter_individual_sentiment, economy_individual_sentiment_change_3davg, economy_individual_sentiment_change_3davg_4dlag, 
                              economy_individual_sentiment_change_3davg_7dlag, economy_individual_sentiment_change_3davg_14dlag, 
                              economy_twitter_individual_sentiment_no_verified, economy_noverified_sentiment_change_3davg, economy_noverified_sentiment_change_3davg_4dlag, 
                              economy_noverified_sentiment_change_3davg_7dlag, economy_noverified_sentiment_change_3davg_14dlag, economy_twitter_bot_sentiment, 
                              economy_bot_sentiment_change_3davg, economy_bot_sentiment_change_3davg_4dlag, economy_bot_sentiment_change_3davg_7dlag, 
                              economy_bot_sentiment_change_3davg_14dlag)

#Create lag variables (-4 through +4 days) for economy sentiment change

data = data %>% mutate(economy_individual_sentiment_change_3davg_neg4dlag = lead(economy_individual_sentiment_change_3davg, 4), 
                       economy_individual_sentiment_change_3davg_neg3dlag = lead(economy_individual_sentiment_change_3davg, 3),
                       economy_individual_sentiment_change_3davg_neg2dlag = lead(economy_individual_sentiment_change_3davg, 2),
                       economy_individual_sentiment_change_3davg_neg1dlag = lead(economy_individual_sentiment_change_3davg, 1),
                       economy_individual_sentiment_change_3davg_1dlag = lag(economy_individual_sentiment_change_3davg, 1),
                       economy_individual_sentiment_change_3davg_2dlag = lag(economy_individual_sentiment_change_3davg, 2),
                       economy_individual_sentiment_change_3davg_3dlag = lag(economy_individual_sentiment_change_3davg, 3)) %>%
  select(date, trump_approval_poll, trump_approval_poll_change_3davg, trump_approval_poll_change_3davg_4dlag, trump_approval_poll_change_3davg_7dlag, 
         trump_approval_poll_change_3davg_14dlag, economy_poll, economy_poll_change_3davg, economy_poll_change_3davg_4dlag, 
         economy_poll_change_3davg_7dlag, economy_poll_change_3davg_14dlag, trump_twitter_corpus_sentiment, 
         trump_corpus_sentiment_change_3davg, trump_corpus_sentiment_change_3davg_4dlag, trump_corpus_sentiment_change_3davg_7dlag, 
         trump_corpus_sentiment_change_3davg_14dlag, trump_twitter_individual_sentiment, trump_individual_sentiment_change_3davg,
         trump_individual_sentiment_change_3davg_neg4dlag, trump_individual_sentiment_change_3davg_neg3dlag,
         trump_individual_sentiment_change_3davg_neg2dlag, trump_individual_sentiment_change_3davg_neg1dlag,
         trump_individual_sentiment_change_3davg_1dlag, trump_individual_sentiment_change_3davg_2dlag, trump_individual_sentiment_change_3davg_3dlag,
         trump_individual_sentiment_change_3davg_4dlag, trump_individual_sentiment_change_3davg_7dlag, trump_individual_sentiment_change_3davg_14dlag, 
         trump_twitter_individual_sentiment_no_verified, trump_noverified_sentiment_change_3davg, trump_noverified_sentiment_change_3davg_4dlag, 
         trump_noverified_sentiment_change_3davg_7dlag, trump_noverified_sentiment_change_3davg_14dlag, trump_twitter_bot_sentiment, 
         trump_bot_sentiment_change_3davg, trump_bot_sentiment_change_3davg_4dlag, trump_bot_sentiment_change_3davg_7dlag, 
         trump_bot_sentiment_change_3davg_14dlag, economy_twitter_corpus_sentiment, economy_corpus_sentiment_change_3davg, 
         economy_corpus_sentiment_change_3davg_4dlag, economy_corpus_sentiment_change_3davg_7dlag, economy_corpus_sentiment_change_3davg_14dlag, 
         economy_twitter_individual_sentiment, economy_individual_sentiment_change_3davg, economy_individual_sentiment_change_3davg_neg4dlag,
         economy_individual_sentiment_change_3davg_neg3dlag, economy_individual_sentiment_change_3davg_neg2dlag, economy_individual_sentiment_change_3davg_neg1dlag,
         economy_individual_sentiment_change_3davg_1dlag, economy_individual_sentiment_change_3davg_2dlag, economy_individual_sentiment_change_3davg_3dlag,
         economy_individual_sentiment_change_3davg_4dlag, economy_individual_sentiment_change_3davg_7dlag, economy_individual_sentiment_change_3davg_14dlag, 
         economy_twitter_individual_sentiment_no_verified, economy_noverified_sentiment_change_3davg, economy_noverified_sentiment_change_3davg_4dlag, 
         economy_noverified_sentiment_change_3davg_7dlag, economy_noverified_sentiment_change_3davg_14dlag, economy_twitter_bot_sentiment, 
         economy_bot_sentiment_change_3davg, economy_bot_sentiment_change_3davg_4dlag, economy_bot_sentiment_change_3davg_7dlag, 
         economy_bot_sentiment_change_3davg_14dlag)


#Create lag variables (-4 through +4 days) for Trump bot sentiment change

data = data %>% mutate(trump_bot_sentiment_change_3davg_neg4dlag = lead(trump_bot_sentiment_change_3davg, 4), 
                       trump_bot_sentiment_change_3davg_neg3dlag = lead(trump_bot_sentiment_change_3davg, 3),
                       trump_bot_sentiment_change_3davg_neg2dlag = lead(trump_bot_sentiment_change_3davg, 2),
                       trump_bot_sentiment_change_3davg_neg1dlag = lead(trump_bot_sentiment_change_3davg, 1),
                       trump_bot_sentiment_change_3davg_1dlag = lag(trump_bot_sentiment_change_3davg, 1),
                       trump_bot_sentiment_change_3davg_2dlag = lag(trump_bot_sentiment_change_3davg, 2),
                       trump_bot_sentiment_change_3davg_3dlag = lag(trump_bot_sentiment_change_3davg, 3)) %>%
  select(date, trump_approval_poll, trump_approval_poll_change_3davg, trump_approval_poll_change_3davg_4dlag, trump_approval_poll_change_3davg_7dlag, 
         trump_approval_poll_change_3davg_14dlag, economy_poll, economy_poll_change_3davg, economy_poll_change_3davg_4dlag, 
         economy_poll_change_3davg_7dlag, economy_poll_change_3davg_14dlag, trump_twitter_corpus_sentiment, 
         trump_corpus_sentiment_change_3davg, trump_corpus_sentiment_change_3davg_4dlag, trump_corpus_sentiment_change_3davg_7dlag, 
         trump_corpus_sentiment_change_3davg_14dlag, trump_twitter_individual_sentiment, trump_individual_sentiment_change_3davg,
         trump_individual_sentiment_change_3davg_neg4dlag, trump_individual_sentiment_change_3davg_neg3dlag,
         trump_individual_sentiment_change_3davg_neg2dlag, trump_individual_sentiment_change_3davg_neg1dlag,
         trump_individual_sentiment_change_3davg_1dlag, trump_individual_sentiment_change_3davg_2dlag, trump_individual_sentiment_change_3davg_3dlag,
         trump_individual_sentiment_change_3davg_4dlag, trump_individual_sentiment_change_3davg_7dlag, trump_individual_sentiment_change_3davg_14dlag, 
         trump_twitter_individual_sentiment_no_verified, trump_noverified_sentiment_change_3davg, trump_noverified_sentiment_change_3davg_4dlag, 
         trump_noverified_sentiment_change_3davg_7dlag, trump_noverified_sentiment_change_3davg_14dlag, trump_twitter_bot_sentiment, 
         trump_bot_sentiment_change_3davg, trump_bot_sentiment_change_3davg_neg4dlag, trump_bot_sentiment_change_3davg_neg3dlag,
         trump_bot_sentiment_change_3davg_neg2dlag, trump_bot_sentiment_change_3davg_neg1dlag, trump_bot_sentiment_change_3davg_1dlag,
         trump_bot_sentiment_change_3davg_2dlag, trump_bot_sentiment_change_3davg_3dlag,
         trump_bot_sentiment_change_3davg_4dlag, trump_bot_sentiment_change_3davg_7dlag, 
         trump_bot_sentiment_change_3davg_14dlag, economy_twitter_corpus_sentiment, economy_corpus_sentiment_change_3davg, 
         economy_corpus_sentiment_change_3davg_4dlag, economy_corpus_sentiment_change_3davg_7dlag, economy_corpus_sentiment_change_3davg_14dlag, 
         economy_twitter_individual_sentiment, economy_individual_sentiment_change_3davg, economy_individual_sentiment_change_3davg_neg4dlag,
         economy_individual_sentiment_change_3davg_neg3dlag, economy_individual_sentiment_change_3davg_neg2dlag, economy_individual_sentiment_change_3davg_neg1dlag,
         economy_individual_sentiment_change_3davg_1dlag, economy_individual_sentiment_change_3davg_2dlag, economy_individual_sentiment_change_3davg_3dlag,
         economy_individual_sentiment_change_3davg_4dlag, economy_individual_sentiment_change_3davg_7dlag, economy_individual_sentiment_change_3davg_14dlag, 
         economy_twitter_individual_sentiment_no_verified, economy_noverified_sentiment_change_3davg, economy_noverified_sentiment_change_3davg_4dlag, 
         economy_noverified_sentiment_change_3davg_7dlag, economy_noverified_sentiment_change_3davg_14dlag, economy_twitter_bot_sentiment, 
         economy_bot_sentiment_change_3davg, economy_bot_sentiment_change_3davg_4dlag, economy_bot_sentiment_change_3davg_7dlag, 
         economy_bot_sentiment_change_3davg_14dlag)

#Create lag variables (-4 through +4 days) for economy bot sentiment change

data = data %>% mutate(economy_bot_sentiment_change_3davg_neg4dlag = lead(economy_bot_sentiment_change_3davg, 4), 
                       economy_bot_sentiment_change_3davg_neg3dlag = lead(economy_bot_sentiment_change_3davg, 3),
                       economy_bot_sentiment_change_3davg_neg2dlag = lead(economy_bot_sentiment_change_3davg, 2),
                       economy_bot_sentiment_change_3davg_neg1dlag = lead(economy_bot_sentiment_change_3davg, 1),
                       economy_bot_sentiment_change_3davg_1dlag = lag(economy_bot_sentiment_change_3davg, 1),
                       economy_bot_sentiment_change_3davg_2dlag = lag(economy_bot_sentiment_change_3davg, 2),
                       economy_bot_sentiment_change_3davg_3dlag = lag(economy_bot_sentiment_change_3davg, 3)) %>%
  select(date, trump_approval_poll, trump_approval_poll_change_3davg, trump_approval_poll_change_3davg_4dlag, trump_approval_poll_change_3davg_7dlag, 
         trump_approval_poll_change_3davg_14dlag, economy_poll, economy_poll_change_3davg, economy_poll_change_3davg_4dlag, 
         economy_poll_change_3davg_7dlag, economy_poll_change_3davg_14dlag, trump_twitter_corpus_sentiment, 
         trump_corpus_sentiment_change_3davg, trump_corpus_sentiment_change_3davg_4dlag, trump_corpus_sentiment_change_3davg_7dlag, 
         trump_corpus_sentiment_change_3davg_14dlag, trump_twitter_individual_sentiment, trump_individual_sentiment_change_3davg,
         trump_individual_sentiment_change_3davg_neg4dlag, trump_individual_sentiment_change_3davg_neg3dlag,
         trump_individual_sentiment_change_3davg_neg2dlag, trump_individual_sentiment_change_3davg_neg1dlag,
         trump_individual_sentiment_change_3davg_1dlag, trump_individual_sentiment_change_3davg_2dlag, trump_individual_sentiment_change_3davg_3dlag,
         trump_individual_sentiment_change_3davg_4dlag, trump_individual_sentiment_change_3davg_7dlag, trump_individual_sentiment_change_3davg_14dlag, 
         trump_twitter_individual_sentiment_no_verified, trump_noverified_sentiment_change_3davg, trump_noverified_sentiment_change_3davg_4dlag, 
         trump_noverified_sentiment_change_3davg_7dlag, trump_noverified_sentiment_change_3davg_14dlag, trump_twitter_bot_sentiment, 
         trump_bot_sentiment_change_3davg, trump_bot_sentiment_change_3davg_neg4dlag, trump_bot_sentiment_change_3davg_neg3dlag,
         trump_bot_sentiment_change_3davg_neg2dlag, trump_bot_sentiment_change_3davg_neg1dlag, trump_bot_sentiment_change_3davg_1dlag,
         trump_bot_sentiment_change_3davg_2dlag, trump_bot_sentiment_change_3davg_3dlag,
         trump_bot_sentiment_change_3davg_4dlag, trump_bot_sentiment_change_3davg_7dlag, 
         trump_bot_sentiment_change_3davg_14dlag, economy_twitter_corpus_sentiment, economy_corpus_sentiment_change_3davg, 
         economy_corpus_sentiment_change_3davg_4dlag, economy_corpus_sentiment_change_3davg_7dlag, economy_corpus_sentiment_change_3davg_14dlag, 
         economy_twitter_individual_sentiment, economy_individual_sentiment_change_3davg, economy_individual_sentiment_change_3davg_neg4dlag,
         economy_individual_sentiment_change_3davg_neg3dlag, economy_individual_sentiment_change_3davg_neg2dlag, economy_individual_sentiment_change_3davg_neg1dlag,
         economy_individual_sentiment_change_3davg_1dlag, economy_individual_sentiment_change_3davg_2dlag, economy_individual_sentiment_change_3davg_3dlag,
         economy_individual_sentiment_change_3davg_4dlag, economy_individual_sentiment_change_3davg_7dlag, economy_individual_sentiment_change_3davg_14dlag, 
         economy_twitter_individual_sentiment_no_verified, economy_noverified_sentiment_change_3davg, economy_noverified_sentiment_change_3davg_4dlag, 
         economy_noverified_sentiment_change_3davg_7dlag, economy_noverified_sentiment_change_3davg_14dlag, economy_twitter_bot_sentiment, 
         economy_bot_sentiment_change_3davg, economy_bot_sentiment_change_3davg_neg4dlag, economy_bot_sentiment_change_3davg_neg3dlag,
         economy_bot_sentiment_change_3davg_neg2dlag, economy_bot_sentiment_change_3davg_neg1dlag, economy_bot_sentiment_change_3davg_1dlag,
         economy_bot_sentiment_change_3davg_2dlag, economy_bot_sentiment_change_3davg_3dlag, economy_bot_sentiment_change_3davg_4dlag, 
         economy_bot_sentiment_change_3davg_7dlag, economy_bot_sentiment_change_3davg_14dlag)


#Run model: Bots, trump, corpus, 0d lag

lm_bots_corpus_trump_0d = lm(trump_corpus_sentiment_change_3davg ~ trump_bot_sentiment_change_3davg, data = data)

sink("D:/Users/Kurt/Documents/Research/Dissertation/AnalysisResults/summary_bots_corpus_trump_0d.txt")

summary_bots_corpus_trump_0d = summary(lm_bots_corpus_trump_0d)

print(summary_bots_corpus_trump_0d)

sink()


#Run model: Bots, trump, corpus, 4d lag

lm_bots_corpus_trump_4d = lm(trump_corpus_sentiment_change_3davg ~ trump_bot_sentiment_change_3davg_4dlag, data = data)

sink("D:/Users/Kurt/Documents/Research/Dissertation/AnalysisResults/summary_bots_corpus_trump_4d.txt")

summary_bots_corpus_trump_4d = summary(lm_bots_corpus_trump_4d)

print(summary_bots_corpus_trump_4d)

sink()


#Run model: Bots, trump, corpus, 3d lag

lm_bots_corpus_trump_3d = lm(trump_corpus_sentiment_change_3davg ~ trump_bot_sentiment_change_3davg_3dlag, data = data)

sink("D:/Users/Kurt/Documents/Research/Dissertation/AnalysisResults/summary_bots_corpus_trump_3d.txt")

summary_bots_corpus_trump_3d = summary(lm_bots_corpus_trump_3d)

print(summary_bots_corpus_trump_3d)

sink()


#Run model: Bots, trump, corpus, 2d lag

lm_bots_corpus_trump_2d = lm(trump_corpus_sentiment_change_3davg ~ trump_bot_sentiment_change_3davg_2dlag, data = data)

sink("D:/Users/Kurt/Documents/Research/Dissertation/AnalysisResults/summary_bots_corpus_trump_2d.txt")

summary_bots_corpus_trump_2d = summary(lm_bots_corpus_trump_2d)

print(summary_bots_corpus_trump_2d)

sink()


#Run model: Bots, trump, corpus, 1d lag

lm_bots_corpus_trump_1d = lm(trump_corpus_sentiment_change_3davg ~ trump_bot_sentiment_change_3davg_1dlag, data = data)

sink("D:/Users/Kurt/Documents/Research/Dissertation/AnalysisResults/summary_bots_corpus_trump_1d.txt")

summary_bots_corpus_trump_1d = summary(lm_bots_corpus_trump_1d)

print(summary_bots_corpus_trump_1d)

sink()


#Run model: Bots, economy, corpus, 0d lag

lm_bots_corpus_economy_0d = lm(economy_corpus_sentiment_change_3davg ~ economy_bot_sentiment_change_3davg, data = data)

sink("D:/Users/Kurt/Documents/Research/Dissertation/AnalysisResults/summary_bots_corpus_economy_0d.txt")

summary_bots_corpus_economy_0d = summary(lm_bots_corpus_economy_0d)

print(summary_bots_corpus_economy_0d)

sink()


#Run model: Bots, economy, corpus, 4d lag

lm_bots_corpus_economy_4d = lm(economy_corpus_sentiment_change_3davg ~ economy_bot_sentiment_change_3davg_4dlag, data = data)

sink("D:/Users/Kurt/Documents/Research/Dissertation/AnalysisResults/summary_bots_corpus_economy_4d.txt")

summary_bots_corpus_economy_4d = summary(lm_bots_corpus_economy_4d)

print(summary_bots_corpus_economy_4d)

sink()


#Run model: Bots, economy, corpus, 3d lag

lm_bots_corpus_economy_3d = lm(economy_corpus_sentiment_change_3davg ~ economy_bot_sentiment_change_3davg_3dlag, data = data)

sink("D:/Users/Kurt/Documents/Research/Dissertation/AnalysisResults/summary_bots_corpus_economy_3d.txt")

summary_bots_corpus_economy_3d = summary(lm_bots_corpus_economy_3d)

print(summary_bots_corpus_economy_3d)

sink()


#Run model: Bots, economy, corpus, 2d lag

lm_bots_corpus_economy_2d = lm(economy_corpus_sentiment_change_3davg ~ economy_bot_sentiment_change_3davg_2dlag, data = data)

sink("D:/Users/Kurt/Documents/Research/Dissertation/AnalysisResults/summary_bots_corpus_economy_2d.txt")

summary_bots_corpus_economy_2d = summary(lm_bots_corpus_economy_2d)

print(summary_bots_corpus_economy_2d)

sink()


#Run model: Bots, economy, corpus, 1d lag

lm_bots_corpus_economy_1d = lm(economy_corpus_sentiment_change_3davg ~ economy_bot_sentiment_change_3davg_1dlag, data = data)

sink("D:/Users/Kurt/Documents/Research/Dissertation/AnalysisResults/summary_bots_corpus_economy_1d.txt")

summary_bots_corpus_economy_1d = summary(lm_bots_corpus_economy_1d)

print(summary_bots_corpus_economy_1d)

sink()


#Run model: Bots, trump, corpus, neg1d lag

lm_bots_corpus_trump_neg1d = lm(trump_corpus_sentiment_change_3davg ~ trump_bot_sentiment_change_3davg_neg1dlag, data = data)

sink("D:/Users/Kurt/Documents/Research/Dissertation/AnalysisResults/summary_bots_corpus_trump_neg1d.txt")

summary_bots_corpus_trump_neg1d = summary(lm_bots_corpus_trump_neg1d)

print(summary_bots_corpus_trump_neg1d)

sink()


#Run model: Bots, trump, corpus, neg2d lag

lm_bots_corpus_trump_neg2d = lm(trump_corpus_sentiment_change_3davg ~ trump_bot_sentiment_change_3davg_neg2dlag, data = data)

sink("D:/Users/Kurt/Documents/Research/Dissertation/AnalysisResults/summary_bots_corpus_trump_neg2d.txt")

summary_bots_corpus_trump_neg2d = summary(lm_bots_corpus_trump_neg2d)

print(summary_bots_corpus_trump_neg2d)

sink()


#Run model: Bots, trump, corpus, neg3d lag

lm_bots_corpus_trump_neg3d = lm(trump_corpus_sentiment_change_3davg ~ trump_bot_sentiment_change_3davg_neg3dlag, data = data)

sink("D:/Users/Kurt/Documents/Research/Dissertation/AnalysisResults/summary_bots_corpus_trump_neg3d.txt")

summary_bots_corpus_trump_neg3d = summary(lm_bots_corpus_trump_neg3d)

print(summary_bots_corpus_trump_neg3d)

sink()


#Run model: Bots, trump, corpus, neg4d lag

lm_bots_corpus_trump_neg4d = lm(trump_corpus_sentiment_change_3davg ~ trump_bot_sentiment_change_3davg_neg4dlag, data = data)

sink("D:/Users/Kurt/Documents/Research/Dissertation/AnalysisResults/summary_bots_corpus_trump_neg4d.txt")

summary_bots_corpus_trump_neg4d = summary(lm_bots_corpus_trump_neg4d)

print(summary_bots_corpus_trump_neg4d)

sink()


#Run model: Bots, economy, corpus, neg1d lag

lm_bots_corpus_economy_neg1d = lm(economy_corpus_sentiment_change_3davg ~ economy_bot_sentiment_change_3davg_neg1dlag, data = data)

sink("D:/Users/Kurt/Documents/Research/Dissertation/AnalysisResults/summary_bots_corpus_economy_neg1d.txt")

summary_bots_corpus_economy_neg1d = summary(lm_bots_corpus_economy_neg1d)

print(summary_bots_corpus_economy_neg1d)

sink()


#Run model: Bots, economy, corpus, neg2d lag

lm_bots_corpus_economy_neg2d = lm(economy_corpus_sentiment_change_3davg ~ economy_bot_sentiment_change_3davg_neg2dlag, data = data)

sink("D:/Users/Kurt/Documents/Research/Dissertation/AnalysisResults/summary_bots_corpus_economy_neg2d.txt")

summary_bots_corpus_economy_neg2d = summary(lm_bots_corpus_economy_neg2d)

print(summary_bots_corpus_economy_neg2d)

sink()


#Run model: Bots, economy, corpus, neg3d lag

lm_bots_corpus_economy_neg3d = lm(economy_corpus_sentiment_change_3davg ~ economy_bot_sentiment_change_3davg_neg3dlag, data = data)

sink("D:/Users/Kurt/Documents/Research/Dissertation/AnalysisResults/summary_bots_corpus_economy_neg3d.txt")

summary_bots_corpus_economy_neg3d = summary(lm_bots_corpus_economy_neg3d)

print(summary_bots_corpus_economy_neg3d)

sink()


#Run model: Bots, economy, corpus, neg4d lag

lm_bots_corpus_economy_neg4d = lm(economy_corpus_sentiment_change_3davg ~ economy_bot_sentiment_change_3davg_neg4dlag, data = data)

sink("D:/Users/Kurt/Documents/Research/Dissertation/AnalysisResults/summary_bots_corpus_economy_neg4d.txt")

summary_bots_corpus_economy_neg4d = summary(lm_bots_corpus_economy_neg4d)

print(summary_bots_corpus_economy_neg4d)

sink()


#Run model: Bots, trump, individual, 0d lag

lm_bots_trump_0d = lm(trump_individual_sentiment_change_3davg ~ trump_bot_sentiment_change_3davg, data = data)

sink("D:/Users/Kurt/Documents/Research/Dissertation/AnalysisResults/summary_bots_trump_0d.txt")

summary_bots_trump_0d = summary(lm_bots_trump_0d)

print(summary_bots_trump_0d)

sink()


#Run model: Bots, trump, individual, 4d lag

lm_bots_trump_4d = lm(trump_individual_sentiment_change_3davg ~ trump_bot_sentiment_change_3davg_4dlag, data = data)

sink("D:/Users/Kurt/Documents/Research/Dissertation/AnalysisResults/summary_bots_trump_4d.txt")

summary_bots_trump_4d = summary(lm_bots_trump_4d)

print(summary_bots_trump_4d)

sink()


#Run model: Bots, trump, individual, 3d lag

lm_bots_trump_3d = lm(trump_individual_sentiment_change_3davg ~ trump_bot_sentiment_change_3davg_3dlag, data = data)

sink("D:/Users/Kurt/Documents/Research/Dissertation/AnalysisResults/summary_bots_trump_3d.txt")

summary_bots_trump_3d = summary(lm_bots_trump_3d)

print(summary_bots_trump_3d)

sink()


#Run model: Bots, trump, individual, 2d lag

lm_bots_trump_2d = lm(trump_individual_sentiment_change_3davg ~ trump_bot_sentiment_change_3davg_2dlag, data = data)

sink("D:/Users/Kurt/Documents/Research/Dissertation/AnalysisResults/summary_bots_trump_2d.txt")

summary_bots_trump_2d = summary(lm_bots_trump_2d)

print(summary_bots_trump_2d)

sink()


#Run model: Bots, trump, individual, 1d lag

lm_bots_trump_1d = lm(trump_individual_sentiment_change_3davg ~ trump_bot_sentiment_change_3davg_1dlag, data = data)

sink("D:/Users/Kurt/Documents/Research/Dissertation/AnalysisResults/summary_bots_trump_1d.txt")

summary_bots_trump_1d = summary(lm_bots_trump_1d)

print(summary_bots_trump_1d)

sink()


#Run model: Bots, economy, individual, 0d lag

lm_bots_economy_0d = lm(economy_individual_sentiment_change_3davg ~ economy_bot_sentiment_change_3davg, data = data)

sink("D:/Users/Kurt/Documents/Research/Dissertation/AnalysisResults/summary_bots_economy_0d.txt")

summary_bots_economy_0d = summary(lm_bots_economy_0d)

print(summary_bots_economy_0d)

sink()


#Run model: Bots, economy, individual, 4d lag

lm_bots_economy_4d = lm(economy_individual_sentiment_change_3davg ~ economy_bot_sentiment_change_3davg_4dlag, data = data)

sink("D:/Users/Kurt/Documents/Research/Dissertation/AnalysisResults/summary_bots_economy_4d.txt")

summary_bots_economy_4d = summary(lm_bots_economy_4d)

print(summary_bots_economy_4d)

sink()


#Run model: Bots, economy, individual, 3d lag

lm_bots_economy_3d = lm(economy_individual_sentiment_change_3davg ~ economy_bot_sentiment_change_3davg_3dlag, data = data)

sink("D:/Users/Kurt/Documents/Research/Dissertation/AnalysisResults/summary_bots_economy_3d.txt")

summary_bots_economy_3d = summary(lm_bots_economy_3d)

print(summary_bots_economy_3d)

sink()


#Run model: Bots, economy, individual, 2d lag

lm_bots_economy_2d = lm(economy_individual_sentiment_change_3davg ~ economy_bot_sentiment_change_3davg_2dlag, data = data)

sink("D:/Users/Kurt/Documents/Research/Dissertation/AnalysisResults/summary_bots_economy_2d.txt")

summary_bots_economy_2d = summary(lm_bots_economy_2d)

print(summary_bots_economy_2d)

sink()


#Run model: Bots, economy, individual, 1d lag

lm_bots_economy_1d = lm(economy_individual_sentiment_change_3davg ~ economy_bot_sentiment_change_3davg_1dlag, data = data)

sink("D:/Users/Kurt/Documents/Research/Dissertation/AnalysisResults/summary_bots_economy_1d.txt")

summary_bots_economy_1d = summary(lm_bots_economy_1d)

print(summary_bots_economy_1d)

sink()


#Run model: Bots, trump, individual, neg1d lag

lm_bots_trump_neg1d = lm(trump_individual_sentiment_change_3davg ~ trump_bot_sentiment_change_3davg_neg1dlag, data = data)

sink("D:/Users/Kurt/Documents/Research/Dissertation/AnalysisResults/summary_bots_trump_neg1d.txt")

summary_bots_trump_neg1d = summary(lm_bots_trump_neg1d)

print(summary_bots_trump_neg1d)

sink()


#Run model: Bots, trump, individual, neg2d lag

lm_bots_trump_neg2d = lm(trump_individual_sentiment_change_3davg ~ trump_bot_sentiment_change_3davg_neg2dlag, data = data)

sink("D:/Users/Kurt/Documents/Research/Dissertation/AnalysisResults/summary_bots_trump_neg2d.txt")

summary_bots_trump_neg2d = summary(lm_bots_trump_neg2d)

print(summary_bots_trump_neg2d)

sink()


#Run model: Bots, trump, individual, neg3d lag

lm_bots_trump_neg3d = lm(trump_individual_sentiment_change_3davg ~ trump_bot_sentiment_change_3davg_neg3dlag, data = data)

sink("D:/Users/Kurt/Documents/Research/Dissertation/AnalysisResults/summary_bots_trump_neg3d.txt")

summary_bots_trump_neg3d = summary(lm_bots_trump_neg3d)

print(summary_bots_trump_neg3d)

sink()


#Run model: Bots, trump, individual, neg4d lag

lm_bots_trump_neg4d = lm(trump_individual_sentiment_change_3davg ~ trump_bot_sentiment_change_3davg_neg4dlag, data = data)

sink("D:/Users/Kurt/Documents/Research/Dissertation/AnalysisResults/summary_bots_trump_neg4d.txt")

summary_bots_trump_neg4d = summary(lm_bots_trump_neg4d)

print(summary_bots_trump_neg4d)

sink()


#Run model: Bots, economy, individual, neg1d lag

lm_bots_economy_neg1d = lm(economy_individual_sentiment_change_3davg ~ economy_bot_sentiment_change_3davg_neg1dlag, data = data)

sink("D:/Users/Kurt/Documents/Research/Dissertation/AnalysisResults/summary_bots_economy_neg1d.txt")

summary_bots_economy_neg1d = summary(lm_bots_economy_neg1d)

print(summary_bots_economy_neg1d)

sink()


#Run model: Bots, economy, individual, neg2d lag

lm_bots_economy_neg2d = lm(economy_individual_sentiment_change_3davg ~ economy_bot_sentiment_change_3davg_neg2dlag, data = data)

sink("D:/Users/Kurt/Documents/Research/Dissertation/AnalysisResults/summary_bots_economy_neg2d.txt")

summary_bots_economy_neg2d = summary(lm_bots_economy_neg2d)

print(summary_bots_economy_neg2d)

sink()


#Run model: Bots, economy, individual, neg3d lag

lm_bots_economy_neg3d = lm(economy_individual_sentiment_change_3davg ~ economy_bot_sentiment_change_3davg_neg3dlag, data = data)

sink("D:/Users/Kurt/Documents/Research/Dissertation/AnalysisResults/summary_bots_economy_neg3d.txt")

summary_bots_economy_neg3d = summary(lm_bots_economy_neg3d)

print(summary_bots_economy_neg3d)

sink()


#Run model: Bots, economy, individual, neg4d lag

lm_bots_economy_neg4d = lm(economy_individual_sentiment_change_3davg ~ economy_bot_sentiment_change_3davg_neg4dlag, data = data)

sink("D:/Users/Kurt/Documents/Research/Dissertation/AnalysisResults/summary_bots_economy_neg4d.txt")

summary_bots_economy_neg4d = summary(lm_bots_economy_neg4d)

print(summary_bots_economy_neg4d)

sink()
































#Run model: Bots, trump, noverified, 0d lag

lm_bots_noverified_trump_0d = lm(trump_noverified_sentiment_change_3davg ~ trump_bot_sentiment_change_3davg, data = data)

sink("D:/Users/Kurt/Documents/Research/Dissertation/AnalysisResults/summary_bots_noverified_trump_0d.txt")

summary_bots_noverified_trump_0d = summary(lm_bots_noverified_trump_0d)

print(summary_bots_noverified_trump_0d)

sink()


#Run model: Bots, trump, noverified, 4d lag

lm_bots_noverified_trump_4d = lm(trump_noverified_sentiment_change_3davg ~ trump_bot_sentiment_change_3davg_4dlag, data = data)

sink("D:/Users/Kurt/Documents/Research/Dissertation/AnalysisResults/summary_bots_noverified_trump_4d.txt")

summary_bots_noverified_trump_4d = summary(lm_bots_noverified_trump_4d)

print(summary_bots_noverified_trump_4d)

sink()


#Run model: Bots, trump, noverified, 3d lag

lm_bots_noverified_trump_3d = lm(trump_noverified_sentiment_change_3davg ~ trump_bot_sentiment_change_3davg_3dlag, data = data)

sink("D:/Users/Kurt/Documents/Research/Dissertation/AnalysisResults/summary_bots_noverified_trump_3d.txt")

summary_bots_noverified_trump_3d = summary(lm_bots_noverified_trump_3d)

print(summary_bots_noverified_trump_3d)

sink()


#Run model: Bots, trump, noverified, 2d lag

lm_bots_noverified_trump_2d = lm(trump_noverified_sentiment_change_3davg ~ trump_bot_sentiment_change_3davg_2dlag, data = data)

sink("D:/Users/Kurt/Documents/Research/Dissertation/AnalysisResults/summary_bots_noverified_trump_2d.txt")

summary_bots_noverified_trump_2d = summary(lm_bots_noverified_trump_2d)

print(summary_bots_noverified_trump_2d)

sink()


#Run model: Bots, trump, noverified, 1d lag

lm_bots_noverified_trump_1d = lm(trump_noverified_sentiment_change_3davg ~ trump_bot_sentiment_change_3davg_1dlag, data = data)

sink("D:/Users/Kurt/Documents/Research/Dissertation/AnalysisResults/summary_bots_noverified_trump_1d.txt")

summary_bots_noverified_trump_1d = summary(lm_bots_noverified_trump_1d)

print(summary_bots_noverified_trump_1d)

sink()


#Run model: Bots, economy, noverified, 0d lag

lm_bots_noverified_economy_0d = lm(economy_noverified_sentiment_change_3davg ~ economy_bot_sentiment_change_3davg, data = data)

sink("D:/Users/Kurt/Documents/Research/Dissertation/AnalysisResults/summary_bots_noverified_economy_0d.txt")

summary_bots_noverified_economy_0d = summary(lm_bots_noverified_economy_0d)

print(summary_bots_noverified_economy_0d)

sink()


#Run model: Bots, economy, noverified, 4d lag

lm_bots_noverified_economy_4d = lm(economy_noverified_sentiment_change_3davg ~ economy_bot_sentiment_change_3davg_4dlag, data = data)

sink("D:/Users/Kurt/Documents/Research/Dissertation/AnalysisResults/summary_bots_noverified_economy_4d.txt")

summary_bots_noverified_economy_4d = summary(lm_bots_noverified_economy_4d)

print(summary_bots_noverified_economy_4d)

sink()


#Run model: Bots, economy, noverified, 3d lag

lm_bots_noverified_economy_3d = lm(economy_noverified_sentiment_change_3davg ~ economy_bot_sentiment_change_3davg_3dlag, data = data)

sink("D:/Users/Kurt/Documents/Research/Dissertation/AnalysisResults/summary_bots_noverified_economy_3d.txt")

summary_bots_noverified_economy_3d = summary(lm_bots_noverified_economy_3d)

print(summary_bots_noverified_economy_3d)

sink()


#Run model: Bots, economy, noverified, 2d lag

lm_bots_noverified_economy_2d = lm(economy_noverified_sentiment_change_3davg ~ economy_bot_sentiment_change_3davg_2dlag, data = data)

sink("D:/Users/Kurt/Documents/Research/Dissertation/AnalysisResults/summary_bots_noverified_economy_2d.txt")

summary_bots_noverified_economy_2d = summary(lm_bots_noverified_economy_2d)

print(summary_bots_noverified_economy_2d)

sink()


#Run model: Bots, economy, noverified, 1d lag

lm_bots_noverified_economy_1d = lm(economy_noverified_sentiment_change_3davg ~ economy_bot_sentiment_change_3davg_1dlag, data = data)

sink("D:/Users/Kurt/Documents/Research/Dissertation/AnalysisResults/summary_bots_noverified_economy_1d.txt")

summary_bots_noverified_economy_1d = summary(lm_bots_noverified_economy_1d)

print(summary_bots_noverified_economy_1d)

sink()


#Run model: Bots, trump, noverified, neg1d lag

lm_bots_noverified_trump_neg1d = lm(trump_noverified_sentiment_change_3davg ~ trump_bot_sentiment_change_3davg_neg1dlag, data = data)

sink("D:/Users/Kurt/Documents/Research/Dissertation/AnalysisResults/summary_bots_noverified_trump_neg1d.txt")

summary_bots_noverified_trump_neg1d = summary(lm_bots_noverified_trump_neg1d)

print(summary_bots_noverified_trump_neg1d)

sink()


#Run model: Bots, trump, noverified, neg2d lag

lm_bots_noverified_trump_neg2d = lm(trump_noverified_sentiment_change_3davg ~ trump_bot_sentiment_change_3davg_neg2dlag, data = data)

sink("D:/Users/Kurt/Documents/Research/Dissertation/AnalysisResults/summary_bots_noverified_trump_neg2d.txt")

summary_bots_noverified_trump_neg2d = summary(lm_bots_noverified_trump_neg2d)

print(summary_bots_noverified_trump_neg2d)

sink()


#Run model: Bots, trump, noverified, neg3d lag

lm_bots_noverified_trump_neg3d = lm(trump_noverified_sentiment_change_3davg ~ trump_bot_sentiment_change_3davg_neg3dlag, data = data)

sink("D:/Users/Kurt/Documents/Research/Dissertation/AnalysisResults/summary_bots_noverified_trump_neg3d.txt")

summary_bots_noverified_trump_neg3d = summary(lm_bots_noverified_trump_neg3d)

print(summary_bots_noverified_trump_neg3d)

sink()


#Run model: Bots, trump, noverified, neg4d lag

lm_bots_noverified_trump_neg4d = lm(trump_noverified_sentiment_change_3davg ~ trump_bot_sentiment_change_3davg_neg4dlag, data = data)

sink("D:/Users/Kurt/Documents/Research/Dissertation/AnalysisResults/summary_bots_noverified_trump_neg4d.txt")

summary_bots_noverified_trump_neg4d = summary(lm_bots_noverified_trump_neg4d)

print(summary_bots_noverified_trump_neg4d)

sink()


#Run model: Bots, economy, noverified, neg1d lag

lm_bots_noverified_economy_neg1d = lm(economy_noverified_sentiment_change_3davg ~ economy_bot_sentiment_change_3davg_neg1dlag, data = data)

sink("D:/Users/Kurt/Documents/Research/Dissertation/AnalysisResults/summary_bots_noverified_economy_neg1d.txt")

summary_bots_noverified_economy_neg1d = summary(lm_bots_noverified_economy_neg1d)

print(summary_bots_noverified_economy_neg1d)

sink()


#Run model: Bots, economy, noverified, neg2d lag

lm_bots_noverified_economy_neg2d = lm(economy_noverified_sentiment_change_3davg ~ economy_bot_sentiment_change_3davg_neg2dlag, data = data)

sink("D:/Users/Kurt/Documents/Research/Dissertation/AnalysisResults/summary_bots_noverified_economy_neg2d.txt")

summary_bots_noverified_economy_neg2d = summary(lm_bots_noverified_economy_neg2d)

print(summary_bots_noverified_economy_neg2d)

sink()


#Run model: Bots, economy, noverified, neg3d lag

lm_bots_noverified_economy_neg3d = lm(economy_noverified_sentiment_change_3davg ~ economy_bot_sentiment_change_3davg_neg3dlag, data = data)

sink("D:/Users/Kurt/Documents/Research/Dissertation/AnalysisResults/summary_bots_noverified_economy_neg3d.txt")

summary_bots_noverified_economy_neg3d = summary(lm_bots_noverified_economy_neg3d)

print(summary_bots_noverified_economy_neg3d)

sink()


#Run model: Bots, economy, noverified, neg4d lag

lm_bots_noverified_economy_neg4d = lm(economy_noverified_sentiment_change_3davg ~ economy_bot_sentiment_change_3davg_neg4dlag, data = data)

sink("D:/Users/Kurt/Documents/Research/Dissertation/AnalysisResults/summary_bots_noverified_economy_neg4d.txt")

summary_bots_noverified_economy_neg4d = summary(lm_bots_noverified_economy_neg4d)

print(summary_bots_noverified_economy_neg4d)

sink()