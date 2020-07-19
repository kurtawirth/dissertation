library(devtools)
library(tidyverse)
library(jsonlite)


#Load botscan

load_all()

bom <- setup_botscan("d8YliMWTOemsh2TWYoqLimGst2vEp1V5fCfjsn07l0dPwjNlgA", "UvXe2RX7dij7K0RLZ3N8WF1CJ", "UDhH5DBgU7KVsWSnt3fKhHJJnkGErtPZwI3bXqb6ymSTEGknjp", "41391869-MDDrq24ZHXBluKLZTnF2FcCXzqIEnfuWtXaJEYFjr", "JNKXbNqB4qBIPemUjkhPgZiWkn1PtqFQF9SZGAXJQhyWE")


#Change file name as needed

file = "D:/Users/Kurt/Documents/Research/Dissertation/Data/tweets/02282020/economy02282020.json"



#Import file as a dataframe, limit to first 2000 tweets

current_day = fromJSON(file) %>% as.data.frame()

current_day = current_day[1:2000,]


#Isolate handles to analyze

users_unique = as.character(unique(current_day$tweets.screen_name))


#Create blank dataframe

df_userbots <- data.frame()


#Run botscan

for(user_idx in 1:length(users_unique)){
  
    if(user_idx %% 100 == 0){
      cat("Checking user account ", users_unique[user_idx], 
          ", number ", user_idx, "\n", sep = "")
    
    }
  
  tryCatch({
    
    tmp_userlist <- bom$check_account(users_unique[user_idx])
    
    tmp_user_df <- as.data.frame(tmp_userlist)
    
    tmp_user_df <- tmp_user_df %>% 
      dplyr::mutate_if(is.factor, as.character)
    
    df_userbots <- dplyr::bind_rows(df_userbots, tmp_user_df)
    
  }, error = function(e) print(e))
  
}







#Isolate likely bots

bots <- dplyr::filter(df_userbots, cap.universal > 0.43)


#Calculate how many handles appear in tweets

n <- length(current_day$tweets.screen_name)


#Calculate percentage of conversation authored by bots

prop_convo_level_bots <- (sum(current_day$tweets.screen_name %in% bots$user.screen_name) / n)

prop_convo_level_bots


#Calculate what percentage of users are bots

nbots <- length(bots$user.screen_name)

n <- length(unique(current_day$tweets.screen_name))

prop_user_level_bots <- (nbots / n)

prop_user_level_bots


#Rename object to be more descriptive, change as needed

diss_botscan_results_economy_022820 = df_userbots


#Save object as .RData file, change as needed

save(diss_botscan_results_economy_022820, file = "/botscan_data.RData")

