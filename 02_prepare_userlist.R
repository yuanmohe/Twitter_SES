#===============================================================================
# 02_prepare_userlist.R
# Purpose: get a list of users who follow at least 5 brands in the sample.
# Author: Yuanmo He
#===============================================================================
library("DBI")

filesList <- list.files('followers_lists', full.names = TRUE)

#using SQLite as the data are too large to be loaded into memory.

db <- dbConnect(RSQLite::SQLite(), "02_temp/followers_list.sqlite")

for (i in 1:length(filesList)) {
  
  load(filesList[i])
  followers <- as.numeric(followers) # turn to numeric to use less memory/storage
  df <- data.frame(followers)
  dbWriteTable(db, "followers_list", df, append = TRUE)
  print(i)
}

dbGetQuery(db, 'SELECT COUNT(followers)
                FROM followers_list')    # n = 472,653,717

followers_list <- dbGetQuery(db, "SELECT COUNT(followers), followers
                                  FROM followers_list
                                  GROUP BY followers 
                                  ORDER BY COUNT(followers) DESC") # n = 191,790,786


colnames(followers_list) <- c("n_brands", "user_id")

save(followers_list, file = "02_temp/followers_list.rdata") 

# users who follow at least five brands
at_least_5  <- followers_list[followers_list$n_brands >= 5, ] # n = 23,567,268

save(at_least_5, file = "at_least_5.rdata")

userlist <- as.character(at_least_5$user_id)
save(userlist, file = "userlist.rdata")

dbDisconnect(db)
