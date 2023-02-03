#===============================================================================
# 14_divergent_validity_ideology.R
# Purpose: test the association between estimated SES with political ideology,
#         measured by Barberá et al.'s (2015) method (https://doi.org/10.1177/0956797615594620).
# Author: Yuanmo He
# Reference: codes for collecting the accounts the users follow are very similar
#   codes in `01_get_followers.R` and thus draws inspirations from Barberá
#   https://github.com/pablobarbera/twitter_ideology/blob/master/replication/01-get-twitter-data.R
#===============================================================================
library("tweetscores")
library("readr")
library("jsonlite")
library("tidyverse")

load("informed_users.rdata")
load("est_users.rdata")

# download the accounts the users follow to use Barberá's method
accounts <- informed_users
user_friends_folder <- 'user_friends/'
credentials_folder <- "credential_folder"

# removing those that we already did (downloaded to "data/friends_lists/")
accounts_done <- gsub(".rdata", "", list.files(user_friends_folder))
accounts_left <- accounts[accounts %in% gsub(".rdata", "", accounts_done) == FALSE]
accounts_left <- accounts_left[!is.na(accounts_left)]

# initially saved in separate files in one folder
# later decided to save in one json file to optimize file system
# could combine the codes to just save in one json file.

while (length(accounts_left) > 0){
    
    # sample randomly one account to get friends
    new_user <- sample(accounts_left, 1)
    cat(new_user, " -- ", length(accounts_left), " accounts left!\n")   
    
    # download friends (with some exception handling...) 
    error <- tryCatch(friends <- getFriends(user_id = new_user,
                                            oauth = credentials_folder), 
                      error=function(e) e)
    if (inherits(error, 'Error')) {
        cat("Error! On to the next one...")
        next
    }
    
    # save to file and remove from lists of "accounts_left"
    file_name <- paste0(user_friends_folder, new_user, ".rdata")
    save(friends, file = file_name)
    accounts_left <- accounts_left[-which(accounts_left %in% new_user)]
    
}

accounts_done <- gsub(".rdata", "", list.files(user_friends_folder))

for (i in 1:length(accounts_done)){
    
    user_id <- accounts_done[i]
    friends_file <- paste0(user_friends_folder, user_id, ".rdata")
    load(friends_file)
    friends <- list(friends)
    names(friends) <- user_id
    friends_json <- toJSON(friends)
    write(friends_json, "user_friends.json", append = TRUE)
    print(i)
}

# get political orientation
i <- 1
df_lst <- list()
error_lst <- list()
fromfile <- file("user_friends.json", "r")

while (TRUE) {
    
    line = readLines(fromfile, n = 1)
    
    if ( length(line) == 0 ) {
        break
    }
    
    friends <- fromJSON(line)
    
    user_id <- names(friends)
    friends <- friends[[user_id]]
    
    error <- tryCatch(
        df_lst[[i]] <- c(user_id, estimateIdeology2(user_id, friends)),
        error = function(e) e
    )
    
    if (inherits(error, 'Error')){
        error_lst <- append(error_lst, error)
    }
    
    print(i)
    i <- i + 1
    
}

close(fromfile)

length(error_lst) # zero errors, good news

user_ideo <- data.frame(matrix(unlist(df_lst), ncol = 2, byrow = TRUE))
colnames(user_ideo) <- c("id", "ideology")
user_ideo$ideology <- as.numeric(user_ideo$ideology)
save(user_ideo, file = "user_ideo.rdata")

# compare estimated ses and political ideology
mdf <- merge(user_ideo, est_users, by = "id")
cor.test(mdf$ideology, mdf$ses, method = "spearman")

