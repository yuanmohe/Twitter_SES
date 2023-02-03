#===============================================================================
# 01_get_followers.R
# Purpose: get the followers of the select brands from Twitter API.
# Author: Yuanmo He
# Reference: a large proportion of the codes in this file is copied with little 
#   alterations from Pablo Barberá's Github repository "twitter_ideology": 
#   https://github.com/pablobarbera/twitter_ideology/blob/master/replication/01-get-twitter-data.R
#===============================================================================

library("readr")
library("tweetscores")

# set the outfolder for storing the list of followers for each brands
outfolder <- 'followers_lists/'

# get the list of brands' twitter screen name.
brands_on_twitter <- read_csv("brands_on_twitter.csv")
accounts <- brands_on_twitter$twitter_account_name

#-------------codes from Barberá------------------------------------------------

credentials_folder <- "credential_folder"
# "credential_folder" is the folder where Twitter API credentials are stored.
# Follow 'Authentication' at https://github.com/pablobarbera/twitter_ideology 
# to get the credentials.

# removing those that were already downloaded to "followers_lists/")
accounts.done <- gsub(".rdata", "", list.files(outfolder))
accounts.left <- accounts[accounts %in% gsub(".rdata", "", accounts.done) == FALSE]
accounts.left <- accounts.left[!is.na(accounts.left)]

while (length(accounts.left) > 0){
  
  # sample randomly one account to get followers
  new.user <- sample(accounts.left, 1)
  cat(new.user, " -- ", length(accounts.left), " accounts left!\n")   
  
  # download followers (with some exception handling...) 
  error <- tryCatch(followers <- 
                      getFollowers(screen_name = new.user,
                                   oauth = credentials_folder), 
                    error=function(e) e)
  if (inherits(error, 'error')) {
    cat("Error! On to the next one...")
    next
  }
  
  # save to file and remove from lists of "accounts.left"
  file.name <- paste0(outfolder, new.user, ".rdata")
  save(followers, file = file.name)
  accounts.left <- accounts.left[-which(accounts.left %in% new.user)]
  
}

#-------------------------------------------------------------------------------

# Sometimes it is hard to download the followers of the accounts that 
# have a large amount of followers at one go and handle the exceptions,
# so we need to do some manual work,
# input the cursor when it stopped last time.

# using WSJ as an example
getFollowers(screen_name = "WSJ",
                          oauth = "00_credentials",
                          file = "WSJ.csv",
                          cursor = -1)

followers <- read_csv("WSJ.csv")
followers <- unlist(followers)
followers <- as.character(followers)
followers <- unique(followers)
save(followers, file = "followers_lists/WSJ.rdata")
