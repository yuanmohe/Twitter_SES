#===============================================================================
# 03_get_active_users.R
# Purpose: download user data from Twitter API, 
#          then select active users based on the user data.
# Author: Yuanmo He
# Reference: some codes draws inspirations from Pablo Barberá's Github repository
#            "twitter_ideology" https://github.com/pablobarbera/twitter_ideology
#===============================================================================
library("tweetscores")
library("DBI")
library("tidyverse")
source('functions.R')

#-------------------------------------------------------------------------------
# download user information from Twitter API
#-------------------------------------------------------------------------------
load('userlist.rdata')

# split the ids to chunks of 100 ids as each twitter query can do up to 100
n <- length(userlist)
k <- 100
user_chunks <- split(userlist, rep(1:ceiling(n/k), each=k)[1:n])
rm(userlist)

outfolder <- "followers_objects/"
# getting data and save to disk.
for (i in 1:length(user_chunks)){
  
  users <- getUsers(ids = unlist(user_chunks[i]), 
                    oauth = "00_credentials")
  
  for (j in 1:length(users)){
    
    obj <- users[[j]]
    
    if (is.null(obj$id_str) == FALSE){
      
      filename <- paste0(outfolder, obj$id_str, ".rdata")
      save(obj, file = filename)
      
    }
  }
  
  print(i)
  rm(users)    
}

#-------------------------------------------------------------------------------
# Select only active users
#-------------------------------------------------------------------------------
# inactive users
# 1. sent less than 100 tweets
# 2. have less than 25 followers
# 3. not sent one tweet in 2020
# 4. locate outside us

##------------------------------------------------------------------------------
## select based on the first two rules, 100-25.
##------------------------------------------------------------------------------
obj_files <- list.files("followers_objects")
save(obj_files, file = '03_temp/obj_files.rdata')

# Some ids are no longer valid so I need to check how many objects are download 
# to see how many ids are valid
length(obj_files)

# a dataframe of users sufficing condition 1 and 2.
# columns are user ids and date for the latest tweet.
# put in SQL
# create/connect to sql DB
db <- dbConnect(RSQLite::SQLite(), "03_temp/active_id_date.sqlite")

# append sufficing users
for (i in 1:length(obj_files)){
  
  filename <- paste0(outfolder, obj_files[i])
  load(filename)
  
  if (obj$followers_count >= 25 & obj$statuses_count >= 100){
    
    error <- tryCatch(df <- data.frame(matrix(c(obj$id_str, 
                                                obj$status$created_at), 
                                              nrow=1)),
                      error = function(e) e)
    if (inherits(error, 'error')) {
      next
    }
    
    dbWriteTable(db, "active_id_date", df, append = TRUE)
  }
  
  if(i %% 1000 == 0){
    print(i)
  }
}

##------------------------------------------------------------------------------
## select only users whose last tweet is in 2020.
##------------------------------------------------------------------------------
# get the data from SQL
active_id_date <- dbGetQuery(db, 'SELECT * FROM active_id_date')
active_id_date <- unique(active_id_date)
dbDisconnect(db)

# get only 2020
active_id_date$year <- substring(active_id_date$X2, 27, 30)
active_id_2020 <- active_id_date[active_id_date$year == "2020",]
active_2020_list <- active_id_2020$X1
save(active_2020_list, file = "03_temp/active_2020_list.rdata")

##------------------------------------------------------------------------------
## exclude non-us users from the whole project.
## Note: there are probably better ways to do this.
##------------------------------------------------------------------------------

# get 2020 active users' id, location and profile description.
df_lst <- list()
for (i in 1:length(active_2020_list)){
  
  filename <- paste0(outfolder, active_2020_list[i], ".rdata")
  load(filename)
  df_lst[[i]] <- c(obj$id_str, obj$location, obj$description)
  
  if(i %% 1000 == 0){
    print(i)
  }
}

act_loc_des <- data.frame(matrix(unlist(df_lst), ncol = 3, byrow = TRUE))

act_loc_des$X1 <- as.character(act_loc_des$X1)
act_loc_des$X2 <- as.character(act_loc_des$X2)
act_loc_des$X3 <- as.character(act_loc_des$X3)
colnames(act_loc_des) <- c("id", "location", "description")
save(act_loc_des, file = "03_temp/act_loc_des.rdata")

# exclude user whose location is empty from this step.
load("03_temp/act_loc_des.rdata")
id_loc <- act_loc_des[act_loc_des$location != "", ]

# what percent of users indicated their location
nrow(id_loc)/nrow(act_loc_des) #0.740784
save(id_loc, file = "03_temp/id_loc.rdata") 

# some simple pre-selections to reduce API query numbers
load("03_temp/id_loc.rdata")
library("rvest")

## get the us states names and abbreviations
weburl <- "https://en.wikipedia.org/wiki/List_of_U.S._state_abbreviations"
html_content <- read_html(weburl)
tab <- html_table(html_content, fill = TRUE)
states <- cbind(tab[[1]]$X1, tab[[1]]$X6)
rm(tab, html_content)
states <- states[13:nrow(states), ]
states <- as.list(states)
states <- unlist(states)
states <- tolower(states)

## find the user who already indicate USA in their location
## by substring the last strings of their location
## and subset those who are not in USA
id_loc$last_string <- sub('.*,\\s*', '', id_loc$location)
id_loc$last_string <- tolower(id_loc$last_string)
id_loc$last_string <- str_trim(id_loc$last_string)
id_loc$last_string <- gsub("[[:punct:]]", "", id_loc$last_string)
id_loc_sub <- id_loc[id_loc$last_string != "usa" & id_loc$last_string != "us"
                     & id_loc$last_string != "united states", ]

## do the same for the US states names & abbreviations
id_loc_sub$us <- ifelse(id_loc_sub$last_string %in% states, 1, 0)
id_loc_sub2 <- id_loc_sub[id_loc_sub$us != 1, ]
rm(id_loc, id_loc_sub)
id_loc_sub2 <- id_loc_sub2[,-which(colnames(id_loc_sub2) == "description")]

# exclude those are surely in other countries

## get a list of country names
weburl <- "https://en.wikipedia.org/wiki/List_of_countries_and_dependencies_by_population"
html_content <- read_html(weburl)
tab <- html_table(html_content, fill = TRUE)
countries <- tab[[1]]$`Country or dependent territory`
countries <- sub("\\[[a-z]\\]", "", countries)
countries <- tolower(countries)
countries <- gsub("[[:punct:]]", "", countries)
rm(tab, html_content)

## subset those are surely not in us.
id_loc_sub2$us <- ifelse(id_loc_sub2$last_string %in% countries, -1, 0)
to_exclude <- id_loc_sub2[id_loc_sub2$us == -1, ]
id_loc_sub3 <- id_loc_sub2[id_loc_sub2$us == 0, ]
save(to_exclude, file = "03_temp/to_exclude.rdata")
save(id_loc_sub3, file = "03_temp/id_loc_sub3.rdata")
rm(id_loc_sub2, to_exclude)

# use frequency table to save query times.
last_string <- count(id_loc_sub3, id_loc_sub3$last_string)
last_string <- last_string[order(last_string$n, decreasing = TRUE),]
colnames(last_string) <- c("location", "n")
save(last_string, file = "03_temp/last_string.rdata")

# use Geogle geocoding API
load("03_temp/last_string.rdata")
library("ggmap")
register_google("<google API code>")


for (i in 1:nrow(last_string)){
  
  gc <- as.numeric(geocode(last_string$location[i]))
  last_string$full_loc[i] <- revgeocode(gc)
  
  if(i %% 1000 == 0){
    save(last_string, file = "03_temp/last_string.rdata")
    print(i)
  }
}
save(last_string, file = "03_temp/last_string.rdata")

# Didn't do all due to query limit. 
# Used up all Google credit of 300$.
# Google geocoding API is not always the best tool as it is not cheap.
# 5 USD per 1000 queries

# select only those got queried.
done_geo <- last_string[1:80071,]
non_geo <- last_string[80072:nrow(last_string),]
nrow(non_geo)
# The strings that are not queried all only paired with one users,
# so 327,189 users' location are not decided. 
# Compared with over 1 million users who are certainly not in the us 
# and over 3 million users left, 
# these 327,189 should not matter that much.

load("03_temp/to_exclude.rdata")
done_geo$full_loc <- sub('.*,\\s*', '', done_geo$full_loc)

# delete NA and USA
# the left are surely in other countries
done_geo2 <- done_geo[complete.cases(done_geo),]
done_geo2 <- done_geo2[done_geo2$full_loc != "USA", ]

load("03_temp/id_loc_sub3.rdata")

# finally
id_loc_sub3$us <- ifelse(id_loc_sub3$last_string %in% done_geo2$location, -1, 0)
to_exclude <- rbind(to_exclude, id_loc_sub3[id_loc_sub3$us == -1, ])
save(to_exclude, file = "03_temp/to_exclude.rdata")
rm(id_loc_sub3, done_geo, done_geo2, non_geo)

# exclude whose location is surely not US.
load("03_temp/act_loc_des.rdata")
`%!in%` = Negate(`%in%`)
us_loc_des <- subset(act_loc_des, act_loc_des$id %!in% to_exclude$id)
save(us_loc_des, file = "us_loc_des.rdata")

# also save only the list of ids
active_us_users <- us_loc_des$id
save(active_us_users, file = "active_us_users.rdata")

