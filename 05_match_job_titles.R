#===============================================================================
# 05_match_job_titles.R
# Purpose: Use text match to find titles from users' description. 
#          Select titles that match UK's SOC and return at least 50 users.
#          This step involves some manual search and assignment of job titles.
# Author: Yuanmo He
#===============================================================================
library("quanteda")
library("stringr")
library("readxl")

load("us_loc_des.rdata")

user_des <- us_loc_des[us_loc_des$description != "",]
rm(us_loc_des)

# preprocessing text to make matching easier
user_des$text <- tolower(user_des$description)
user_des <- user_des[, c("id", "text")]
user_des$text <- gsub("[^\x01-\x7F]", "", user_des$text) # remove emojis
user_des$text <- gsub('http\\S+\\s*', '', user_des$text) # remove urls

save(user_des, file = "05_temp/user_des.rdata")

# read the soc2020 table and select the useful columns
soc2020 <- read_excel("soc2020.xlsx", sheet = "SOC2020 coding index")
soc_ind <- soc2020[, 7:8]
colnames(soc_ind) <- c("class", "title")

# rearrange the words of the from the title column
soc_ind$p1 <- gsub(",", "", tolower(soc_ind$title))
soc_ind$main_title <- word(soc_ind$p1, 1)
soc_ind$two_word <- paste(word(soc_ind$p1, 2), word(soc_ind$p1, 1))

save(soc_ind, file = "05_temp/soc_ind.rdata")

# match the main titles with the profile descriptions to find the frequency 
# of the titles mentioned in our dataset
# if the frequency is too low, the title is not useful for our analysis
uni_title <- unique(soc_ind$main_title)

title_lst <- list()
for (i in 1:length(uni_title)){
  
  title_lst[[i]] <- grep(uni_title[i], user_des$text)
  print(i)
}

save(title_lst, file = "05_temp/title_lst.rdata")

nmatch <- sapply(title_lst, length)

# build the frequency table
title_rank <- data.frame(title = uni_title, n = nmatch)
title_rank <- title_rank[order(title_rank$n, decreasing = TRUE), ]
# delete non-matching titles
title_rank <- title_rank[title_rank$n != 0,]
# the first 20 are no useful
title_rank <- title_rank[21:nrow(title_rank), ]
save(title_rank, file = "05_temp/title_rank.rdata")

# only use the single word job titles.
one_word <- soc_ind[is.na(word(soc_ind$p1, 2)),]
one_title <- unique(one_word$main_title)
one_rank <- title_rank[title_rank$title %in% one_title, ]

# only use the two-word job titles
two_word <- soc_ind[!is.na(word(soc_ind$p1, 2)) & is.na(word(soc_ind$p1, 3)),]

# only use the main title's frequency larger than 1000 ones
title_1000 <- title_rank$title[title_rank$n >= 1000]
two_word <- two_word[two_word$main_title %in% title_1000,]
two_title <- unique(two_word$two_word)

# frequencies for the two-word titles
title_lst2 <- list()
for (i in 1:length(two_title)){
  
  title_lst2[[i]] <- grep(two_title[i], user_des$text)
  print(i)
}

save(title_lst2, file = "05_temp/title_lst2.rdata")

nmatch2 <- sapply(title_lst2, length)

two_rank <- data.frame(title = two_title, n = nmatch2)
two_rank <- two_rank[two_rank$n > 0,]
save(two_rank, file = "05_temp/two_rank.rdata")


#-------------------------------------------------------------------------------
# from one_rank and two_rank, manually select job titles
# check the instances in our sample to decide whether to include the title
# save the useful titles
#-------------------------------------------------------------------------------
user_des$title <- NA

# search certain title
job <- "managing director" # manually change the text to check and save
tg <- paste0("\\b", job, "\\b")
tst <- grep(tg, user_des$text)  

# check the instances
# user_des[tst, 1:2]

# save the title in the data
# also populate the validation_titles.xlsx file manually.
user_des[tst,]$title <- job
nrow(user_des[which(user_des$title == job),])

user_des_title <- user_des
save(user_des_title, file = "05_temp/user_des_title.rdata")

titled_user <- user_des_title[is.na(user_des_title$title) == FALSE,]
save(titled_user, file = "titled_user.rdata")

titled_id <- titled_user[c("id", "title")]
save(titled_id, file = "titled_id.rdata")
