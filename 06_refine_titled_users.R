#===============================================================================
# 06_refine_titled_users.R
# Purpose: further filtering to eliminate the wrongly matched job titles.
# Author: Yuanmo He
#===============================================================================
library("tidyverse")
library("readxl")
load("titled_user.rdata")

# create a new variable to avoid conflicts
titled_user_refined <- titled_user
rm(titled_user)

# use variable `status` to indicate the status of the label
titled_user_refined$status <- "unsure"
# - "wrong" means the labeling should be wrong.
# - "confident" means the labeling should be right.
# - "unsure" means the labelling need to be checked.

# testing individual titles and get some insights
# here is just one example
sample(titled_user_refined$text[titled_user_refined$status == "unsure" & 
                                  titled_user_refined$title == "software engineer"], 20)


#-------------------------------------------------------------------------------
# based on the insights, select the ones that should be wrong
#-------------------------------------------------------------------------------

## The words before titles
topaste <- c("future ", "aspiring ", "wannabe ", "retired ", "ex-", "ex ", 
             "former ","not a ", "not ", "can't afford a ", "cannot afford a ",
             "used to be ", "used to be a ", "son of a ", "grandson of a ", 
             "daughter of a ", "granddaughter of a ","wife of a ", 
             "husband of a ")

lst <- list()
for (i in 1:nrow(titled_user_refined)) {
  
  ptitle <- titled_user_refined$title[i]
  togrep <- paste0(topaste, ptitle, collapse="|")
  
  if(grepl(togrep, titled_user_refined$text[i]) == TRUE){
    lst <- c(lst, i)
  }
  
}

lst <- unlist(lst)
# sample some cases to check
sample(titled_user_refined$text[lst], 10)
# save status
titled_user_refined$status[lst] <- "wrong"


# The words after titles
topaste <- c("'s wife", "'s husband", "'s son",  "'s daughter", " services", 
             " studies", " student", " candidate", " wannabe", " becoming", 
             " degree")

lst <- list()
for (i in 1:nrow(titled_user_refined)) {
  
  ptitle <- titled_user_refined$title[i]
  togrep <- paste0(ptitle, topaste, collapse="|")
  
  if(grepl(togrep, titled_user_refined$text[i]) == TRUE){
    lst <- c(lst, i)
  }
  
}

lst <- unlist(lst)
# sample some cases to check
sample(titled_user_refined$text[lst], 10)
# save status
titled_user_refined$status[lst] <- "wrong"

# individual wrong cases for each title
topaste <- c("retired", "forever barista", "rocket surgeon", "my dentist",
             "physical therapist assistant", "coming fashion designer ",
             "firefighter wife", "volunteer firefighter")
togrep <- paste0("\\b", topaste, "\\b", collapse="|")

ind <- grep(togrep, titled_user_refined$text[titled_user_refined$status != "wrong"])
sample(titled_user_refined$text[titled_user_refined$status != "wrong"][ind], 20)
titled_user_refined$status[titled_user_refined$status != "wrong"][ind]  <- "wrong"

#-------------------------------------------------------------------------------
# confident ones
#-------------------------------------------------------------------------------

topaste <- c("professional ", "certified ")
lst <- list()
for (i in 1:nrow(titled_user_refined)) {
  
  ptitle <- titled_user_refined$title[i]
  togrep <- paste0(topaste, ptitle, collapse="|")
  
  if(grepl(togrep, titled_user_refined$text[i]) == TRUE){
    lst <- c(lst, i)
  }
  
}
lst <- unlist(lst)
sample(titled_user_refined$text[lst], 10)

titled_user_refined$status[lst] <- "confident"

#-------------------------------------------------------------------------------
# marking good titles 
# (the titles that are nearly all valid from qualitative inspection)
#-------------------------------------------------------------------------------
good_titles <- c("art director", "civil engineer", "data scientist",
                 "flight attendant", "general manager", "graphic designer",
                 "software engineer", "insurance agent", "interior designer",
                 "lawyer", "managing director", "marketing manager", 
                 "marketing specialist", "massage therapist", "nurse practitioner",
                 "operations manager", "sales manager", "software developer",
                 "estate agent", "physical therapist","registered nurse", 
                 "surgeon", "barber", "dispatcher","fashion designer", 
                 "paralegal", "plumber", "travel agent","truck driver", 
                 "hairstylist", "firefighter","personal trainer", 
                 "receptionist", "locksmith")

titled_user_refined$status[titled_user_refined$title %in% good_titles & 
                             titled_user_refined$status != "wrong"] <- "confident"

#-------------------------------------------------------------------------------
# marking bad titles 
# (the titles that are often not really job titles from qualitative inspection)
#-------------------------------------------------------------------------------
bad_titles <- c("tailor", "chef")
# length(titled_user_refined$status[titled_user_refined$title %in% bad_titles])
titled_user_refined$status[titled_user_refined$title %in% bad_titles & 
                             titled_user_refined$status == "unsure"] <- "wrong"

#-------------------------------------------------------------------------------
# for the rest of titles
# presume that if the n-word job titles appear within the first n words,
# it is correct (excluding words such as "I am", "mom" etc.)
#-------------------------------------------------------------------------------
# other titles
other_titles <- setdiff(unique(titled_user_refined$title), good_titles)
other_titles <- setdiff(other_titles, bad_titles)

# divide the titles based on the number of the words of the title
# in our sample we only have one-word and two-word titles
title_words <- sapply(strsplit(other_titles, " "), length)
other_titles1 <- other_titles[title_words == 1]
other_titles2 <- other_titles[title_words == 2]

firstwords_id <- function(title_list, n){
    # a function that take a list of titles that contains the same number of words
    # and the number of the words of the titles in that list n
    # return the the ids of the users whose profile description's first n words
    # are the job titles
  
    tsub <- titled_user_refined[titled_user_refined$status == "unsure",]
    tsub <- tsub[tsub$title %in% title_list,]
    
    tsub$text <- gsub('[[:punct:] ]+', ' ',  tsub$text)
    tsub$text <- gsub('\n', ' ',  tsub$text)
    tsub$text <- gsub('i am a|i am|wife|husband|mother|father|dad|mom', ' ',  tsub$text)
    tsub$text <- gsub('  ', ' ',  tsub$text)
    tsub$text <- sub("^\\s+", "", tsub$text)
    
    text_length <- sapply(strsplit(tsub$text, " "), length)
    
    # skip the profile description that has less than n words
    tsub$text[text_length > n] <- word(tsub$text[text_length > n], 1, n)
    
    for (i in 1:nrow(tsub)) {
      
      if(grepl(tsub$title[i], tsub$text[i]) == TRUE){
        tsub$status[i] <- "confident"
      }
    }
    
    return(tsub$id[tsub$status == "confident"])
  
}

firstwords_id1 <- firstwords_id(other_titles1, 1)
firstwords_id2 <- firstwords_id(other_titles2, 2)

# combine and label
ids <- c(firstwords_id1, firstwords_id2)
titled_user_refined$status[titled_user_refined$id %in% ids & 
                             titled_user_refined$status == "unsure"] <- "confident"

#save(titled_user_refined, file = "titled_user_refined.rdata")

# also save the ids for users with confident titles for later use
titled_id_confident <- titled_user_refined[titled_user_refined$status == "confident", 
                                           c("id", "title")]

table(titled_id_confident$title)

# remove tailor and waitress as they now have less than observations.
titled_id_confident <- titled_id_confident[!titled_id_confident$title %in% c('tailor', 'waitress'),]

# save(titled_id_confident, file = "titled_id_confident.rdata")



