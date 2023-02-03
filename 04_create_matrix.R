#===============================================================================
# 04_create_matrix.R
# Purpose: Create a matrix of users*brands.
# Author: Yuanmo He
# Reference: a large proportion of the codes in this file is copied with little 
#   alterations from Pablo Barberá's Github repository "twitter_ideology": 
#   https://github.com/pablobarbera/twitter_ideology/blob/master/replication/03-create-adjacency-matrix.R
#===============================================================================
library("Matrix")

#-------------codes from Barberá------------------------------------------------

# brands M
outfolder <- "followers_lists"
fls <- list.files(outfolder, full.names=TRUE)
brands <- list.files(outfolder)
brands <- gsub(".rdata", "", brands)
m <- length(brands)

# users N
load("active_us_users.rdata")
n <- length(active_us_users)

# creating adjacency matrix
rows <- list()
columns <- list()

for (j in 1:m){
  cat(fls[j])
  load(fls[j])
  to_add <- which(active_us_users %in% followers)
  rows[[j]] <- to_add
  columns[[j]] <- rep(j, length(to_add))
  print(j)
  
}

rows <- unlist(rows)
columns <- unlist(columns)

y <- sparseMatrix(i = rows, j = columns)

rownames(y) <- active_us_users
colnames(y) <- brands

#-------------------------------------------------------------------------------


colSums(y)[order(colSums(y))][1:10]
# 'Red Mango' and  'SaatvaMattress' are only followed by 0 and 1 users, 
# while other brands are followed by at least 1000. 
# These two brands are not going to be useful for the CA 
# so I deleted them at this stage. 
# Then I select the ones who follow at least 5 brands in the  new sample.

y <- y[, -which(colnames(y) %in% c("Red Mango", "SaatvaMattress"))]
y <- y[rowSums(y) >= 5,]

# save(y, file = "3m_matrix.rdata")
