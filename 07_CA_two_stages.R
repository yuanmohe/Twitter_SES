#===============================================================================
# 07_CA_two_stages.R
# Purpose: correspondence analysis in two stages.
# Author: Yuanmo He
# Reference: Some codes draws inspirations from Pablo Barberá's Github repository
#            "twitter_ideology" https://github.com/pablobarbera/twitter_ideology.
#            A large proportion of the codes in Stage 2 is copied with little 
#            alterations from Pablo Barberá's Github repository "echo_chambers":
# https://github.com/pablobarbera/echo_chambers/blob/master/02_estimation/11-second-stage.r
#===============================================================================
library("Matrix")
library("ca")
library("readr")
library("FactoMineR")
library("factoextra")
load("3m_matrix.rdata")
brands_on_twitter <- read_csv("brands_on_twitter.csv")


# Stage 1

# selecting domains
brands_domain <- brands_on_twitter[c("twitter_account_name", "domain")]

supermarket_department <- brands_domain$twitter_account_name[brands_domain$domain == "supermarket" | 
                                                               brands_domain$domain == "department store"]
clothing_speciality <- brands_domain$twitter_account_name[brands_domain$domain == "clothing" | 
                                                            brands_domain$domain == "speciality retail"]
restaurants <- brands_domain$twitter_account_name[brands_domain$domain == "restaurant"]
news <- brands_domain$twitter_account_name[brands_domain$domain == "newspapers" | 
                                             brands_domain$domain == "news"]
sports <- brands_domain$twitter_account_name[brands_domain$domain == "sports"]
tv <- brands_domain$twitter_account_name[brands_domain$domain == "tv shows"]


# selecting informed users
y1 <- y[, colnames(y) %in% supermarket_department]
y2 <- y[, colnames(y) %in% clothing_speciality]
y3 <- y[, colnames(y) %in% restaurants ]
y4 <- y[, colnames(y) %in% news]
y5 <- y[, colnames(y) %in% sports]
y6 <- y[, colnames(y) %in% tv]

informed_index <- which(rowSums(y1) > 0  & rowSums(y2) > 0 & rowSums(y3) > 0  
                & rowSums(y4) > 0 & rowSums(y5) > 0 & rowSums(y6) > 0)

y <- y[informed_index, ]
y <- y[, which(colSums(y) > 1000)]
# save(y, file = "informed_matrix.rdata")

# correspondence analysis for the informed matrix
y <- as.matrix(y)
set.seed(1)
informed_ca_res <- ca(y, nd = 3)
#save(informed_ca_res , file = "informed_ca_res.rdata")
singular_values <- informed_ca_res$sv
#save(singular_values, file = "singular_values.rdata")

# simple diagnosis
get_eigenvalue(informed_ca_res) # variance 2.34%
fviz_ca_col(informed_ca_res) 

# list of informed users for later use
informed_users <- names(informed_index)
# save(informed_users, file = "informed_users.rdata")


# Stage 2

#-------------codes from Barberá------------------------------------------------
# projecting columns
load("3m_matrix.rdata")

##  deleting rows not in first stage
y <- y[dimnames(y)[[1]] %in% informed_ca_res$rownames,]
# deleting columns in first stage (we keep only new columns)
points <- y[, dimnames(y)[[2]] %in% informed_ca_res$colnames == FALSE]

# principal coordinates for rows
Psi <- informed_ca_res$rowcoord 
# new points
h <- matrix(points, ncol=dim(points)[2])
h.sum <- apply(h, 2, sum)
hs <- h/matrix(h.sum, nrow=nrow(h), ncol=ncol(h), byrow=TRUE)
# singular values
svgam <- matrix(informed_ca_res$sv[1:3], nrow=ncol(h), ncol=3, byrow=TRUE)
# projecting and normalizing
g <- (t(hs) %*% Psi) / svgam

col.df <- data.frame(
  colname = c(
    informed_ca_res$colnames, 
    dimnames(y)[[2]][dimnames(y)[[2]] %in% informed_ca_res$colnames == FALSE]),
  coord1 = c(informed_ca_res$colcoord[,1], g[,1]),
  coord2 = c(informed_ca_res$colcoord[,2], g[,2]),
  coord3 = c(informed_ca_res$colcoord[,3], g[,3]),
  stringsAsFactors=F)

col.df <- col.df[order(col.df$colname),]


# projecting rows
load("3m_matrix.rdata")

col.df <- col.df[match(dimnames(y)[[2]], col.df$colname),]
colmasses <- colSums(y) / sum(y)
colcoords <- matrix(as.matrix(col.df[,2:4]), ncol=3)
# save(colmasses, file = "colmasses.rdata")

supplementary_rows <- function(res, points, colmasses, colcoords){
  svphi <- matrix(res$sv[1:3], nrow = nrow(points), ncol = res$nd, 
                  byrow = TRUE)
  ## adapted from CA package
  cs <- colmasses
  gam.00 <- colcoords
  SR <- as.matrix(points)*1
  rs.sum <- rowSums(points)
  base2 <- t(SR/matrix(rs.sum, nrow = nrow(SR), ncol = ncol(SR)))
  cs.0 <- matrix(cs, nrow = nrow(base2), ncol = ncol(base2))
  base2 <- base2 - cs.0
  phi2 <- (t(as.matrix(base2)) %*% gam.00)/svphi
  return(phi2)
}

# directly put in 3.5m exhausts memory
# split in groups of 300,000
groups <- as.numeric(cut(1:nrow(y), c(seq(0, nrow(y), 300000), nrow(y))))
n_groups <- length(unique(groups))
results <- list()
for (i in 1:n_groups){
  cat(i, "/", n_groups, "\n")
  results[[i]] <- supplementary_rows(informed_ca_res, y[which(groups==i),],
                                     colmasses, colcoords)
}

# merge all estimations
row.df <- do.call(rbind, results)

row.df <- data.frame(
  rowname = dimnames(y)[[1]][1:nrow(row.df)],
  coord1 = row.df[,1],
  coord2 = row.df[,2],
  coord3 = row.df[,3],
  sum = rowSums(y),
  stringsAsFactors=F)

#-------------------------------------------------------------------------------
# save(col.df, file = "col_res.rdata")
# save(row.df, file = "07_temp/row_res.Rdata")


# save just the users/brands and their SES estimates
est_brands <- col.df[, 1:2]
colnames(est_brands) <- c("screen_name", "ses")
# Standardization
est_brands$ses <- scale(est_brands$ses)
# save ses as vector instead of matrix
est_brands$ses <- est_brands$ses[,1]
# save(est_brands, file = "est_brands.rdata")

est_users <- row.df[, 1:2]
colnames(est_users) <- c("id", "ses")
# Standardization
est_users$ses <- scale(est_users$ses) 
# save ses as vector instead of matrix
est_users$ses <- est_users$ses[,1]
# save(est_users, file = "est_users.rdata")


