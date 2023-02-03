#===============================================================================
# 16_quick_ses_estimation.R
# Purpose: A quick function to estimated SES 
# Author: Yuanmo He
# Reference: codes draws inspirations from Pablo Barberá's Github repository "twitter_ideology"
# https://github.com/pablobarbera/twitter_ideology/blob/master/pkg/tweetscores/R/estimate-ideology2.R
#===============================================================================

load("colmasses.rdata")
load("col_res.rdata")
load("singular_values.rdata")

estimate_ses <- function(follow_list){
    # input a list of a users' followed accounts (in Twitter username)
    # return a SES estimation based on the method used in the paper.
    
    followed_brands <- intersect(unlist(follow_list), col.df$colname)
        
    if(length(followed_brands) == 0){
        stop("Cannot estiamte SES as user followed 0 brands in our algorithm.")
    }
    
    message("The user follows ", length(followed_brands), " brands in our algorithm: ",
            paste(followed_brands, collapse=", "))
    
    points <- matrix(0, nrow = 1, ncol = length(col.df$colname))
    colnames(points) <- col.df$colname
    points[1, which(colnames(points) %in% followed_brands)] <- 1
    
    svphi <- matrix(singular_values[1:3], nrow = 1, ncol = 3, 
                    byrow = TRUE)
    ## adapted from CA package
    cs <- colmasses
    gam.00 <- matrix(as.matrix(col.df[,2:4]), ncol=3)
    SR <- as.matrix(points)*1
    rs.sum <- rowSums(points)
    base2 <- t(SR/matrix(rs.sum, nrow = nrow(SR), ncol = ncol(SR)))
    cs.0 <- matrix(cs, nrow = nrow(base2), ncol = ncol(base2))
    base2 <- base2 - cs.0
    phi2 <- (t(as.matrix(base2)) %*% gam.00)/svphi
    return(phi2[1])
}
