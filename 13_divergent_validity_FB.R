#===============================================================================
# 13_divergent_validity_FB.R
# Purpose: test divergent validity with data from Facebook Marketing API
# Author: Yuanmo He
#===============================================================================
library("tidyverse")

load("est_brands.rdata")
brand_twitter <- read_csv("brands_on_twitter.csv")[c("brand", "twitter_account_name")]

lower_mau <- read_csv("lower_mau.csv")
upper_mau <- read_csv("upper_mau.csv")

# use the middle number here
# tried upper, middle, lower, results are essentially the same
# to change to lower or upper, just change the dataframe in the following lines
# from middle to upper_mau or lower_mau
middle <- upper_mau
middle[2:ncol(middle)] <- (upper_mau[2:ncol(middle)] + lower_mau[2:ncol(middle)]) / 2

head(middle[order(middle$all_US),])
# Simple inspection shows that for "Brio Tuscan Grill" and "Makita Tools",
# most of their measures are 1000 for the specific categories, 
# indicating unreliable data, so we remove these two for later analyses

middle <- middle[!middle$brand %in% c("Brio Tuscan Grill", "Makita Tools"),]

# get the proportions
middle[3:ncol(middle)] <- middle[3:ncol(middle)] / middle$all_US

# save data for sharing 
## delete non-processed data
fb_aud_demo <- subset(middle, select = -c(all_US))
## merge to get twitter screen name
fb_aud_demo <- merge(brand_twitter, fb_aud_demo, by = "brand")
fb_aud_demo <- fb_aud_demo %>% rename(screen_name = twitter_account_name)
## delete brands already removed from the study
fb_aud_demo <- fb_aud_demo[!fb_aud_demo$screen_name %in% c("Red Mango", "SaatvaMattress"),]
save(fb_aud_demo, file = "fb_aud_demo.rdata")


# merging to get estimated SES
mdf <- merge(est_brands, fb_aud_demo, by = "screen_name")

# urban
cor.test(mdf$ses, mdf$urban, method = "spearman")

# gender
cor.test(mdf$ses, mdf$male, method = "spearman")
cor.test(mdf$ses, mdf$female, method = "spearman")

# age
cor.test(mdf$ses, mdf$age18_24, method = "spearman")
cor.test(mdf$ses, mdf$age25_34, method = "spearman")
cor.test(mdf$ses, mdf$age35_44, method = "spearman")
cor.test(mdf$ses, mdf$age45_54, method = "spearman")
cor.test(mdf$ses, mdf$age55_64, method = "spearman")
cor.test(mdf$ses, mdf$age65plus, method = "spearman")
