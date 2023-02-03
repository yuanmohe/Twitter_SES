#===============================================================================
# 08_results_brands.R
# Purpose: validate the estimated SES of the brands.
# Author: Yuanmo He
#===============================================================================
library("tidyverse")
library("ggrepel")
library("ggpubr")
library("ggpmisc")
library("gridExtra")

load("est_brands.rdata")
load("fb_audience.rdata")

# change column names to make it easier to call
mdf <- merge(est_brands, fb_audience, by = "screen_name")
mdf <- mdf %>% 
  rename(
    "high_school" = "high school grad",
    "ug" = "university graduate",
    "master" = "master degree",
    "doctor" = "doctorate degree",
    "pro" = "professional degree"
  )

mdf$higher <- mdf$master + mdf$pro + mdf$doctor

#  remove cases where FB does not have a reasonble estimates
ol <- c("FinishLine", "GNCLiveWell", "GreysABC", "Gap", 
        "LEVIS", "MakitaTools", "CodeBlackCBS")

mdf <- mdf[!mdf$screen_name %in% ol,]

cor.test(mdf$ses, mdf$high_school, method = "spearman") 
cor.test(mdf$ses, mdf$ug,  method = "spearman")
cor.test(mdf$ses, mdf$higher, method = "spearman")

# plot figure 3
g1 <- ggplot(mdf, aes(x = ses, y = high_school, label = screen_name)) + 
    geom_point(size = 0.4, color = "blue") + 
    stat_dens2d_filter(geom = "text_repel", keep.fraction = 0.04, size = 3, segment.size = 0.2, min.segment.length = 0) +
    labs(subtitle = "A) High school diploma", y = "proportion") +
    xlab("estimated SES") +
    expand_limits(y = 0) + 
    theme_bw() +
    theme(plot.subtitle = element_text(hjust = 0.5)) 
    

g2 <- ggplot(mdf, aes(x = ses, y = ug, label = screen_name)) + 
    geom_point(size = 0.4, color = "blue") + 
    stat_dens2d_filter(geom = "text_repel", keep.fraction = 0.04, size = 3, segment.size = 0.2, min.segment.length = 0) +
    labs(subtitle = "B) Bachelor's degree", y = "proportion") +
    xlab("estimated SES") +
    expand_limits(y = 0) + 
    theme_bw() +
    theme(plot.subtitle = element_text(hjust = 0.5)) 

g3 <- ggplot(mdf, aes(x = ses, y = higher, label = screen_name)) + 
    geom_point(size = 0.4, color = "blue") + 
    stat_dens2d_filter(geom = "text_repel", keep.fraction = 0.04, size = 3, segment.size = 0.2, min.segment.length = 0) +
    labs(subtitle = "C) Master's degree or higher", y = "proportion") +
    xlab("estimated SES") +
    expand_limits(y = 0) + 
    theme_bw() +
    theme(plot.subtitle = element_text(hjust = 0.5)) 

p <- grid.arrange(g1, g2, g3, nrow = 2, 
             layout_matrix = matrix(c(1, 1, 2, 2, 4, 3, 3, 4), nrow = 2, byrow = TRUE))

ggsave("figure3.png", p, width = 10, height = 8, scale = 0.9)


