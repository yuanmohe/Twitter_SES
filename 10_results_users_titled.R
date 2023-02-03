#===============================================================================
# 10_results_users_titled.R
# Purpose:  validate the SES of users who are matched with job titles.
# Author: Yuanmo He
#===============================================================================
library("tidyverse")
library("ggrepel")
library("readxl")
library("ggpubr")
library("ggpmisc")
library("gridExtra")
load("est_users.rdata")
load("titled_id_confident.rdata")
validation_titles <- read_excel("validation_titles.xlsx")

# pair user with titles
id_title_est <- merge(est_users, titled_id_confident, "id")

# correlation aggregated at title level
title_ses <- aggregate(ses ~ title, FUN = median, data = id_title_est)
colnames(title_ses)[2] <- "ses"

# get all information about the titles
title_all <- merge(title_ses, validation_titles, "title")

# correlation aggregated at title level
cor.test(title_all$ses, title_all$salary, method = "spearman")
cor.test(title_all$ses, title_all$class, method = "spearman")
cor.test(title_all$class, title_all$salary, method = "spearman")

# get summary statistics for each title for later use
title_sum <- id_title_est %>%
  group_by(title) %>%
  summarise_at(vars(ses), funs(median, n(), sd))

title_sum$se <- title_sum$sd / sqrt(title_sum$n)
title_sum$median_se <- title_sum$se * 1.25

title_all <- merge(title_all, title_sum, "title")

# plot Figure 4
title_all$class <- as.factor(title_all$class)

ggplot(title_all, aes(x = median, y = salary, color = class, shape = class)) + 
    geom_point() + 
    geom_text_repel(aes(label = title), size = 2, color = "Black", segment.size = 0.3) +
    labs(x = "median estimated SES",
         y = "mean annual salary") +
    theme_bw() +
    theme(plot.caption = element_text(hjust=0.5, size=rel(1))) +
    geom_errorbarh(aes(xmin = median - median_se, xmax = median + median_se)) +
    scale_y_log10(breaks = c(25000, 50000, 100000, 200000)) +
    scale_colour_viridis_d() + 
    scale_shape_manual(values = 0:9)
    


ggsave("figure4.png", width = 10, height = 7, scale = 0.8)


# reference
# color  https://ggplot2.tidyverse.org/reference/scale_viridis.html
# shape https://stackoverflow.com/questions/26223857/more-than-six-shapes-in-ggplot