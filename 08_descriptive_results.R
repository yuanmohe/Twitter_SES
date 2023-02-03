#===============================================================================
# 08_descriptive_results.R
# Purpose: descriptive results and plots for the estimated SES.
# Author: Yuanmo He
#===============================================================================
library("tidyverse")
library("ggrepel")
library("ggpubr")
library("ggpmisc")
library("gridExtra")

load("est_brands.rdata")
load("est_users.rdata")


# descriptive statistics
summary(est_brands$ses)
summary(est_users$ses)

# plot Figure 1 in the paper
g1 <- ggplot(est_brands, aes(x = ses)) +
    geom_density() +
    geom_vline(aes(xintercept = median(ses)),
               color="blue", linetype = "dashed", size = 1) +
    geom_text(aes(-0.9, 0.5), label = "median = 0.036", size = 3.5)+
    xlab("estimated SES")  +
    labs(subtitle = "A) brands") + 
    theme_bw() +
    theme(plot.subtitle = element_text(hjust = 0.5))

g2 <- ggplot(est_users, aes(x = ses)) +
    geom_density() +
    geom_vline(aes(xintercept = 0.183),
               color="blue", linetype = "dashed", size = 1) +
    geom_text(aes(-1.2, 0.46), label = "median = 0.183", size = 3.5)+
    xlab("estimated SES") +
    labs(subtitle = "B) users") +
    theme_bw() +
    theme(plot.subtitle = element_text(hjust = 0.5))
    

ggsave("figure1.png", grid.arrange(g1, g2, nrow = 1), width = 10, height = 4, scale = 1)


# plot Figure 2 in the paper
# empty space is cropped with the Preview software on Macbook.
selected_brands <- c("NPR", "nytimes", "WSJ", "60Minutes", "onepeloton", 
                     "MadMen_AMC", "WholeFoods","Starbucks", "NBA", 
                     "McDonalds", "kfc", "myfamilydollar", "pizzahut",
                     "BurgerKing", "	BurgerKing", "DollarGeneral", 
                     "BestBuy", "TrueValue", "FoodLion", "Target", "Honda",
                     "HomeGoods","Gap", "hmusa", "chevrolet", "Toyota", 
                     "TiffanyAndCo", "washingtonpost", "soulcycle",
                     "bigbangtheory", "BigLots", "AldiUSA", "Walmart",
                     "Nike", "NFL")

df <- est_brands[est_brands$screen_name %in% selected_brands, ]

f2 <- ggplot(df, aes(x = ses, y = 0, label = screen_name)) +
    geom_point(size = 1, color = "blue") +
    geom_label_repel(size = 2.5, segment.size = 0.3, max.overlaps = 15) +
    ylim(-1, 10) +
    theme_bw() +
    theme(
        axis.line.y  = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y  = element_blank(),
        axis.title.y = element_blank()
    ) +
    labs(x = "estimated SES") 

ggsave("figure2.png", f2, width = 8, height = 4, scale = 1)



# ordered brand SES for Supplementary Table 2
brands_on_twitter <- read_csv("brands_on_twitter.csv")
est_brands_ordered <- merge(brands_on_twitter, est_brands, 
                            by.x = "twitter_account_name", by.y = "screen_name")
est_brands_ordered <- est_brands_ordered[c("brand", "twitter_account_name", "domain", "ses")]
est_brands_ordered <- est_brands_ordered[order(est_brands_ordered$ses, decreasing = TRUE),]
write_csv(est_brands_ordered, "est_brands_ordered.csv")
