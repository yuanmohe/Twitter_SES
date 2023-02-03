#===============================================================================
# 15_divergent_validity_yougov.R
# Purpose: test divergent validity with data from the YouGov data.
# Author: Yuanmo He
#===============================================================================
library("tidyverse")
library("rstatix")

tw_201905_vars <- read_csv("tw_201905_vars.csv") # YouGov data
load("yougov_res.rdata")

mdf <- merge(yougov_res,  tw_201905_vars)
mdf <- mdf[!is.na(mdf$ses),]

# show the overlaps of NA for the columns
age_na <- which(is.na(mdf$age))
female_na <- which(is.na(mdf$female))
raceeth_na <- which(is.na(mdf$raceeth))
ideo_na <- which(is.na(mdf$ideo))

identical(age_na, female_na)
identical(age_na, raceeth_na)
setdiff(ideo_na, age_na)
# age, female, raceeth has identical NA, ideo has 6 extra
# so one dataframe for age, female, raceeth, one for ideo.
df <- mdf[!is.na(mdf$age),]
df_ideo <- mdf[!is.na(mdf$ideo),]

# the case of income == 97 need to be treat as NA
df$income[df$income == 97] <- NA
df_ideo$income[df_ideo$income == 97] <- NA

sum(!is.na(df$education))
sum(!is.na(df$income))
sum(!is.na(df_ideo$education))
sum(!is.na(df_ideo$income))

# age
cor.test(df$ses, df$age, method = "spearman")
cor.test(df$education, df$age, method = "spearman")
cor.test(df$income, df$age, method = "spearman")

# gender
t.test(df$ses ~ df$female)
t.test(df$education ~ df$female)
t.test(df$income ~ df$female)

#political ideology
cor.test(df_ideo$ses, df_ideo$ideo, method = "spearman")
cor.test(df_ideo$education, df_ideo$ideo, method = "spearman")
cor.test(df_ideo$income, df_ideo$ideo, method = "spearman")

# race
df$race <- as.factor(df$raceeth)
df$race <- relevel(df$race, ref = "White")
racelm <- lm(ses ~ race, data = df)
summary(racelm)

## anova
df %>% anova_test(ses ~ raceeth)
df %>% anova_test(education ~ raceeth)
df %>% anova_test(income ~ raceeth)

# Pairwise comparisons
df %>% pairwise_t_test(ses ~ raceeth, p.adjust.method = "bonferroni")
df %>% pairwise_t_test(education ~ raceeth, p.adjust.method = "bonferroni")
df %>% pairwise_t_test(income ~ raceeth, p.adjust.method = "bonferroni")

# multivariate analysis
df_ideo$race <- as.factor(df_ideo$raceeth)
df_ideo$race <- relevel(df_ideo$race, ref = "White")
df_ideo$female <- as.factor(df_ideo$female)


m1 <- lm(ses ~ education + age + female + race + ideo, data = df_ideo)
summary(m1)

m2 <- lm(ses ~ income + age + female + race + ideo, data = df_ideo)
summary(m2)
