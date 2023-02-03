#===============================================================================
# 11_results_yougov.R
# Purpose: validate the estimated SES of a small sample of users linked with YouGov survey data.
# Author: Yuanmo He
#===============================================================================
library("labelled")
library("tidyverse")
library("Matrix")
library("gridExtra")

load("informed_ca_res.rdata")
load("yougov_201905.rdata") # yougov data
brands_on_twitter <- read_csv("brands_on_twitter.csv")
load("colmasses.rdata")
load("col_res.rdata")


# function for projection, adapted from Barbera
supplementary_rows <- function(res, points, colmasses, col.df){
    svphi <- matrix(res$sv[1:3], nrow = nrow(points), ncol = res$nd, 
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
    return(phi2)
}

# transform the yougov data to matrix
user_brand <- yougov_201905[, 2:345]
l <- var_label(user_brand)
l <- unlist(l)
colnames(user_brand) <- l
var_label(user_brand) <- NULL
user_brand <- user_brand[, -which(colnames(user_brand) %in% c("Red Mango", "Saatva"))]

## match brand names with twitter_screen name
brand <- brands_on_twitter[, c("brand", "twitter_account_name")]
brand <- merge(col.df, brand, by.x = "colname", by.y = "twitter_account_name")
brand <- brand[,c("brand", "colname")]

## manual changes
user_brand <- user_brand %>% 
  rename(
    "Cold Stone Creamery" = "Cold Stone Creamery\t",
    "Hooters" = "Hooters\t",
  )

## replace brand names with Twitter screen_name
names(user_brand) <- brand$colname[match(names(user_brand), brand$brand)]

## prepare the matrix for projection
m <- as.matrix(user_brand)
rownames(m) <- yougov_201905$id
m <- m[, sort(colnames(m))] 
m <- m[, match(col.df$colname, colnames(m))]
# save(m, file = "yougov_matrix.rdata")

# project
est <- supplementary_rows(informed_ca_res, m, colmasses, col.df)
yougov_res <- yougov_201905[c("id", "W1_educ", "W1_employ", "W1_faminc_new")]
yougov_res <- yougov_res[yougov_res$id %in% rownames(m),]
yougov_res$ses <- est[, 1]
colnames(yougov_res)[2:4] <- c("education", "employment", "income")
yougov_res$nbrands <- rowSums(m)
# save(yougov_res, file = "yougov_res.rdata")

# analysis
df <- na.omit(yougov_res)
cor.test(df$education, df$ses, method = "spearman")

df_income <- df[df$income != 97,] # 97 means "prefer not to say".
cor.test(df_income$income, df_income$ses, method = "spearman")

cor.test(df_income$income, df_income$education, method = "spearman")

# plots Figure 5
set.seed(1)

g1 <- ggplot(df_income, aes(x = ses, y = education, fill = income)) +
    geom_jitter(width = 0, height = 0.3, pch = 21, size = 2) +
    labs(subtitle = "A) Educational Level") + 
    theme_bw() +
    theme(plot.subtitle = element_text(hjust = 0.5), 
          axis.text.y = element_text(angle = 0, size = 9),
          legend.position = "top",
          legend.key.size = unit(0.25, "cm"),
          legend.text = element_blank(),
          legend.title = element_text(size = 10)) + 
    scale_fill_viridis_c(direction = -1) +  
    xlab("estimated SES") +
    scale_y_continuous(breaks = c(1:6), 
                       labels = c("No high school", "High school graduates",
                                  "Some college", "Two-year college",
                                  "Four-year college", "Post-graduate"))
   

g2 <- ggplot(df_income, aes(x = ses, y = income, fill = education)) +
    geom_jitter(width = 0, height = 0.3, pch = 21, size = 2) +
    labs(subtitle = "B) Income") + 
    theme_bw() +
    theme(plot.subtitle = element_text(hjust = 0.5), 
          axis.text.y = element_text(angle = 0, size = 9),
          legend.position= "top",
          legend.key.size = unit(0.25, "cm"),
          legend.text = element_blank(),
          legend.title = element_text(size = 10)) + 
    scale_fill_viridis_c(direction = -1) +  
    xlab("estimated SES") + 
    scale_y_continuous(breaks = c(1:16), 
                       labels = c("less than $10,000", "$10,000-$19,999",
                                  "$20,000-$29,999", "$30,000-$39,999",
                                  "$40,000-$49,999", "$50,000-$59,999", 
                                  "$60,000-$69,999", "$70,000-$79,999",
                                  "$80,000-$99,999", "$100,000-$119,999", 
                                  "$120,000-$149,999", "$150,000-$199,999", 
                                  "$200,000-$249,999", "$250,000-$349,999", 
                                  "$350,000-$499,999", "$500,000 or more"))

ggsave("figure5_2.png", grid.arrange(g1, g2, nrow = 1), width = 10, height = 5)

# following two or more bands
df <- na.omit(yougov_res)
df <- df[df$nbrands >= 3,]
cor.test(df$education, df$ses, method = "spearman")

df_income <- df[df$income != 97,] # 97 means "prefer not to say".
cor.test(df_income$income, df_income$ses, method = "spearman")
cor.test(df_income$income, df_income$education, method = "spearman")


