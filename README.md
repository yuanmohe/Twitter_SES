This repository contains codes and data related to the paper "A Method for Estimating Individual Socioeconomic Status of Twitter Users". 

Many codes in preprocessing and correspondence analysis are copied with some alterations or inspirations from Pablo Barberá's Github repositories "[twitter_ideology](https://github.com/pablobarbera/twitter_ideology)" and "[echo_chambers](https://github.com/pablobarbera/echo_chambers)". We mark the lines where the codes are copied with some alterations and declare at the beginning of the scripts that draw inspiration. 

This paper uses Twitter data obtained from the Twitter API (academic access). Due to Twitter Policy restrictions and ethical considerations, we do not share the Twitter data used in this paper. Instead, we provide the data and codes needed to get the relevant data from Twitter API. The codes use wrapper functions in the R library `tweetscores` developed by Pablo Barberá to query Twitter API, and the functions still work well at the time of publishing this repository. If the functions stop working, [Twitter API's official documents](https://developer.twitter.com/en/docs/twitter-api) provide many ways to query from Twitter API. 

The paper also uses data from Facebook Marketing API. We share processed data useful for replication and updated codes to query the Facebook Marketing API.

The YouGov panel data used in the paper are from the paper [The consequences of online partisan media]([https://doi.org/10.1073/pnas.201346411](https://doi.org/10.1073/pnas.2013464118)). We do not have the liberty of sharing the data. Readers need to contact the authors of that paper to obtain the relevant data. For users who follow at least one brand from the brands in our sample, we asked the authors to share a matrix of users and brands representing the following network, and the users' income, education, age, gender, race, and political orientation. 

# data

`brands_on_twitter.rdata`

- 341 brands included in this study, their Twitter account name, and domain (e.g., supermarkets, department stores, etc.). Also available as Supplementary Table 1.

`soc2020.xlsx`

- coding index from [UK's Standard Occupational Classification (SOC)](https://www.ons.gov.uk/methodology/classificationsandstandards/standardoccupationalclassificationsoc/soc2020/soc2020volume2codingrulesandconventions)

`validation_titles.xlsx`

- 52 job titles identified and paired with SOC class and annual mean salary. Also available as Supplementary Table 3.
- annual mean salary from [US May 2019 National Occupational Employment and Wage Estimates](https://www.bls.gov/oes/2019/may/oes_nat.htm)
- an output of `05_match_job_titles.R`

`est_brands.rdata`

- the estimated SES for the 339 brands. It contains two columns, `screen_name` is the brand's Twitter handle, and `ses` is the estimated SES.

`fb_audience.rdata`

- Processed Facebook audience composition data of brands by highest degree earned. 337 rows * 6 variables. Useful for `09_results_brands.R` and replicating Figure 3. The variables Include the brand's Twitter screen name, and the proportions of audience with the highest degree of a high school diploma, Bachelor's degree, Master's degree, doctorate degree, and professional degree, respectively. As mentioned in the paper, no suitable data were available for four brands (moen, Hanes, thehill, WestworldHBO). 
- Facebook Marketing API has changed from version 6.0 at the time of initial data collection (May 2020) to 15.0 at the time of publishing this repository (February 2023). Consequently, the codes for collecting and processing Facebook data that resulted in this dataset are no longer useful. Therefore, we just share this processed dataset here, which is useful for replicating the analysis part of the paper. We share the updated codes for collecting and processing data from Facebook Marketing API at 12_more_FB_data.py , as the data collection and analyses for divergent validity have been conducted more recently. 

`fb_aud_demo.rdata`

- More processed Facebook audience composition data of brands for other demographic variables. Useful for `13_FB_divergent_validity.R`. The variable includes the brand's name, the brand's Twitter screen name, the proportion of urban users, male/female users and users in different age groups. As mentioned in the paper, data is available for 295 brands.

# Scripts

The scripts are written assuming all scripts and data are saved in one main folder, so all scripts work in the same directory as the folder. Each script contains the codes for a step of the research, where there are data worth saving for later steps to prevent data/time loss and improve modularity. At the end of every script, the output data useful for later steps are also saved in the main folder. Sometimes, to prevent data/time loss, temporary data in the XX step are saved in the folder "XX_temp". 

## Preprocessing steps

`01_get_followers.R` gets the followers of the selected brands from Twitter API. 

`02_prepare_userlist.R ` gets a list of users who follow at least 5 brands in the sample.

`03_get_active_users.R` downloads user data from Twitter API and then selects active users based on the user data. 

`04_create_matrix.R` creates a matrix of users*brands (339 * 3482652). 

`05_match_job_titles.R` uses text match to find titles from users' descriptions and select titles that match UK's SOC, and return at least 50 users. This step involves some manual search and assignment of job titles.

`06_refine_titled_users.R` conducts further filtering to eliminate the wrongly matched job titles.

## Results and Validation

`07_CA_two_stages.R` conducts correspondence analysis in two stages. As explained in the paper, the first stage uses correspondence analysis on an informed subset to identify the low-dimensional space. The second stage projects other brands and users.

`08_descriptive_results.R` conducts descriptive analysis and plots for the estimated SES. It replicates Figure 1 and 2 in the paper and produces Supplementary Table 2.

`09_results_brands.R` validates the estimated SES of the brands with the Facebook audience data. It replicates Figure 3 in the paper.

`10_results_users_titled.R` validates the estimated SES of users who are matched with job titles. It replicates Figure 4 in the paper.

`11_results_yougov.R` validates the estimated SES of a small sample of users linked with YouGov survey data. It replicates Figure 5 in the paper. 

 ### Divergent validity

`12_more_FB_data.ipynb` collects and preprocesses more data from Facebook Marketing API to test divergent validity.

`13_divergent_validity_FB.R` tests divergent validity with data from Facebook Marketing API.

`14_divergent_validity_ideology.R`  tests the association between estimated SES with political ideology, measured by [Barberá et al.'s (2015)](https://doi.org/10.1177/0956797615594620) method.

`15_divergent_validity_yougov.R` tests divergent validity with data from the YouGov data.

## Bonus with caution

`16_quick_ses_estimation.R` contains a function that provides an SES estimation of a user based on our method and configuration, given a list of the user's followed accounts' Twitter usernames. The following datasets need to be loaded for the function to work: `colcoords.rdata` , `colmasses.rdata`, and `singular_values.rdata` , which are available in this repository. The datasets contain the column coordinates and masses for the 339 brands in our sample and the singular values estimated from the correspondence analysis. These datasets are provided so readers could write their own functions to estimate more users' SES. However, it is critical to note that the function and datasets just provide quick and dirty ways to estimate SES for some simple sense checks. As the coordinates and parameters are produced with 2020 Twitter data in the US, they should not be directly used for empirical research out of context. To use the proposed method for empirical research, we recommend researchers replicate the steps with data suitable for their research purpose and configure more up-to-date and context-specific estimates.

