library(readxl)
library(writexl)
library(tidyverse)
library(dplyr)
library(stringdist)
library(rdd)
library(rdrobust)
library(broom)
library(rddtools)
library(sandwich)
library(lmtest)
library(multiwayvcov)
library(xtable)


setwd("/Users/valeria/Google Drive/Ph.D./BSE/Thesis/masters_thesis_BSE_2023/")

# nightlights and census merging, filtering down to top two candidates and relevant years

df <- read_csv("data/TCPD_GE_All_States_2023-5-18.csv", show_col_types = F)
nl <- read_csv("data/shrug_nl_wide.csv", show_col_types = F)
key <- read_csv("data/shrug_pc11_district_key.csv", show_col_types = F)

census_91 <- read.csv("data/shrug-v1.5.samosa-pop-econ-census-csv/shrug_pc91.csv")
census_01 <- read.csv("data/shrug-v1.5.samosa-pop-econ-census-csv/shrug_pc01.csv")
census_11 <- read.csv("data/shrug-v1.5.samosa-pop-econ-census-csv/shrug_pc11.csv")

df <- df %>%
  filter(Year == 1996 | Year == 1999 | Year == 2004 | Year == 2009)

nl <- merge(nl, key[, c("shrid", "pc11_district_name")], by = "shrid")
nl <- merge(nl, census_91[, c("shrid", "pc91_pca_tot_p")], by = "shrid")
nl <- merge(nl, census_01[, c("shrid", "pc01_pca_tot_p")], by = "shrid")
nl <- merge(nl, census_11[, c("shrid", "pc11_pca_tot_p")], by = "shrid")

nl <- nl %>%
  select(shrid, pc11_district_name,
         total_light1996, total_light_cal1996, max_light1996,
         total_light1999, total_light_cal1999, max_light1999,
         total_light2004, total_light_cal2004, max_light2004,
         total_light2009, total_light_cal2009, max_light2009,
         total_light2013, total_light_cal2013, max_light2013,
         pc91_pca_tot_p, pc01_pca_tot_p, pc11_pca_tot_p) %>%
  filter(!is.na(pc11_district_name))

nl <- nl %>%
  dplyr::group_by(pc11_district_name) %>%
  dplyr::summarize(across(-shrid, sum, na.rm = TRUE), .groups = "drop") %>%
  dplyr::rename(district = pc11_district_name)

df <- df %>%
  mutate(Constituency_Name = tolower(Constituency_Name)) %>%
  rename(district = Constituency_Name) %>%
  left_join(nl, by = "district") %>%
  filter(!is.na(total_light1996) & (Position == 1 | Position == 2)) %>%
  arrange(district)

# predict population for missing years

predict_population <- function(y1, y2, y3, x1 = 1991, x2 = 2001, x3 = 2011, predict_year = 2014){
  # Calculate slope and intercept
  years <- c(x1, x2, x3)
  populations <- c(y1, y2, y3)
  m <- sum((years - mean(years)) * (populations - mean(populations))) / sum((years - mean(years))^2)
  b <- mean(populations) - m * mean(years)
  
  # Predict population for predict_year
  return(m * predict_year + b)
}

df <- df %>%
  rowwise() %>%
  mutate(
    pc96_pca_tot_p = approx(c(1991, 2001), c(pc91_pca_tot_p, pc01_pca_tot_p), xout = 1996)$y,
    pc99_pca_tot_p = approx(c(1991, 2001), c(pc91_pca_tot_p, pc01_pca_tot_p), xout = 1999)$y,
    pc04_pca_tot_p = approx(c(2001, 2011), c(pc01_pca_tot_p, pc11_pca_tot_p), xout = 2004)$y,
    pc09_pca_tot_p = approx(c(2001, 2011), c(pc01_pca_tot_p, pc11_pca_tot_p), xout = 2009)$y,
    pc14_pca_tot_p = predict_population(pc91_pca_tot_p, pc01_pca_tot_p, pc11_pca_tot_p)
  ) %>%
  ungroup()

# adding vote_share_diff, lights_diff, and population columns

df_list <- split(df, df$Year)

df_1996 <- df_list[[1]]
df_1999 <- df_list[[2]]
df_2004 <- df_list[[3]]
df_2009 <- df_list[[4]]

### 1996 ### 

df_vote_share_diff <- df_1996 %>%
  group_by(district) %>%
  reframe(vote_share_diff = Vote_Share_Percentage[Position == 1] - Vote_Share_Percentage[Position == 2])

df_1996 <- left_join(df_1996, df_vote_share_diff, by = "district", relationship = "many-to-many")

df_1996 <- df_1996 %>%
  mutate(total_light1996 = ifelse(total_light1996 == 0, log(total_light1996 + 0.000001), total_light1996),
         max_light1996 = ifelse(max_light1996 == 0, log(max_light1996 + 0.000001), max_light1996),
         total_light_cal1996 = ifelse(total_light_cal1996 == 0, log(total_light_cal1996 + 0.000001), total_light_cal1996),
         vote_share_diff = ifelse(Position == 2, -vote_share_diff, vote_share_diff),
         population = pc96_pca_tot_p,
         total_light_diff = log(total_light1999) - log(total_light1996),
         max_light_diff = log(max_light1999) - log(max_light1996),
         total_light_cal_diff = log(total_light_cal1999) - log(total_light_cal1996))

### 1999 ###

df_vote_share_diff <- df_1999 %>%
  group_by(district) %>%
  reframe(vote_share_diff = Vote_Share_Percentage[Position == 1] - Vote_Share_Percentage[Position == 2])

df_1999 <- left_join(df_1999, df_vote_share_diff, by = "district", relationship = "many-to-many")

df_1999 <- df_1999 %>%
  mutate(total_light1999 = ifelse(total_light1999 == 0, log(total_light1999 + 0.000001), total_light1999),
         max_light1999 = ifelse(max_light1999 == 0, log(max_light1999 + 0.000001), max_light1999),
         total_light_cal1999 = ifelse(total_light_cal1999 == 0, log(total_light_cal1999 + 0.000001), total_light_cal1999),
         vote_share_diff = ifelse(Position == 2, -vote_share_diff, vote_share_diff),
         population = pc96_pca_tot_p,
         total_light_diff = log(total_light2004) - log(total_light1999),
         max_light_diff = log(max_light2004) - log(max_light1999),
         total_light_cal_diff = log(total_light_cal2004) - log(total_light_cal1999))

### 2004 ###

df_vote_share_diff <- df_2004 %>%
  group_by(district) %>%
  reframe(vote_share_diff = Vote_Share_Percentage[Position == 1] - Vote_Share_Percentage[Position == 2])

df_2004 <- left_join(df_2004, df_vote_share_diff, by = "district", relationship = "many-to-many")

df_2004 <- df_2004 %>%
  mutate(total_light2004 = ifelse(total_light2004 == 0, log(total_light2004 + 0.000001), total_light2004),
         max_light2004 = ifelse(max_light2004 == 0, log(max_light2004 + 0.000001), max_light2004),
         total_light_cal2004 = ifelse(total_light_cal2004 == 0, log(total_light_cal2004 + 0.000001), total_light_cal2004),
         vote_share_diff = ifelse(Position == 2, -vote_share_diff, vote_share_diff),
         population = pc96_pca_tot_p,
         total_light_diff = log(total_light2009) - log(total_light2004),
         max_light_diff = log(max_light2009) - log(max_light2004),
         total_light_cal_diff = log(total_light_cal2009) - log(total_light_cal2004))

### 2009 ###

df_vote_share_diff <- df_2009 %>%
  group_by(district) %>%
  reframe(vote_share_diff = Vote_Share_Percentage[Position == 1] - Vote_Share_Percentage[Position == 2])

df_2009 <- left_join(df_2009, df_vote_share_diff, by = "district", relationship = "many-to-many")

df_2009 <- df_2009 %>%
  mutate(total_light2009 = ifelse(total_light2009 == 0, log(total_light2009 + 0.000001), total_light2009),
         max_light2009 = ifelse(max_light2009 == 0, log(max_light2009 + 0.000001), max_light2009),
         total_light_cal2009 = ifelse(total_light_cal2009 == 0, log(total_light_cal2009 + 0.000001), total_light_cal2009),
         vote_share_diff = ifelse(Position == 2, -vote_share_diff, vote_share_diff),
         population = pc96_pca_tot_p,
         total_light_diff = log(total_light2013) - log(total_light2009),
         max_light_diff = log(max_light2013) - log(max_light2009),
         total_light_cal_diff = log(total_light_cal2013) - log(total_light_cal2009))

df <- bind_rows(df_1996, df_1999, df_2004, df_2009)

# turn education to ordinal variable

education_levels <- c("Illiterate", "Literate", "5th Pass", "8th Pass", "10th Pass", "12th Pass", "Graduate", "Graduate Professional", "Post Graduate", "Doctorate")
df$education <- factor(df$MyNeta_education, levels = education_levels, ordered = TRUE)
df$education[df$education == "Others"] <- NA
df$education <- as.numeric(df$education)

# create year dummies for fixed effects

df <- df %>%
  mutate(Sex = ifelse(Sex == "F", 0,
                      ifelse(Sex == "M", 1, NA)),
         year_1996 = ifelse(Year == 1996, 1, 0),
         year_1999 = ifelse(Year == 1999, 1, 0),
         year_2004 = ifelse(Year == 2004, 1, 0),
         year_2009 = ifelse(Year == 2009, 1, 0))

# include dynast dummy

dynast_list_wiki_pdf_unclean <- read_excel("data/dynast_list_wiki_pdf_unclean.xlsx")
dynast_names <- read_excel("data/dynast_names_matched_fuzzy.xlsx")

df$dynast_wiki <- ifelse(df$Candidate %in% dynast_names$Candidate_trivedi_filtere_wiki, 1, 0)
df$dynast_pdf <- ifelse(df$Candidate %in% dynast_names$Candidate_trivedi_filtered_pdf, 1, 0)
df$dynast <- ifelse(df$dynast_wiki == 1 | df$dynast_pdf == 1, 1, 0)

### OLS ########################################################################

df_ols <- df %>% filter(Position == 1)

df_ols$dynast <- as.factor(df_ols$dynast)

ols <- lm(total_light_cal_diff ~ dynast:year_1999 + dynast:year_2004 + dynast:year_2009
          + year_1999 + year_2004 + year_2009,
          data = df_ols)

summary(ols)

clust_se <- cluster.vcov(ols, df_ols$district)
clust_coef <- coeftest(ols, clust_se)
clust_coef

# Controls

ols_controls <- lm(total_light_cal_diff ~ dynast:year_1999 + dynast:year_2004 + dynast:year_2009
                   + year_1999 + year_2004 + year_2009
                   + population,
                   data = df_ols)

summary(ols_controls)

clust_se_controls <- cluster.vcov(ols_controls, df_ols$district)
clust_coef_controls <- coeftest(ols_controls, clust_se)
clust_coef_controls

df_ols$predicted_controls <- predict(ols_controls, df_ols)

ggplot(df_ols, aes(x = predicted_controls, y = total_light_cal_diff)) + 
  geom_point() + 
  geom_smooth(method='lm', color='red', se=FALSE) +
  labs(x = "Predicted total_light_cal_diff",
       y = "Actual total_light_cal_diff",
       title = "Actual vs Predicted Values") 

### RDD ########################################################################

# keep only the districts where the winner is dynast an loser is non-dynast, or vice versa

df_list <- split(df, df$Year)
df_1996 <- df_list[[1]]
df_1999 <- df_list[[2]]
df_2004 <- df_list[[3]]
df_2009 <- df_list[[4]]

### 1996 ###

winners_1996 <- df_1996 %>% filter(Position == 1)
losers_1996 <- df_1996 %>% filter(Position == 2)

# Merge winners and losers by district, keeping only the necessary columns
merged_1996 <- inner_join(winners_1996 %>% select(district, dynast_winner = dynast),
                          losers_1996 %>% select(district, dynast_loser = dynast),
                          by = "district",
                          relationship = "many-to-many")

# Filter to keep only the districts where the winner is a dynast and the loser is a non-dynast, or vice versa
filtered_1996 <- merged_1996 %>% filter((dynast_winner == 1 & dynast_loser == 0) | (dynast_winner == 0 & dynast_loser == 1))

# Get the final dataset with all columns, keeping only the required rows
final_1996 <- df_1996 %>% filter(district %in% filtered_1996$district)

### 1999 ###

winners_1999 <- df_1999 %>% filter(Position == 1)
losers_1999 <- df_1999 %>% filter(Position == 2)

merged_1999 <- inner_join(winners_1999 %>% select(district, dynast_winner = dynast),
                          losers_1999 %>% select(district, dynast_loser = dynast),
                          by = "district",
                          relationship = "many-to-many")

filtered_1999 <- merged_1999 %>%
  filter((dynast_winner == 1 & dynast_loser == 0) | (dynast_winner == 0 & dynast_loser == 1))

final_1999 <- df_1999 %>% filter(district %in% filtered_1999$district)

### 2004 ###

winners_2004 <- df_2004 %>% filter(Position == 1)
losers_2004 <- df_2004 %>% filter(Position == 2)

merged_2004 <- inner_join(winners_2004 %>% select(district, dynast_winner = dynast),
                          losers_2004 %>% select(district, dynast_loser = dynast),
                          by = "district",
                          relationship = "many-to-many")

filtered_2004 <- merged_2004 %>%
  filter((dynast_winner == 1 & dynast_loser == 0) | (dynast_winner == 0 & dynast_loser == 1))

final_2004 <- df_2004 %>% filter(district %in% filtered_2004$district)

### 2009 ###

winners_2009 <- df_2009 %>% filter(Position == 1)
losers_2009 <- df_2009 %>% filter(Position == 2)

merged_2009 <- inner_join(winners_2009 %>% select(district, dynast_winner = dynast),
                          losers_2009 %>% select(district, dynast_loser = dynast),
                          by = "district",
                          relationship = "many-to-many")

filtered_2009 <- merged_2009 %>%
  filter((dynast_winner == 1 & dynast_loser == 0) | (dynast_winner == 0 & dynast_loser == 1))

final_2009 <- df_2009 %>%
  filter(district %in% filtered_2009$district)

### Merge ###

df <- bind_rows(final_1996, final_1999, final_2004, final_2009)

write_csv(df, "output/26.5.2023 - TCPD_GE_All_States_2023-5-18.csv")

df_1996 <- df %>% filter(dynast == 1 & Year == 1996)
df_1999 <- df %>% filter(dynast == 1 & Year == 1999)
df_2004 <- df %>% filter(dynast == 1 & Year == 2004)
df_2009 <- df %>% filter(dynast == 1 & Year == 2009)

# r rdd

df_rdd <- df %>% filter(dynast == 1) %>% mutate(D = ifelse(vote_share_diff >= 0, 1, 0))

rdd_vanilla = ggplot(data = df_rdd, aes(vote_share_diff, total_light_cal_diff)) +
  geom_point(aes(x = vote_share_diff, y = total_light_cal_diff), data = df_rdd) +
  geom_vline(xintercept = 0) +
  xlab("Margin of victory") +
  ylab("Δ nightlights")

rdd_vanilla

rdd_vanilla + stat_smooth(aes(vote_share_diff, total_light_cal_diff, group = D),
                          method = "lm", formula = y ~ x + I(x^2))

### Pooled RDD ###

rdd_reg <- RDestimate(total_light_cal_diff ~ vote_share_diff,
                      data = df_rdd,
                      cutpoint = 0,
                      bw = 10)

summary(rdd_reg)
plot(rdd_reg)

### 1996 ###

rdd_1996_reg <- RDestimate(total_light_cal_diff ~ vote_share_diff,
                           data = df_1996,
                           cutpoint = 0,
                           bw = 10)

summary(rdd_1996_reg)
plot(rdd_1996_reg)

### 1999 ###

rdd_1999_reg <- RDestimate(total_light_cal_diff ~ vote_share_diff,
                      data = df_1999,
                      cutpoint = 0,
                      bw = 10)

summary(rdd_1999_reg)
plot(rdd_1999_reg)

### 2004 ###

rdd_2004_reg <- RDestimate(total_light_cal_diff ~ vote_share_diff,
                           data = df_2004,
                           cutpoint = 0,
                           bw = 10)

summary(rdd_2004_reg)
plot(rdd_2004_reg)

### 2009 ###

rdd_2009_reg <- RDestimate(total_light_cal_diff ~ vote_share_diff,
                           data = df_2009,
                           cutpoint = 0,
                           bw = 10)

summary(rdd_2009_reg)
plot(rdd_2009_reg)

hist(df_rdd$vote_share_diff, breaks = 50)

quad_rdd <- RDestimate(total_light_cal_diff ~ poly(vote_share_diff, 2),
                       data = df_rdd,
                       cutpoint = 0,
                       bw = 10)

summary(quad_rdd)
plot(quad_rdd)

###

# Non-Dynastic vs. Dynastic Rule by Year
table_rule <- table(df$Year, df$dynast_winner)
colnames(table_rule) <- c("Non-Dynastic Rule", "Dynastic Rule")
table_rule_latex <- xtable(table_rule)
print(table_rule_latex, type = "latex")