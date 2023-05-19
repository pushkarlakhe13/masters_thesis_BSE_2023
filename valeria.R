library(readxl)
library(writexl)
library(tidyverse)
library(dplyr)
library(stringdist)
library(rdd)
library(rdrobust)
library(broom)
library(rddtools)

setwd("/Users/valeria/Google Drive/Ph.D./BSE/Thesis/masters_thesis_BSE_2023/")

### Merge nightlights and census and clean ###

df <- read_csv("data/TCPD_GE_All_States_2023-5-18.csv")
nl <- read_csv("data/shrug_nl_wide.csv")
key <- read_csv("data/shrug_pc11_district_key.csv")

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
  group_by(pc11_district_name) %>%
  summarize(across(-shrid, sum, na.rm = TRUE)) %>%
  rename(district = pc11_district_name)

df <- df %>%
  mutate(Constituency_Name = tolower(Constituency_Name)) %>%
  rename(district = Constituency_Name) %>%
  left_join(nl, by = "district") %>%
  filter(!is.na(total_light1996) & (Position == 1 | Position == 2)) %>%
  arrange(district)

################################################################################

# Predict population for missing years

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

################################################################################

# Add vote_share_diff and population columns

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
  mutate(vote_share_diff = ifelse(Position == 2, -vote_share_diff, vote_share_diff),
         population = pc96_pca_tot_p,
         total_light_diff = total_light1999 - total_light1996,
         max_light_diff = max_light1999 - max_light1996,
         total_light_cal_diff = total_light_cal1999 - total_light_cal1996)

### 1999 ###

df_vote_share_diff <- df_1999 %>%
  group_by(district) %>%
  reframe(vote_share_diff = Vote_Share_Percentage[Position == 1] - Vote_Share_Percentage[Position == 2])

df_1999 <- left_join(df_1999, df_vote_share_diff, by = "district", relationship = "many-to-many")

df_1999 <- df_1999 %>%
  mutate(vote_share_diff = ifelse(Position == 2, -vote_share_diff, vote_share_diff),
         population = pc99_pca_tot_p,
         total_light_diff = total_light2004 - total_light1999,
         max_light_diff = max_light2004 - max_light1999,
         total_light_cal_diff = total_light_cal2004 - total_light_cal1999)

### 2004 ###

df_vote_share_diff <- df_2004 %>%
  group_by(district) %>%
  reframe(vote_share_diff = Vote_Share_Percentage[Position == 1] - Vote_Share_Percentage[Position == 2])

df_2004 <- left_join(df_2004, df_vote_share_diff, by = "district", relationship = "many-to-many")

df_2004 <- df_2004 %>%
  mutate(vote_share_diff = ifelse(Position == 2, -vote_share_diff, vote_share_diff),
         population = pc04_pca_tot_p,
         total_light_diff = total_light2009 - total_light2004,
         max_light_diff = max_light2009 - max_light2004,
         total_light_cal_diff = total_light_cal2009 - total_light_cal2004)

### 2009 ###

df_vote_share_diff <- df_2009 %>%
  group_by(district) %>%
  reframe(vote_share_diff = Vote_Share_Percentage[Position == 1] - Vote_Share_Percentage[Position == 2])

df_2009 <- left_join(df_2009, df_vote_share_diff, by = "district", relationship = "many-to-many")

df_2009 <- df_2009 %>%
  mutate(vote_share_diff = ifelse(Position == 2, -vote_share_diff, vote_share_diff),
         population = pc09_pca_tot_p,
         total_light_diff = total_light2013 - total_light2009,
         max_light_diff = max_light2013 - max_light2009,
         total_light_cal_diff = total_light_cal2013 - total_light_cal2009)

df <- bind_rows(df_1996, df_1999, df_2004, df_2009)

################################################################################

# Turn education to ordinal variable

education_levels <- c("Illiterate", "Literate", "5th Pass", "8th Pass", "10th Pass", "12th Pass", "Graduate", "Graduate Professional", "Post Graduate", "Doctorate")
df$education <- factor(df$MyNeta_education, levels = education_levels, ordered = TRUE)
df$education[df$education == "Others"] <- NA
df$education <- as.numeric(df$education)

################################################################################

df <- df %>%
  mutate(Sex = ifelse(Sex == "F", 0,
                      ifelse(Sex == "M", 1, NA)),
         year_1996 = ifelse(Year == 1996, 1, 0),
         year_1999 = ifelse(Year == 1999, 1, 0),
         year_2004 = ifelse(Year == 2004, 1, 0),
         year_2009 = ifelse(Year == 2009, 1, 0))

write_xlsx(df, "output/20.5.2023 - TCPD_GE_All_States_2023-5-18.xlsx")

################################################################################

### Pushkar's script ###

dynast_list_wiki_pdf_unclean <- read_excel("data/dynast_list_wiki_pdf_unclean.xlsx")
dynast_names <- read_excel("data/dynast_names_matched_fuzzy.xlsx")

df$dynast_wiki <- ifelse(df$Candidate %in% dynast_names$Candidate_trivedi_filtere_wiki, 1, 0)
df$dynast_pdf <- ifelse(df$Candidate %in% dynast_names$Candidate_trivedi_filtered_pdf, 1, 0)
df$dynast <- ifelse(df$dynast_wiki == 1 | df$dynast_pdf == 1, 1, 0)

################################################################################

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
                          by = "district")

# Filter to keep only the districts where the winner is a dynast and the loser is a non-dynast, or vice versa
filtered_1996 <- merged_1996 %>% filter((dynast_winner == 1 & dynast_loser == 0) | (dynast_winner == 0 & dynast_loser == 1))

# Get the final dataset with all columns, keeping only the required rows
final_1996 <- df_1996 %>% filter(district %in% filtered_1996$district)

### 1999 ###

winners_1999 <- df_1999 %>% filter(Position == 1)
losers_1999 <- df_1999 %>% filter(Position == 2)

merged_1999 <- inner_join(winners_1999 %>% select(district, dynast_winner = dynast),
                          losers_1999 %>% select(district, dynast_loser = dynast),
                          by = "district")

filtered_1999 <- merged_1999 %>%
  filter((dynast_winner == 1 & dynast_loser == 0) | (dynast_winner == 0 & dynast_loser == 1))

final_1999 <- df_1999 %>% filter(district %in% filtered_1999$district)

### 2004 ###

winners_2004 <- df_2004 %>% filter(Position == 1)
losers_2004 <- df_2004 %>% filter(Position == 2)

merged_2004 <- inner_join(winners_2004 %>% select(district, dynast_winner = dynast),
                          losers_2004 %>% select(district, dynast_loser = dynast),
                          by = "district")

filtered_2004 <- merged_2004 %>%
  filter((dynast_winner == 1 & dynast_loser == 0) | (dynast_winner == 0 & dynast_loser == 1))

final_2004 <- df_2004 %>% filter(district %in% filtered_2004$district)

### 2009 ###

winners_2009 <- df_2009 %>% filter(Position == 1)
losers_2009 <- df_2009 %>% filter(Position == 2)

merged_2009 <- inner_join(winners_2009 %>% select(district, dynast_winner = dynast),
                          losers_2009 %>% select(district, dynast_loser = dynast),
                          by = "district")

filtered_2009 <- merged_2009 %>%
  filter((dynast_winner == 1 & dynast_loser == 0) | (dynast_winner == 0 & dynast_loser == 1))

final_2009 <- df_2009 %>%
  filter(district %in% filtered_2009$district)

### Merge ###

df <- bind_rows(final_1996, final_1999, final_2004, final_2009)
df_1996 <- df %>% filter(dynast == 1 & Year == 1996)
df_1999 <- df %>% filter(dynast == 1 & Year == 1999)
df_2004 <- df %>% filter(dynast == 1 & Year == 2004)
df_2009 <- df %>% filter(dynast == 1 & Year == 2009)

df <- df %>% mutate(dynast_winner = ifelse(dynast == 1 & Position == 1, 1, 0),
                    D = ifelse(vote_share_diff >= 0, 1, 0))

ols <- lm(max_light_diff ~ dynast_winner*year_1996
          + dynast_winner*year_1999
          + dynast_winner*year_2004
          + dynast_winner*year_2009, data = df)

summary(ols)

df <- df %>% filter(dynast == 1)

gg_srd = ggplot(data = df, aes(vote_share_diff, max_light_diff)) +
  geom_point(aes(x = vote_share_diff, y = max_light_diff), data = df) +
  geom_vline(xintercept = 0) +
  xlab("Margin of victory") +
  ylab("Δ nightlights")

gg_srd + stat_smooth(aes(vote_share_diff, max_light_diff, group = D),
                     method = "lm", formula = y ~ x + I(x^2))

gg_srd + stat_smooth(data = df %>% filter(vote_share_diff > (-10) & vote_share_diff < 10),
                     aes(vote_share_diff, max_light_diff, group = D),
                     method = "lm", formula = y ~ x + I(x^2))

rdd_reg <- RDestimate(max_light_diff ~ vote_share_diff, data = df, cutpoint = 0)
summary(rdd_reg)
plot(rdd_reg)

# Plot residuals to check continuity assumption
residuals <- residuals(rdd_reg)
plot(df$vote_share_diff, residuals)

# Check for discontinuity in other variables
plot(df$vote_share_diff, df$population)

# Include polynomial terms
quad_rdd <- RDestimate(max_light_diff ~ poly(vote_share_diff, 2), data = df, cutpoint = 0, bw = 10)
summary(quad_rdd)
plot(quad_rdd)

# Use heteroscedasticity-robust and cluster-robust standard errors
robust_rdd <- RDestimate(max_light_diff ~ vote_share_diff, data = df, cutpoint = 0, se = "HC1")
cluster_rdd <- RDestimate(max_light_diff ~ vote_share_diff, data = df, cutpoint = 0, cluster = df$district)  # replace with your actual cluster variable
summary(robust_rdd)
summary(cluster_rdd)
