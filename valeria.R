library(readxl)
library(tidyverse)
library(dplyr)
library(writexl)

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
         population = pc96_pca_tot_p)

### 1999 ###

df_vote_share_diff <- df_1999 %>%
  group_by(district) %>%
  reframe(vote_share_diff = Vote_Share_Percentage[Position == 1] - Vote_Share_Percentage[Position == 2])

df_1999 <- left_join(df_1999, df_vote_share_diff, by = "district", relationship = "many-to-many")

df_1999 <- df_1999 %>%
  mutate(vote_share_diff = ifelse(Position == 2, -vote_share_diff, vote_share_diff),
         population = pc99_pca_tot_p)

### 2004 ###

df_vote_share_diff <- df_2004 %>%
  group_by(district) %>%
  reframe(vote_share_diff = Vote_Share_Percentage[Position == 1] - Vote_Share_Percentage[Position == 2])

df_2004 <- left_join(df_2004, df_vote_share_diff, by = "district", relationship = "many-to-many")

df_2004 <- df_2004 %>%
  mutate(vote_share_diff = ifelse(Position == 2, -vote_share_diff, vote_share_diff),
         population = pc04_pca_tot_p)

### 2009 ###

df_vote_share_diff <- df_2009 %>%
  group_by(district) %>%
  reframe(vote_share_diff = Vote_Share_Percentage[Position == 1] - Vote_Share_Percentage[Position == 2])

df_2009 <- left_join(df_2009, df_vote_share_diff, by = "district", relationship = "many-to-many")

df_2009 <- df_2009 %>%
  mutate(vote_share_diff = ifelse(Position == 2, -vote_share_diff, vote_share_diff),
         population = pc09_pca_tot_p)

df <- bind_rows(df_1996, df_1999, df_2004, df_2009)

unique(df$MyNeta_education)

################################################################################

# Turn education to ordinal variable

education_levels <- c("Illiterate", "Literate", "5th Pass", "8th Pass", "10th Pass", "12th Pass", "Graduate", "Graduate Professional", "Post Graduate", "Doctorate")
df$education <- factor(df$MyNeta_education, levels = education_levels, ordered = TRUE)
df$education[df$education == "Others"] <- NA
df$education <- as.numeric(df$education)

################################################################################

write_xlsx(df, "18.5.2023 - TCPD_GE_All_States_2023-5-18.xlsx")
