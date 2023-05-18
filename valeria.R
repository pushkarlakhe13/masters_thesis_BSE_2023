library(readxl)
library(tidyverse)
library(dplyr)

setwd("/Users/valeria/Google Drive/Ph.D./BSE/Thesis/masters_thesis_BSE_2023/")

df <- read_csv("data/TCPD_GE_All_States_2023-5-18.csv")
nl <- read_csv("data/shrug_nl_wide.csv")
key <- read_csv("data/shrug_pc11_district_key.csv")

census_91 <- read.csv("data/shrug-v1.5.samosa-pop-econ-census-csv/shrug_pc91.csv")
census_01 <- read.csv("data/shrug-v1.5.samosa-pop-econ-census-csv/shrug_pc01.csv")
census_11 <- read.csv("data/shrug-v1.5.samosa-pop-econ-census-csv/shrug_pc11.csv")

df <- df %>%
  filter(Year == 1996 | Year == 1999 | Year == 2004 | Year == 2009 | Year == 2014)

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

