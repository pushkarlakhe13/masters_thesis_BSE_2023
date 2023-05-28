library(dplyr)
library(tidyverse)
library(lmtest)
library(stringr)
library(rdd)
library (ggplot2)
library(ggthemes)
library(scales)
library(hrbrthemes)
library(forcats)

setwd("/Users/valeria/Google Drive/Ph.D./BSE/Thesis/masters_thesis_BSE_2023")

data <- read.csv("output/23.5.2023 - TCPD_GE_All_States_2023-5-18.csv", header = T)

theme_set(theme_ipsum_rc(grid = "Y"))

data <- data %>%
  mutate(winner = if_else(vote_share_diff > 0, 1, 0))

# Calculate the percentages for all candidates
all_candidates <- data %>%
  group_by(Sex) %>%
  dplyr::summarise(n = n(), 
            percentage = n() / nrow(data) * 100)

# Calculate the percentages for winners
winners <- data %>%
  filter(winner == 1) %>%
  group_by(Sex) %>%
  dplyr::summarise(n = n(), 
            percentage = n() / nrow(data[data$winner == 1,]) * 100)

# Calculate the percentages for dynastic candidates
dynastic_candidates <- data %>%
  filter(dynast == 1) %>%
  group_by(Sex) %>%
  dplyr::summarise(n = n(), 
            percentage = n() / nrow(data[data$dynast == 1,]) * 100)

# Print the results
print(all_candidates)
print(winners)
print(dynastic_candidates)

all_candidates_filtered <- all_candidates[!is.na(all_candidates$Sex), ]
winners_filtered <- winners[!is.na(winners$Sex), ]
dynastic_candidates_filtered <- dynastic_candidates[!is.na(dynastic_candidates$Sex), ]

# Create a new data frame with a grouping variable indicating the data subsets
all_data <- rbind(transform(all_candidates_filtered, Subset = "All Candidates"),
                  transform(winners_filtered, Subset = "Winners"),
                  transform(dynastic_candidates_filtered, Subset = "Dynastic Candidates"))

custom_palette <- c("#EDB500", "#008571") # Replace with your desired colors

# Create the combined plot using facet_wrap
ggplot(all_data, aes(x = as.factor(Sex), y = percentage, fill = as.factor(Sex))) +
  geom_bar(stat = "identity", width = 0.6, alpha = 0.9) +
  scale_x_discrete(labels = NULL) +  # Remove x-axis labels
  scale_fill_manual(values = custom_palette, labels = c("Female", "Male")) +  # Set legend title and labels
  labs(y = "Percentage", title = "Percentage of Candidates by Sex") +
  scale_y_continuous(breaks = pretty_breaks(n = 10), labels = abs) +
  facet_wrap(~ Subset, nrow = 1) +
  labs(
    x = NULL,
    y = NULL,   # Remove y-axis label
    fill = NULL  # Remove legend label
  ) +
  theme(legend.position = "bottom") +  # Move the legend to the bottom
  geom_text(
    aes(label = paste0(round(percentage), "%")),
    position = position_stack(vjust = 0.5),  # Adjust the vertical position of the text
    size = 3,  # Adjust the font size of the text
    color = "white"  # Set the text color
  )
 
dynastic_winners_sex <- data %>%
  filter(dynast == 1, winner == 1) %>%
  group_by(Sex) %>%
  dplyr::summarise(n = n(), 
            percentage = n() / nrow(data[data$dynast == 1 & data$winner == 1,]) * 100)

# Calculate the percentages for dynastic winners with vote share diff up to 5
dynastic_winners_votediff <- data %>%
  filter(dynast == 1, winner == 1, vote_share_diff <= 5) %>%
  group_by(Sex) %>%
  dplyr::summarise(n = n(), 
            percentage = n() / nrow(data[data$dynast == 1 & data$winner == 1 & data$vote_share_diff <= 5,]) * 100)

# Print the results
print(dynastic_winners_sex)
print(dynastic_winners_votediff)

# Set up the plot layout
par(mfrow = c(1, 2))

# Plot for dynastic winners by sex

# Combine the data frames and create a new grouping variable
combined_data <- bind_rows(
  mutate(dynastic_winners_sex, Group = "Dynastic Winners"),
  mutate(dynastic_winners_votediff, Group = "Dynastic Winners\n(Vote Margin Up to 5%)")
)

# Create the combined plot using facet_wrap
ggplot(combined_data, aes(x = as.factor(Sex), y = percentage, fill = as.factor(Sex))) +
  geom_bar(stat = "identity", width = 0.6, alpha = 0.9) +
  scale_x_discrete(labels = NULL) +  # Remove x-axis labels
  scale_fill_manual(values = custom_palette, labels = c("Female", "Male")) +  # Set legend labels
  labs(y = "Percentage", title = "Percentage of Dynastic Winners by Sex") +
  scale_y_continuous(breaks = pretty_breaks(n = 10), labels = abs) +
  facet_wrap(~ Group, nrow = 1) +
  labs(x = NULL, y = NULL, fill = NULL) +  # Remove axis and legend labels
  theme(legend.position = "bottom", panel.spacing = unit(0.2, "lines")) +  # Move the legend to the bottom and adjust panel spacing
  geom_text(
    aes(label = paste0(round(percentage), "%")),
    position = position_stack(vjust = 0.5),  # Adjust the vertical position of the text
    size = 3,  # Adjust the font size of the text
    color = "white"  # Set the text color
  )

# Calculate percentages for each group
grouped_data <- data %>%
  group_by(MyNeta_education) %>%
  dplyr::summarise(total = n(),
            dynasts = sum(dynast),
            non_dynasts = total - dynasts,
            winners = sum(winner),
            losers = total - winners,
            dynastic_winners = sum(dynast & winner),
            non_dynastic_winners = winners - dynastic_winners,
            dynastic_winners_vote_diff = sum(dynast & winner & vote_share_diff <= 5),
            non_dynastic_winners_vote_diff = sum(winner & vote_share_diff <= 5) - dynastic_winners_vote_diff) %>%
  mutate(dynasts = dynasts / total * 100,
         non_dynasts = non_dynasts / total * 100,
         winners = winners / total * 100,
         losers = losers / total * 100,
         dynastic_winners = dynastic_winners / total * 100,
         non_dynastic_winners = non_dynastic_winners / total * 100,
         dynastic_winners_vote_diff = dynastic_winners_vote_diff / total * 100,
         non_dynastic_winners_vote_diff = non_dynastic_winners_vote_diff / total * 100)

# Filter out rows with NA values for Education Level
grouped_data_filtered <- grouped_data %>%
  filter(!is.na(MyNeta_education))

# Plot the data
par(mfrow = c(1, 2))

# First graph
custom_palette_four <- c("#BF6556", "#EDB500", "#008571", "#8DBFFF")

grouped_data_filtered %>%
  select(MyNeta_education, dynasts, non_dynasts, winners, losers) %>%
  gather(key = "group", value = "percentage", -MyNeta_education) %>%
  ggplot(aes(x = MyNeta_education, y = percentage, fill = group)) +
  geom_col(position = "dodge", alpha = 0.9) +
  scale_fill_manual(values = custom_palette_four, labels = c("Dynasts", "Non-Dynasts", "Winners", "Losers")) +
  labs(x = NULL, y = "Percentage", title = "Dynasts and Non-Dynasts, Winners and Losers by Education Level", fill = NULL) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_text(
    aes(label = paste0(round(percentage), "%")),
    position = position_dodge(width = 1),
    vjust = -1,
    size = 3,
    color = "black"
  ) +
  theme(legend.position = "bottom", panel.spacing = unit(0.2, "lines"))  # Move the legend to the bottom and adjust panel spacing

# Create dynast_winner variable
data$dynast_winner <- ifelse(data$winner == 1 & data$dynast == 1, 1, 0)

grouped_data_filtered <- data %>%
  filter(!is.na(MyNeta_education))

# Second graph
grouped_data_filtered %>%
  select(MyNeta_education, dynast_winner, vote_share_diff) %>%
  mutate(dynastic_winners = ifelse(dynast_winner == 1, 1, 0),
         non_dynastic_winners = ifelse(dynast_winner == 0, 1, 0),
         dynastic_winners_vote_diff = ifelse(dynast_winner == 1 & vote_share_diff <= 5, 1, 0),
         non_dynastic_winners_vote_diff = ifelse(dynast_winner == 0 & vote_share_diff <= 5, 1, 0)) %>%
  group_by(MyNeta_education) %>%
  dplyr::summarise(dynastic_winners = mean(dynastic_winners),
            non_dynastic_winners = mean(non_dynastic_winners),
            dynastic_winners_vote_diff = mean(dynastic_winners_vote_diff),
            non_dynastic_winners_vote_diff = mean(non_dynastic_winners_vote_diff)) %>%
  pivot_longer(cols = -MyNeta_education, names_to = "group", values_to = "percentage") %>%
  ggplot(aes(x = MyNeta_education, y = percentage, fill = group)) +
  geom_col(position = "dodge", alpha = 0.9) +
  scale_fill_manual(
    values = custom_palette_four,
    labels = c(
      "dynastic_winners" = "Dynastic Winners",
      "non_dynastic_winners" = "Non-Dynastic Winners",
      "dynastic_winners_vote_diff" = "Dynastic Winners (Vote Share Diff ≤ 5)",
      "non_dynastic_winners_vote_diff" = "Non-Dynastic Winners (Vote Share Diff ≤ 5)"
    )
  ) +
  labs(x = NULL, y = "Percentage", title = "Dynastic and Non-Dynastic Winners by Education Level", fill = NULL) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_text(
    aes(label = paste0(round(percentage * 100), "%")),
    position = position_dodge(width = 0.6),
    vjust = -0.5,
    size = 3,
    color = "black"
  ) +
  theme(legend.position = "bottom", panel.spacing = unit(0.2, "lines"))

# Ensure the 'population' column is treated as numeric
data$population <- as.numeric(data$population)
# First, group the data by dynast_winner and summarize by the mean population
data_grouped <- data %>%
  group_by(dynast_winner) %>%
  summarize(average_population = mean(population, na.rm = TRUE))

data_grouped <- data_grouped %>%
  mutate(percentage = average_population / sum(average_population) * 100)

# Now, plot the data
ggplot(data_grouped, aes(x = factor(dynast_winner), y = average_population, fill = factor(dynast_winner))) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = custom_palette, labels = c("Non dynast", "Dynast")) +
  labs(x = "Type of politician in office", y = "Average population", 
       fill = NULL,
       title = "Average district population\nby type of politician in office") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), position = position_stack(vjust = 0.5), color = "white") +
  theme(axis.text.x = element_blank())

# Ensure the 'total_light2004' column is treated as numeric
data$total_light2004 <- as.numeric(data$total_light2004)

# Filter for year 2004
data_2004 <- data %>% filter(year_2004 == 1)

# Group by 'dynast_winner' and calculate average 'total_light2004'
data_grouped <- data_2004 %>%
  group_by(dynast_winner) %>%
  summarize(average_light = mean(total_light_cal2004, na.rm = TRUE))

data_grouped <- data_grouped %>%
  mutate(percentage_light = average_light / sum(average_light) * 100)

ggplot(data_grouped, aes(x = factor(dynast_winner), y = average_light, fill = factor(dynast_winner))) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = custom_palette, labels = c("Non dynast", "Dynast")) +
  labs(x = "Type of politician in office", y = "Average total light", 
       fill = NULL,
       title = "Average total light\nby type of politician in office (2004)") +
  geom_text(aes(label = paste0(round(percentage_light, 1), "%")), position = position_stack(vjust = 0.5), color = "white") +
  theme(axis.text.x = element_blank())

data$total_light_cal2009 <- as.numeric(data$total_light_cal2009)
data_2009 <- data %>% filter(year_2009 == 1)

data_grouped <- data_2009 %>%
  group_by(dynast_winner) %>%
  summarize(average_light = mean(total_light_cal2009, na.rm = TRUE))

data_grouped <- data_grouped %>%
  mutate(percentage_light = average_light / sum(average_light) * 100)

ggplot(data_grouped, aes(x = factor(dynast_winner), y = average_light, fill = factor(dynast_winner))) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = custom_palette, labels = c("Non dynast", "Dynast")) +
  labs(x = "Type of politician in office", y = "Average total light", 
       fill = NULL,
       title = "Average total light\nby type of politician in office (2009)") +
  geom_text(aes(label = paste0(round(percentage_light, 1), "%")), position = position_stack(vjust = 0.5), color = "white") +
  theme(axis.text.x = element_blank())

# Filter for year 2004 and dynast winners with vote share diff up to 10, and dynast losers with vote share diff from -5 to 0
data_2004_dynast_voteshare <- data %>% 
  filter(year_2004 == 1 & ((dynast_winner == 1 & vote_share_diff <= 10) | (dynast_winner == 0 & vote_share_diff >= -10 & vote_share_diff <= 0)))

# Group by 'dynast_winner' and calculate average 'total_light2004'
data_grouped <- data_2004_dynast_voteshare %>%
  group_by(dynast_winner) %>%
  summarize(average_light = mean(total_light_cal2004, na.rm = TRUE))

data_grouped <- data_grouped %>%
  mutate(percentage_light = average_light / sum(average_light) * 100)

ggplot(data_grouped, aes(x = factor(dynast_winner), y = average_light, fill = factor(dynast_winner))) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = custom_palette, labels = c("Non dynast", "Dynast")) +
  geom_text(aes(label = paste0(round(percentage_light, 1), "%")), position = position_stack(vjust = 0.5), color = "white") +
  labs(x = NULL, y = "Average Total Light 2004", 
       fill = "Dynast Winner",
       title = "Average Total Light for Districts by Dynast Winner and Loser with up to 5 Vote Share Difference (2004)") +
  theme(axis.text.x = element_blank())


# Filter for year 2004 and dynast winners with vote share diff up to 5, and dynast losers with vote share diff from -5 to 0
data_2009_dynast_voteshare <- data %>% 
  filter(year_2009 == 1 & ((dynast_winner == 1 & vote_share_diff <= 5) | (dynast_winner == 0 & vote_share_diff >= -5 & vote_share_diff <= 0)))

# Group by 'dynast_winner' and calculate average 'total_light2004'
data_grouped <- data_2009_dynast_voteshare %>%
  group_by(dynast_winner) %>%
  summarize(average_light = mean(total_light2009, na.rm = TRUE))

# Now, plot the data
ggplot(data_grouped, aes(x = factor(dynast_winner), y = average_light, fill = factor(dynast_winner))) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("blue", "red")) +
  labs(x = "Dynast Winner", y = "Average Total Light 2009", 
       fill = "Dynast Winner",
       title = "Average Total Light 2009 for Districts by Dynast Winner and Loser with up to 5 Vote Share Difference") +
  theme_minimal()






       









 