library(readr)
library(readxl)
X18_5_2023_TCPD_GE_All_States_2023_5_18 <- read_excel("18.5.2023 - TCPD_GE_All_States_2023-5-18.xlsx")
dynast_list_wiki_pdf_unclean <- read_excel("dynast_list_wiki_pdf_unclean.xlsx")
dynast_names <- read_excel("dynast_names_matched_fuzzy.xlsx")
# First, let's create the dynast_wiki and dynast_pdf variables

X18_5_2023_TCPD_GE_All_States_2023_5_18$dynast_wiki <- ifelse(
  X18_5_2023_TCPD_GE_All_States_2023_5_18$Candidate %in% dynast_names$Candidate_trivedi_filtere_wiki, 1, 0
)

X18_5_2023_TCPD_GE_All_States_2023_5_18$dynast_pdf <- ifelse(
  X18_5_2023_TCPD_GE_All_States_2023_5_18$Candidate %in% dynast_names$Candidate_trivedi_filtered_pdf, 1, 0
)

# Next, let's create the dynastic_dummy variable
# This variable equals 1 if either dynast_wiki or dynast_pdf equals 1, and 0 otherwise.

X18_5_2023_TCPD_GE_All_States_2023_5_18$dynastic_dummy <- ifelse(
  X18_5_2023_TCPD_GE_All_States_2023_5_18$dynast_wiki == 1 | X18_5_2023_TCPD_GE_All_States_2023_5_18$dynast_pdf == 1, 1, 0
)
# Get the column names
column_names <- names(X18_5_2023_TCPD_GE_All_States_2023_5_18)

# Define the new order
new_order <- c("State_Name", "Assembly_No", "Constituency_No", "Year", "month", "Poll_No", "DelimID", 
               "Position", "Candidate", "dynastic_dummy")

# Find out which column names were not included in new_order
remaining_columns <- column_names[!column_names %in% new_order]

# Create the final column order
final_order <- c(new_order, remaining_columns)

# Rearrange the columns
X18_5_2023_TCPD_GE_All_States_2023_5_18 <- X18_5_2023_TCPD_GE_All_States_2023_5_18[, final_order]
dynastic_counts_by_year <- X18_5_2023_TCPD_GE_All_States_2023_5_18 %>%
  group_by(Year) %>%
  summarise(count = sum(dynastic_dummy == 1, na.rm = TRUE))

print(dynastic_counts_by_year)

## Adding the rural and urban binary and percentages 
rural_urban <- read_excel("Rural Population India Districrs .xlsx")
matched_rural_urban <- read_csv("rural_urban_matches_with_persons.csv")
# convert district column to lower case
matched_rural_urban$District <- tolower(matched_rural_urban$District)
X18_5_2023_TCPD_GE_All_States_2023_5_18$district <- tolower(X18_5_2023_TCPD_GE_All_States_2023_5_18$district)

# merge the datasets
merged_data <- merge(matched_rural_urban, X18_5_2023_TCPD_GE_All_States_2023_5_18, by.x = "District", by.y = "district")

# check if the merge was successful
str(merged_data)


# create a new dataframe
district_rural_urban_population_percentage <- merged_data %>%
  select(district = District, total_population = population, persons_rural = persons) %>%
  mutate(percent_rural = ifelse(persons_rural == 0, 0, (persons_rural / total_population) * 100),
         percent_urban = 100 - percent_rural,
         rural_urban = ifelse(percent_rural > 65, "RURAL", "URBAN"),
         rural_urban_binary = ifelse(rural_urban == "RURAL", 0, 1))

# display the new dataframe
head(district_rural_urban_population_percentage)

# Remove duplicate districts, keep the first instance
district_rural_urban_population_percentage <- district_rural_urban_population_percentage %>%
  distinct(district, .keep_all = TRUE)

write.csv(district_rural_urban_population_percentage, "district_rural_urban_population_percentage.csv", row.names = FALSE)













