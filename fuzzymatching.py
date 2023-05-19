pip install fuzzywuzzy
pip install python-Levenshtein
import pandas as pd
from fuzzywuzzy import fuzz, process
dynast_list = pd.read_excel('dynast_list_wiki_pdf_unclean.xlsx')
# Define the matching function using token_sort_ratio
def name_match(name, choices, scorer=fuzz.token_sort_ratio):
    return process.extractOne(name, choices, scorer=scorer)  # Return the match and the score
# Initialize an empty list to hold match results
matches = []
# Loop over all wiki_names
for wiki_name in dynast_list['wiki_names']:
    # Find the best match among Candidate_trivedi_filtered
    match = name_match(wiki_name, dynast_list['Candidate_trivedi_filtered'])

    # Check if the match score is above the threshold
    if match[1] >= 70:
        matches.append({
            'wiki_names': wiki_name,
            'Candidate_trivedi_filtered': match[0],
            'score': match[1]
        })
# Convert matches list to a DataFrame
matches_df = pd.DataFrame(matches)
matches_df.head(20)
# Save DataFrame to a CSV file
matches_df.to_csv('name_matches.csv', index=False)
