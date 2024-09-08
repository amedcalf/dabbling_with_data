# TODO: Add other years from file
# Todo add 2024


# First we'll deal with the historical data from 1918-2019
# https://commonslibrary.parliament.uk/research-briefings/cbp-8647/

# Excel verison has more details

library(tidyverse)
library(readxl)
library(tidyverse)
library(janitor)

election_results <- read_excel("1918-2019election_results_by_pcon.xlsx", sheet = "2019", skip = 1)

# First use the first row as column names if there are any

election_results_clean <- election_results
election_results_clean <- row_to_names(election_results, row_number = 1, remove_row = TRUE) |>
  clean_names()

# remove cols that are all NA

election_results_clean <- election_results_clean |>
  select_if(~ !all(is.na(.)))

# Remove cols where the first value is vote share.
# We can calculate this from the rest of the data if we need it later

election_results_clean <- election_results_clean[, !unlist(election_results_clean[1, ]) %in% c("Vote share", "Votes Share")]

# Now if the name of the field starts with na we make it the first row value:

# Loop through the columns
for (i in seq_along(election_results_clean)) {
  # Check if the column name starts with "na"
  if (startsWith(colnames(election_results_clean)[i], "na")) {
    # Set the column name to the first value in the column
    colnames(election_results_clean)[i] <- election_results_clean[1, i]
  }
}

# Ensure the column names are 'safe' per R's rules

election_results_clean <- clean_names(election_results_clean)

# Remove first row now we extracted it as column names

election_results_clean <- election_results_clean[-1, ]


# Now we pivot it to a long version where the per-party results are transformed from columns into rows
# And we move any rows with null votes - these will be constituencies where the relevant party didn't field a candidate.

election_results_clean_long <- election_results_clean |>
  pivot_longer(cols = 7:19, names_to = "party", values_to = "votes") |>
  filter(!is.na(votes))
