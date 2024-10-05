# Aim: to create a dataset that shows, per constituency, the votes for each party in each UK general election
# from 1918 to 2024. It should be in tidy, long, form - 1 row per election per party per constituency.
# See usage notes on accompanying blog post.

# First we'll deal with the historical data from 1918-2019
# https://commonslibrary.parliament.uk/research-briefings/cbp-8647/

library(tidyverse)
library(readxl)
library(tidyverse)
library(janitor)
library(scales)
library(assertr)

# First we need to import the historic Excel data
# It comes in an Excel file with 1 sheet per election.

historic_results_file <- "1918-2019election_results_by_pcon.xlsx"

# Get the names of the sheets in that file

historic_sheet_names <- excel_sheets(historic_results_file)

# The first sheet concerns university seats. I'm not interested in those at the moment

historic_sheet_names <- historic_sheet_names[historic_sheet_names != "University Seats"]

# Now, we want to iterate through each sheet name and process the relevant parts of the results into a nice tidy dataframe

# Initialise a data frame to store the final results
historic_election_results <- data.frame()

# Now iterate through each sheet name

for (sheet in historic_sheet_names)
{
  # Which sheet are we working on?
  print(sheet)

  # Get the contents of the current sheet
  election_results <- read_excel(historic_results_file, sheet = sheet, skip = 1)

  # Initialise a variable we can manipulate to get a clean dataset to match the data we just read in
  # (not strictly necessary but it also does no harm - might make debugging any problems easier)

  election_results_clean <- election_results

  # Use the first row as column names, where a value exists

  election_results_clean <- row_to_names(election_results, row_number = 1, remove_row = TRUE) |>
    clean_names()

  # remove cols that are all NA

  election_results_clean <- election_results_clean |>
    select_if(~ !all(is.na(.)))

  # Remove cols where the first value is vote share.
  # We can calculate this from the rest of the data if we need it later

  election_results_clean <- election_results_clean[, !unlist(election_results_clean[1, ]) %in% c("Vote share", "Votes Share")]

  # Now if the name of the field is "na" starts with na_ we rename it to the value in the first row.
  # We can't just look for "starts with na" because sometimes legitimate party names start with those letters.

  # Loop through the columns
  for (i in seq_along(election_results_clean)) {
    # Check if the column name starts with "na_" or is exactly "na"
    if (startsWith(colnames(election_results_clean)[i], "na_") | colnames(election_results_clean)[i] == "na") {
      # Set the column name to the first value in the column
      colnames(election_results_clean)[i] <- election_results_clean[1, i]
    }
  }

  # Ensure the column names are 'safe' per R's rules

  election_results_clean <- clean_names(election_results_clean)

  # Remove first row now we extracted it into our column names

  election_results_clean <- election_results_clean[-1, ]

  # Remove any rows with no constituency name. These are usually blank rows or interpretation notes

  election_results_clean <- filter(election_results_clean, !is.na(constituency))

  # Any column names that at this stage still start with "na_" or are exactly "na" are random notes or other such data we'll not keep in our dataset

  election_results_clean <- select(election_results_clean, -starts_with("na_"), -any_of(c("na")))

  # Now we pivot it to a long version where the per-party results are transformed from columns into rows
  # And we move any rows with null votes - these will be constituencies where the relevant party didn't field a candidate.

  # Some sheets contain different field names to other in terms of the columns that don't relate to a party. We'll drop those, but note that not all sheets contain all fields so we use "any_of" to avoid column not found errors.

  election_results_clean_long <- election_results_clean |>
    pivot_longer(
      cols = -any_of(c("id", "constituency", "county", "country", "country_region", "seats", "electorate", "total_votes", "turnout", "ons_id")), # this are the columns that don't relate to party - not all of them appear in each year's sheet
      names_to = "party",
      values_to = "votes"
    ) |>
    filter(!is.na(votes))

  # Add a field to keep track of which election these results are from. This is based on the sheet name we read in.

  election_results_clean_long <- election_results_clean_long |>
    mutate(election = sheet)

  # Now we can append our long, clean results from this sheet to the final dataset

  historic_election_results <- bind_rows(
    historic_election_results,
    election_results_clean_long
  )
}

# Check only legitimate party names made the cut

select(historic_election_results, party) |>
  distinct()

# Some entries read liberal_democrat, others read liberal_democrats
# Make them all read the latter

historic_election_results <- historic_election_results |>
  mutate(party = if_else(party == "liberal_democrat", "liberal_democrats", party))

# Now we need to import the 2024 results

election_results_2024 <- read_excel("HoC-GE2024-results-by-constituency.xlsx", sheet = "Data", skip = 2) |>
  clean_names()

# The columns are named differently to the dataframe that we've been constructing so far, and there are more of them.
# Rename and preserve only the columns in common
# Constituencies were in capital letters in the historic file. Transform these similarly.

election_results_2024 <- election_results_2024 |>
  select(ons_id,
    constituency = constituency_name,
    country_region = region_name,
    country = country_name,
    electorate,
    total_votes = valid_votes,
    con:all_other_candidates
  ) |>
  mutate(
    election = "2024",
    constituency = toupper(constituency)
  )

# Now we need to convert the 2024 results to a similar style long table as we have with the historic results

election_results_2024_long <- election_results_2024 |>
  pivot_longer(
    cols = (con:all_other_candidates),
    names_to = "party",
    values_to = "votes"
  ) |>
  filter(!is.na(votes))

# How the party names are represented in the 2024 file is different to the older data.
# We'll recode the 2024 results accordingly.

# First, what are the entries in each case?

parties_2024 <- select(election_results_2024_long, party) |>
  distinct() |>
  arrange(party)
parties_historic <- select(historic_election_results, party) |>
  distinct() |>
  arrange(party)

# It seems like there may be other discrepancies even within the historic dataset itself, although some might be legitimately caused by changes to the parties over time.
# If we wanted to analyse at a party level, especially for the smaller parties, we should revisit this.
# For now though, we recode 2024 values as follows:

election_results_2024_long$party <- election_results_2024_long$party |>
  fct_recode(
    "other" = "all_other_candidates",
    "alliance" = "apni",
    "conservative" = "con",
    "labour" = "lab",
    "liberal_democrats" = "ld",
    "plaid_cymru" = "pc",
    "reform_uk" = "ruk",
    "sinn_fein" = "sf"
  )

# Now we can add the 2024 data to the historic data frame.
# We'll drop some of the columns we had in the historic data as they weren't present in the 2024 results
# and reorder the columns to make it a little more intuitive

consolidated_election_results_long <- bind_rows(
  select(historic_election_results, -c(county, turnout)) |>
    mutate(
      electorate = as.numeric(electorate),
      total_votes = as.numeric(total_votes),
      votes = as.numeric(votes)
    ),
  election_results_2024_long
) |>
  select(election, id, ons_id, constituency, country_region, country, electorate, total_votes, party, votes) |>
  mutate(share_of_vote = votes / total_votes)


# Now let's sense check the results by comparing some of the totals to those published by official sources.

# How many constituencies do we have data for in 2024?

filter(consolidated_election_results_long, election == "2024") |>
  summarise(n_distinct(constituency))

# 650 constituencies. That's a good sign.

# Check 2024 vote totals vs https://electionresults.parliament.uk/general-elections/6

filter(consolidated_election_results_long, election == "2024") |>
  group_by(party) |>
  summarise(count_of_votes_for_party = sum(votes)) |>
  arrange(desc(count_of_votes_for_party)) |>
  ggplot(aes(y = fct_reorder(party, count_of_votes_for_party), x = count_of_votes_for_party)) +
  geom_col() +
  theme_minimal() +
  labs(
    title = "2024 General Election Results",
    x = "Votes",
    y = "Party"
  ) +
  # use SI units for x axis
  scale_x_continuous(labels = scales::label_number(scale_cut = scales::cut_short_scale())) +
  # add data labels
  geom_text(aes(label = count_of_votes_for_party), hjust = -0.1) +
  expand_limits(x = 11000000)

# That looks good


# Check 2019 vote totals vs https://electionresults.parliament.uk/general-elections/4

filter(consolidated_election_results_long, election == "2019") |>
  group_by(party) |>
  summarise(count_of_votes_for_party = sum(votes)) |>
  arrange(desc(count_of_votes_for_party)) |>
  ggplot(aes(y = fct_reorder(party, count_of_votes_for_party), x = count_of_votes_for_party)) +
  geom_col() +
  theme_minimal() +
  labs(
    title = "2019 General Election Results",
    x = "Votes",
    y = "Party"
  ) +
  # use SI units for x axis
  scale_x_continuous(labels = scales::label_number(scale_cut = scales::cut_short_scale())) +
  # add data labels
  geom_text(aes(label = count_of_votes_for_party), hjust = -0.1) +
  expand_limits(x = 16000000)

# Also good

# Finally lets look at turnout vs https://electionresults.parliament.uk/general-elections/6/turnout
# and https://electionresults.parliament.uk/general-elections/4/turnout

filter(consolidated_election_results_long, election %in% c("2024", "2019")) |>
  select(election, constituency, electorate, total_votes) |>
  distinct() |>
  group_by(election) |>
  summarise(
    total_electorate = sum(electorate),
    total_votes = sum(total_votes)
  ) |>
  mutate(turnout = total_votes / total_electorate)

# Check that the grain of the data is correct - 1 row per election per constituency per party

assert_rows(consolidated_election_results_long, col_concat, is_uniq, c(election, constituency, party))

# Finally, write out the file as a CSV

write_csv(consolidated_election_results_long, "uk_general_election_votes_1918_2024.csv")
