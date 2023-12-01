library(dplyr)
library(rvest)
library(httr)

# Define the URL
away_url <- "https://stats.ncaa.org/teams/561074"

# Fetching the webpage
away_webpage <- httr::GET(away_url, user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.110 Safari/537.3"))
away_html_data <- rvest::read_html(away_webpage)

# Extracting tables and converting to dataframe
away_tables <- away_html_data %>% html_nodes("table")
away_ncaa_table <- away_tables %>% .[[3]] %>% html_table(fill = TRUE)

# Define the URL
home_url <- "https://stats.ncaa.org/teams/561232"

# Fetching the webpage
home_webpage <- httr::GET(home_url, user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.110 Safari/537.3"))
home_html_data <- rvest::read_html(home_webpage)

# Extracting tables and converting to dataframe
home_tables <- home_html_data %>% html_nodes("table")
home_ncaa_table <- home_tables %>% .[[3]] %>% html_table(fill = TRUE)

away_ncaa_table <- away_ncaa_table %>% dplyr::rename(Stat = X1, Away_Rank = X2)
home_ncaa_table <- home_ncaa_table %>% dplyr::rename(Stat = X1, Home_Rank = X2)

# Select only the columns you want to compare, using dplyr's select function explicitly
away_ncaa_table_selected <- away_ncaa_table %>% dplyr::select(Stat, Away_Rank)
home_ncaa_table_selected <- home_ncaa_table %>% dplyr::select(Stat, Home_Rank)

# Join the data frames by the Stat column using dplyr's left_join function explicitly
comparison_table <- dplyr::left_join(away_ncaa_table_selected, home_ncaa_table_selected, by = "Stat")

