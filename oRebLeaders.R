library(dplyr)
library(rvest)
library(gt)
library(gtExtras)
library(httr)
library(purrr)
library(stringr)
library(htmltools)

# Fetching the webpage
url <- "https://stats.ncaa.org/rankings/MBB/2024/1/857/22"
webpage <- httr::GET(url, user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.110 Safari/537.3"))
html_data <- rvest::read_html(webpage)

# Extracting tables and converting to dataframe
tables <- html_data %>% html_nodes("table")
leaderboard_table <- tables %>% html_table(fill = TRUE) %>% .[[2]]

# Filter to only include the top 10 rows
top_10_table <- leaderboard_table %>% slice(1:10)

csv_path <- "/Users/Mason/Desktop/testingdata/IDs2020D1.csv"
school_ids <- read.csv(csv_path)

# Clean and standardize team names in both dataframes
top_10_table <- top_10_table %>%
  mutate(Team = str_remove(Team, " \\(.*\\)"))  # Removes anything in parentheses and the parentheses themselves

school_ids <- school_ids %>%
  mutate(school = str_remove(school, " \\(.*\\)"))  # Same cleaning for school_ids

# Match team names with school IDs and create logo URLs
top_10_table <- top_10_table %>%
  left_join(school_ids, by = c("Team" = "school")) %>%
  mutate(logo_url = ifelse(is.na(school_id), NA, paste0("http://web2.ncaa.org/ncaa_style/img/All_Logos/sm/", school_id, ".gif"))) %>%
  select(-school_id)  # remove the school_id column if not needed

top_10_table$logo_url[6] <- 'http://web2.ncaa.org/ncaa_style/img/All_Logos/sm/650.gif'

ncaa_logo_url <- "https://stats.ncaa.org/assets/main_logo.17e742a1-fbc4d0406ae731b47babcc3fca53775b5955ef372bf8b8e998ca9a513e468b8f.svg"

title_with_logo <- htmltools::tags$div(
  style = "display: flex; align-items: center;",
  htmltools::tags$img(src = ncaa_logo_url, style = "height: 40px; margin-right: 10px;"),
  htmltools::tags$div(
    style = "line-height: 1.1;",
    htmltools::tags$h2("NCAA Basketball Team Statistics", style = "margin: 0; font-size: 28px;"),  # Adjust font-size as needed
    htmltools::tags$h3("Rebounds (Offensive) Per Game", style = "margin: 0; font-size: 12px;")   # Adjust font-size as needed
  )
)

selected_columns_table <- top_10_table %>%
  select(Rank, logo_url, Team, ORebs, RPG)

# Create a gt table and incorporate logos
gt_table <- gt(selected_columns_table) %>%
  # Setting column labels
  cols_label(
    Rank = "Rank",
    logo_url = "",
    Team = "Team",
    ORebs = "Offensive Rebounds",
    RPG = "Rebounds per Game"
  ) %>%
  # Add logos to the table
  text_transform(
    locations = cells_body(columns = c("logo_url")),
    fn = function(x) {
      map_chr(x, ~ifelse(is.na(.x), NA_character_, as.character(htmltools::img(src = .x, height = 25))))
    }
  ) %>%
  # Formatting data
  fmt_number(
    columns = c("Rank", "ORebs", "RPG"),
    decimals = 0
  ) %>%
  # Styling rows and columns
  cols_align(
    align = "center",
    columns = everything()
  ) %>%
  tab_header(
    title = htmltools::as.tags(title_with_logo)
  ) %>%
  tab_footnote(
    md("Source: stats.ncaa.org | Mason Wood")
  ) %>%
  # Add a border around the table
  tab_options(
    table.border.top.color = "#D3D3D3",
    table.border.bottom.color = "#D3D3D3",
    table.border.left.color = "#D3D3D3",
    table.border.right.color = "#D3D3D3"
  ) %>%
  # Style the table outline
  opt_table_outline(style = "solid", color = "#D3D3D3")

# Display the gt table
gt_table
