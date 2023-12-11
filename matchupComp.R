library(dplyr)
library(rvest)
library(httr)
library(stringr)
library(dplyr)
library(gt)
library(gtExtras)
library(scales)
library(tidyverse)

away_url <- "https://stats.ncaa.org/teams/560695"

# Fetching the webpage
away_webpage <- httr::GET(away_url, user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.110 Safari/537.3"))
away_html_data <- rvest::read_html(away_webpage)

away_tables <- away_html_data %>% html_nodes("table")
away_ncaa_table <- away_tables %>% .[[3]] %>% html_table(fill = TRUE)

home_url <- "https://stats.ncaa.org/teams/561074"

home_webpage <- httr::GET(home_url, user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.110 Safari/537.3"))
home_html_data <- rvest::read_html(home_webpage)

home_tables <- home_html_data %>% html_nodes("table")
home_ncaa_table <- home_tables %>% .[[3]] %>% html_table(fill = TRUE)

away_ncaa_table <- away_ncaa_table %>% dplyr::rename(Stat = X1, Away_Rank = X2)
home_ncaa_table <- home_ncaa_table %>% dplyr::rename(Stat = X1, Home_Rank = X2)

away_ncaa_table_selected <- away_ncaa_table %>% dplyr::select(Stat, Away_Rank)
home_ncaa_table_selected <- home_ncaa_table %>% dplyr::select(Stat, Home_Rank)

# Join the data frames by the Stat column using dplyr's left_join function explicitly
comparison_table <- dplyr::left_join(away_ncaa_table_selected, home_ncaa_table_selected, by = "Stat")

comparison_table <- comparison_table %>%
  mutate(
    Away_Rank = str_remove(Away_Rank, "T-"),
    Home_Rank = str_remove(Home_Rank, "T-")
  )

comparison_table <- comparison_table %>% 
  slice(-1, -2, -n()) 

#------------------------------------------------------------------------------------------------

url <- "https://kenpom.com/"
webpage <- httr::GET(url, user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.110 Safari/537.3"))
html_data <- rvest::read_html(webpage)

# Extracting tables
tables <- html_data %>% html_nodes("table")

# Extracting the first table
kenpom_table <- tables %>% 
  .[[1]] %>% 
  html_table(fill = TRUE)

col_names <- as.character(unlist(kenpom_table[1, ]))

# Make the names unique
unique_col_names <- make.unique(col_names)

kenpom_table <- kenpom_table[-1, ]
colnames(kenpom_table) <- unique_col_names

# Convert the data to a tibble
kenpom_table <- as_tibble(kenpom_table, .name_repair = "unique")

filtered_kenpom_table <- kenpom_table %>% 
  filter(Team == "Boston College" | Team == "St. John's")

# May have to be toggled depending on home/away + stat or swap with asc below
#filtered_kenpom_table <- filtered_kenpom_table %>%
  #arrange(desc(AdjT))

filtered_kenpom_table <- filtered_kenpom_table %>%
  arrange(AdjT)

original_col_names <- colnames(filtered_kenpom_table)

# Transpose the filtered table
transposed_table <- t(filtered_kenpom_table)

transposed_table_df <- as.data.frame(transposed_table)

transposed_table_df <- cbind(Statistic = original_col_names[-1], transposed_table_df[-1, ])

colnames(transposed_table_df)[-1] <- transposed_table_df[1, -1]

transposed_table_df <- transposed_table_df[-1, ]

rownames(transposed_table_df) <- NULL

filtered_transposed_table_df <- transposed_table_df %>%
  filter(Statistic %in% c("Conf", "W-L", "AdjO.1", "AdjD.1", "AdjT.1", "Luck.1"))

#------------------------------------------------------------------------------------------

colnames(comparison_table) <- paste0("Col", 1:ncol(comparison_table))
colnames(filtered_transposed_table_df) <- paste0("Col", 1:ncol(filtered_transposed_table_df))

combined_table <- bind_rows(comparison_table, filtered_transposed_table_df)

combined_table <- combined_table %>%
  rename(
    Statistic = Col1,
    Away = Col2,
    Home = Col3
  )

combined_table$Away <- as.numeric(as.character(combined_table$Away))
combined_table$Home <- as.numeric(as.character(combined_table$Home))

combined_table$Away[is.na(combined_table$Away)] <- 0
combined_table$Home[is.na(combined_table$Home)] <- 0

combined_table <- combined_table %>%
  mutate(
    AwayPercentage = 100 - round((Away / 362) * 100, digits = 0),
    AwayRanking = paste(AwayPercentage, " (", Away, "/362)", sep = ""),  # Removed the "%" symbol here
    HomePercentage = 100 - round((Home / 362) * 100, digits = 0),
    HomeRanking = paste(HomePercentage, " (", Home, "/362)", sep = "")  # Removed the "%" symbol here
  )


combined_table <- combined_table %>%
  filter(!(Statistic %in% c("Scoring Offense", "Scoring Defense", "Winning Percentage", "Field Goal Percentage Defense", "Three Point Percentage Defense", "W-L", "Conf", "Fouls Per Game", "Betting line", "Kenpom Rank")))

#------------------------------------------------------------------------------------------

combined_table <- combined_table %>%
  dplyr::select(AwayRanking, Statistic, HomeRanking, everything()) %>%
  dplyr::select(-Away, -Home, -AwayPercentage, -HomePercentage) %>%
  add_row(Statistic = "Record", AwayRanking = "7-3", HomeRanking = "6-2", .before = 1) %>%
  add_row(Statistic = "Kenpom Ranking", AwayRanking = "98", HomeRanking = "55", .before = 1) %>%
  mutate(Statistic = factor(Statistic, levels = c("Record", "Kenpom Ranking", "Scoring Margin", "AdjO.1", "AdjD.1", "AdjT.1", "Luck.1", "Assist/Turnover Ratio", "Turnovers Forced Per Game", "Rebound Margin", "Rebounds Per Game", "Rebounds (Offensive) Per Game", "Rebounds (Defensive) Per Game", "Effective FG pct", "Field Goal Percentage", "Free Throw Percentage", "Free Throw Attempts Per Game", "Free Throws Made Per Game", "Three Point Percentage", "Three Point Attempts Per Game", "Three Pointers Per Game"))) %>%
  mutate(Statistic = fct_recode(Statistic,
                                "Adjusted Offensive Efficiency" = "AdjO.1",
                                "Adjusted Defensive Efficiency" = "AdjD.1",
                                "Adjusted Tempo" = "AdjT.1",
                                "Luck" = "Luck.1"
  )) %>%
  arrange(Statistic)

away_label <- "<img src='https://upload.wikimedia.org/wikipedia/en/thumb/9/96/Boston_College_Eagles_logo.svg/1200px-Boston_College_Eagles_logo.svg.png' style='height:50px;' />"
home_label <- "<img src='https://upload.wikimedia.org/wikipedia/commons/9/9e/St._John%27s_Athletics_logo.png' style='height:50px;' />"
middle_label <- "<img src= 'https://nabc.com/app/uploads/2023/09/NABC-Brooklyn-Showcase-PNG.png' style='height:50px;' />"

combined_table <- combined_table %>%
  mutate(
    AwayRanking = sub("(\\d+) (.+)", "\\1<br><span style='font-size: 10px;'>\\2</span>", AwayRanking),
    HomeRanking = sub("(\\d+) (.+)", "\\1<br><span style='font-size: 10px;'>\\2</span>", HomeRanking)
  )

get_color <- function(value) {
  col_numeric(palette = c("#D40000", "white", "#00B400"), domain = c(0, 100))(value)
}

apply_color_to_cell <- function(html_string) {
  numeric_value <- as.numeric(sub("<.*", "", html_string))
  color <- get_color(numeric_value)
  new_html_string <- sprintf("<div style='background-color:%s; padding: 5px;'>%s</div>", color, html_string)
  new_html_string
}

combined_table <- combined_table %>%
  mutate(
    AwayRanking = ifelse(
      Statistic %in% c("Scoring Margin", "Adjusted Offensive Efficiency", "Adjusted Defensive Efficiency", "Adjusted Tempo", "Luck", "Assist/Turnover Ratio", "Turnovers Forced Per Game", "Rebound Margin", "Rebounds Per Game", "Rebounds (Offensive) Per Game", "Rebounds (Defensive) Per Game", "Effective FG pct", "Field Goal Percentage", "Free Throw Percentage", "Free Throw Attempts Per Game", "Free Throws Made Per Game", "Three Point Percentage", "Three Point Attempts Per Game", "Three Pointers Per Game"), 
      mapply(apply_color_to_cell, AwayRanking), 
      AwayRanking
    ),
    HomeRanking = ifelse(
      Statistic %in% c("Scoring Margin", "Adjusted Offensive Efficiency", "Adjusted Defensive Efficiency", "Adjusted Tempo", "Luck", "Assist/Turnover Ratio", "Turnovers Forced Per Game", "Rebound Margin", "Rebounds Per Game", "Rebounds (Offensive) Per Game", "Rebounds (Defensive) Per Game", "Effective FG pct", "Field Goal Percentage", "Free Throw Percentage", "Free Throw Attempts Per Game", "Free Throws Made Per Game", "Three Point Percentage", "Three Point Attempts Per Game", "Three Pointers Per Game"), 
      mapply(apply_color_to_cell, HomeRanking), 
      HomeRanking
    )
  )

gt_table <- gt(combined_table) %>%
  tab_header(
    title = md("**Boston College vs St. John's 12/10/23**"),
    subtitle = md("D1 Percentile Rankings")
  ) %>%
  cols_label(
    AwayRanking = html(away_label),
    HomeRanking = html(home_label),
    Statistic = html(middle_label)
    #Statistic = ""
  ) %>%
  cols_align(
    align = "center",
    columns = everything()
  ) %>%
  tab_footnote(
    footnote = md("Mason Wood | Twitter: @Mason25W")
  ) %>%
  tab_options(
    table.border.top.color = "#D3D3D3",
    table.border.bottom.color = "#D3D3D3",
    table.border.left.color = "#D3D3D3",
    table.border.right.color = "#D3D3D3",
    heading.title.font.size = px(20),
    heading.subtitle.font.size = px(16)
  ) %>%
  opt_table_outline(style = "solid", color = "#D3D3D3") %>%
  fmt_markdown(columns = c(AwayRanking, HomeRanking))

# Print the gt table
print(gt_table)

gtsave(gt_table, "/Users/Mason/Desktop/rimages/matchup.png", expand = 10)