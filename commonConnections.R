library(ggplot2)
library(tidyverse)
library(nbastatR)
library(devtools)
library(ncaahoopR)
library(extrafont)
library(cowplot)
library(scales)

#data(ids)

# Get Schedule w/ game IDS
sju_schedule <- get_schedule("St John's", season = "2023-24")

game_ids <- sju_schedule$game_id[1:7]

# Grab all shots from Cuse games 
sju_shots <- get_pbp_game(game_ids)

# Assuming sju_shots is your data frame
assisted_shots <- sju_shots %>%
  filter(shot_team == "St. John's", shot_outcome == "made", !is.na(assist))

assist_shooter_combinations <- assisted_shots %>%
  group_by(assist, shooter) %>%
  summarize(count = n(), .groups = 'drop') %>%
  arrange(desc(count))

top_10_combinations <- assist_shooter_combinations %>%
  slice(1:10)

ncaa_logo_url <- "http://web2.ncaa.org/ncaa_style/img/All_Logos/sm/603.gif"

title_with_logo <- htmltools::tags$div(
  style = "display: flex; align-items: center;",
  htmltools::tags$img(src = ncaa_logo_url, style = "height: 40px; margin-right: 10px;"),
  htmltools::tags$div(
    style = "line-height: 1.1;",
    htmltools::tags$h2("SJUBB 2023-24 Statistics", style = "margin: 0; font-size: 28px;"),  # Adjust font-size as needed
    htmltools::tags$h3("Common connections", style = "margin: 0; font-size: 12px;")   # Adjust font-size as needed
  )
)

my_palette <- scales::col_numeric(
  palette = "Reds",
  domain = c(min(top_10_with_photos$count), max(top_10_with_photos$count))
)

player_photos <- read.csv("/Users/Mason/Desktop/testingdata/Sjubb 2023 Photos - Sheet1.csv")

# Merge the dataframes
top_10_with_photos <- top_10_combinations %>%
  left_join(player_photos, by = c("assist" = "Player")) %>%
  rename(assist_photo = Photo) %>%
  left_join(player_photos, by = c("shooter" = "Player")) %>%
  rename(shooter_photo = Photo)

top_10_with_photos <- top_10_with_photos %>%
  select(assist_photo, assist, shooter_photo, shooter, count)

top_10_with_photos <- top_10_with_photos %>%
  mutate(shooter = ifelse(shooter == "Glenn Taylor Jr.", "Glenn Taylor Jr", shooter))

# Create a gt table and incorporate logos
gt_table <- gt(top_10_with_photos) %>%
  text_transform(
    locations = cells_body(columns = c(assist_photo)),
    fn = function(x) {
      map(x, ~htmltools::tags$img(src = ., height = 30))
    }
  ) %>%
  text_transform(
    locations = cells_body(columns = c(shooter_photo)),
    fn = function(x) {
      map(x, ~htmltools::tags$img(src = ., height = 30))
    }
  ) %>%
  # Setting column labels
  cols_label(
    assist_photo = "",
    assist = "Assist",
    shooter_photo = "",
    shooter = "Shooter",
    count = "Count"
  ) %>%
  cols_align(
    align = "center",
    columns = everything()
  ) %>%
  tab_header(
    title = htmltools::as.tags(title_with_logo)
  ) %>%
  tab_footnote(
    md("Source: ncaahoopR | Mason Wood")
  ) %>%
  data_color(
    columns = c(count),
    colors = function(x) {
      my_palette(x)
    }
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
