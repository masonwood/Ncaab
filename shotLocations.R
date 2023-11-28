library(ggplot2)
library(tidyverse)
library(nbastatR)
library(devtools)
library(ncaahoopR)
library(extrafont)
library(cowplot)
library(scales)
library(gt)
library(gtExtras)

#data(ids)

# Get Schedule w/ game IDS
sju_schedule <- get_schedule("St John's", season = "2023-24")

game_ids <- sju_schedule$game_id[1:7]

# Grab all shots from Cuse games 
sju_shots <- get_pbp_game(game_ids)

filtered_sju_shots_made <- sju_shots %>%
  filter(shot_team == "St. John's", shot_outcome == "made")

filtered_sju_shots_made_clean <- filtered_sju_shots_made %>%
  mutate(description = str_extract(description, ".*made\\s\\w+")) %>%
  group_by(description) %>%
  summarize(quantity = n(), .groups = 'drop') %>%
  arrange(desc(quantity))

df_wide <- filtered_sju_shots_made_clean %>%
  separate(description, into = c("Player", "ShotType"), sep = " made ") %>%
  group_by(Player, ShotType) %>%
  summarise(quantity = sum(quantity), .groups = 'drop') %>%
  pivot_wider(names_from = ShotType, values_from = quantity, values_fill = list(quantity = 0)) %>%
  mutate(InsideScore = Layup + Dunk) %>% # Create a new column for the sum of Layup and Dunk
  select(-Free) # Remove the Free column

df_wide <- df_wide %>%
  mutate(Total = InsideScore + Jumper + Three)

# Sort by the Total column
df_sorted <- df_wide %>%
  arrange(desc(Total))

player_photos <- read.csv("/Users/Mason/Desktop/testingdata/Sjubb 2023 Photos - Sheet1.csv")

df_final <- df_sorted %>%
  left_join(player_photos, by = "Player")

#df_final <- df_final %>%
  #mutate(Photo = ifelse(is.na(Photo), "", sprintf("<img src='%s' style='height: 30px;'/>", Photo)))

# Now create df_list without the Total column
df_list <- df_final %>%
  transmute(
    Photo,
    Player, 
    ShotList = pmap(list(InsideScore, Jumper, Three), ~replace(c(...), c(...) == 0, NA))
  )

ncaa_logo_url <- "/Users/Mason/Desktop/testingdata/sjulogo.gif"

title_with_logo <- htmltools::tags$div(
  style = "display: flex; align-items: center;",
  htmltools::tags$img(src = ncaa_logo_url, style = "height: 40px; margin-right: 10px;"),
  htmltools::tags$div(
    style = "line-height: 1.1;",
    htmltools::tags$h2("SJUBB 2023-24 Statistics", style = "margin: 0; font-size: 28px;"),  # Adjust font-size as needed
    htmltools::tags$h3("Made Shot Breakdown", style = "margin: 0; font-size: 12px;")   # Adjust font-size as needed
  )
)

shotLocationtable <- df_list %>%
  gt() %>%
  gt_plt_bar_stack(
    column = ShotList, 
    width = 75,
    labels = c("Layups/Dunks", "Mid-range", "Threes"),
    palette = c("#BA0C2F", "#A7A8AA", "#041C2C")
  ) %>%
  text_transform(
    locations = cells_body(columns = c("Photo")),
    fn = function(x) {
      map(x, ~htmltools::tags$img(src = ., height = 30))
    }
  ) %>%
  cols_align(
    align = "center",
    columns = everything()
  ) %>%
  cols_label(
    Photo = ""  # Set the label of the Photo column to an empty string
  ) %>%
  tab_header(
    title = htmltools::as.tags(title_with_logo)
  ) %>%
  tab_footnote(
    md("Source: ncaahoopR | Mason Wood")
  ) %>%
  tab_options(
    table.border.top.color = "#D3D3D3",
    table.border.bottom.color = "#D3D3D3",
    table.border.left.color = "#D3D3D3",
    table.border.right.color = "#D3D3D3"
  ) %>%
  opt_table_outline(style = "solid", color = "#D3D3D3")

# Display the table
shotLocationtable

gtsave(shotLocationtable, "/Users/Mason/Desktop/rimages/tab_1.png", expand = 10)