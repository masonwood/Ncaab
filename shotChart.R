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
library(tibble)
library(png)
library(grid)
library(gridExtra)

#data(ids)

# Get Schedule w/ game IDS
schedule <- get_schedule("UConn", season = "2023-24")

game_ids <- schedule$game_id[1:8]

# Grab all shots from Cuse games 
shots <- get_pbp_game(game_ids)

# Filter for all shots (including free throws) for Tristen Newton
specified_player_all_shots <- shots %>%
  filter(!is.na(shot_x) & !is.na(shot_y) & shooter == "Tristen Newton")

player_stats <- specified_player_all_shots %>%
  group_by(shooter) %>%
  summarise(
    TotalPoints = sum(case_when(
      shot_outcome == "made" & free_throw == FALSE & three_pt == TRUE ~ 3,
      shot_outcome == "made" & free_throw == FALSE & three_pt == FALSE ~ 2,
      shot_outcome == "made" & free_throw == TRUE ~ 1,
      TRUE ~ 0
    )),
    FG_Attempts = sum(free_throw == FALSE),
    FG_Made = sum(shot_outcome == "made" & free_throw == FALSE),
    FT_Attempts = sum(free_throw == TRUE),
    FT_Made = sum(shot_outcome == "made" & free_throw == TRUE),
    ThreeP_Attempts = sum(three_pt == TRUE),
    ThreeP_Made = sum(shot_outcome == "made" & three_pt == TRUE)
  ) %>%
  mutate(
    FG_Percentage = round(FG_Made / FG_Attempts * 100, 1),
    FT_Percentage = round(FT_Made / FT_Attempts * 100, 1),
    ThreeP_Percentage = round(ThreeP_Made / ThreeP_Attempts * 100, 1)
  ) %>%
  mutate(
    FG_Percentage = paste0(FG_Percentage, "%"),
    FT_Percentage = paste0(FT_Percentage, "%"),
    ThreeP_Percentage = paste0(ThreeP_Percentage, "%")
  ) %>%
  rename(
    Player = shooter,
    Points = TotalPoints,
    `FG%` = FG_Percentage,
    `FT%` = FT_Percentage,
    `3P%` = ThreeP_Percentage
  ) %>%
  select(Player, Points, `FG%`, `FT%`, `3P%`)

# Display the dataframe
print(player_stats)

# Filter for shots excluding free throws for Tristen Newton
specified_player_excluding_ft <- shots %>%
  filter(!is.na(shot_x) & !is.na(shot_y) & !free_throw & shooter == "Tristen Newton")

player_name <- specified_player_all_shots$shooter[1]
away_team <- specified_player_all_shots$away[1]
home_team <- specified_player_all_shots$home[1]
game_date <- specified_player_all_shots$date[1]

#court_image <- png::readPNG("/Users/Mason/Desktop/testingdata/court.png")

court_image <- png::readPNG("/Users/Mason/Desktop/testingdata/Carnesecca.png")

# Convert the image to a raster object that ggplot can understand
court_grob <- grid::rasterGrob(court_image, interpolate = TRUE)

#BLANK Shot Chart
#shot_chart <- ggplot(shots_with_loc, aes(x = shot_x, y = shot_y)) +
  #annotation_custom(court_grob, xmin = -53.5, xmax = 53.5, ymin = -31, ymax = 31) +  # Add the court image
  #geom_point() +
  #labs(x = "Shot X Coordinate", y = "Shot Y Coordinate", title = "Shot Chart") +
  #theme_minimal() +
  #coord_cartesian(xlim = c(-53.5, 53.5), ylim = c(-31, 31))

plot_title <- paste(player_name, "shot chart", away_team, "vs", home_team, game_date)

shot_chart <- ggplot(specified_player_excluding_ft, aes(x = shot_x, y = shot_y)) +
  annotation_custom(court_grob, xmin = -53.5, xmax = 53.5, ymin = -31, ymax = 31) +  # Add the court image
  geom_point(data = specified_player_excluding_ft %>% filter(shot_outcome == "made"), 
             aes(x = shot_x, y = shot_y), 
             color = "#00B400", 
             size = 3, # Adjust the size as needed
             alpha = 0.5, # Adjust alpha for transparency
             stroke = 1, # Adjust stroke for border thickness
             position = position_jitter(width = 0.5, height = 0.5)) +
  geom_point(data = specified_player_excluding_ft %>% filter(shot_outcome == "missed"), 
             aes(x = shot_x, y = shot_y), 
             color = "#D40000", 
             shape = 4, 
             size = 3, # Adjust the size as needed
             position = position_jitter(width = 0.5, height = 0.5)) +
  labs(title = plot_title) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, color = "black"),  # Center and color the title
    axis.title = element_blank(),  # Remove axis titles
    axis.text = element_blank(),  # Remove axis text
    axis.ticks = element_blank(),  # Remove axis ticks
    panel.grid = element_blank(),  # Remove panel grid
    panel.border = element_blank(),  # Remove panel border
    plot.background = element_blank(),  # Remove plot background
    legend.position = "none"  # Remove legend if not needed
  ) +
  coord_cartesian(xlim = c(-53.5, 53.5), ylim = c(-31, 31))

# Display the scatter plot
print(shot_chart)

player_stats_table <- tableGrob(player_stats, rows = NULL,
                                theme = ttheme_default(
                                  core = list(fg_params = list(fontface = 'bold', fontsize = 10),
                                              bg_params = list(fill = 'white', alpha = 0)),
                                  colhead = list(fg_params = list(fontface = 'bold', fontsize = 12),
                                                 bg_params = list(fill = 'grey90', alpha = 1)),
                                  padding = unit(c(4, 4), "mm") # Adjust padding
                                ))

# Arrange your plots (assuming shot_chart is a ggplot object)
combined_plot <- grid.arrange(shot_chart, player_stats_table, ncol = 1, heights = c(4, 1))

# Save the combined plot to a file
ggsave("/Users/Mason/Desktop/rimages/shotchart.png", combined_plot, width = 48 / 4, height = (31 + 10) / 4, dpi = 600, limitsize = FALSE)

