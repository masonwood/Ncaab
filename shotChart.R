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

#data(ids)

# Get Schedule w/ game IDS
schedule <- get_schedule("Syracuse", season = "2023-24")

game_ids <- schedule$game_id[1:7]

# Grab all shots from Cuse games 
shots <- get_pbp_game(game_ids)

shots_with_loc <- shots %>%
  filter(!is.na(shot_x) & !is.na(shot_y))

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

shot_chart <- ggplot(shots_with_loc, aes(x = shot_x, y = shot_y)) +
  annotation_custom(court_grob, xmin = -53.5, xmax = 53.5, ymin = -31, ymax = 31) +  # Add the court image
  geom_point(data = shots_with_loc %>% filter(shot_outcome == "made"), aes(x = shot_x, y = shot_y), color = "green") +
  geom_point(data = shots_with_loc %>% filter(shot_outcome == "missed"), aes(x = shot_x, y = shot_y), color = "red", shape = 4) +
  labs(title = "Shot Chart") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, color = "white"),  # Center and color the title
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
shot_chart

file_path <- "/Users/Mason/Desktop/rimages/shotchart.png"

# Save the shot chart to the specified file path
ggsave(file_path, shot_chart, width = 48 / 4, height = 31 / 4, dpi = 600, limitsize = FALSE)
