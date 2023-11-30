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

court_image <- png::readPNG("/Users/Mason/Desktop/testingdata/court.png")

# Convert the image to a raster object that ggplot can understand
court_grob <- grid::rasterGrob(court_image, interpolate = TRUE)

# Create the shot chart with the court image in the background
shot_chart <- ggplot(shots_with_loc, aes(x = shot_x, y = shot_y)) +
  annotation_custom(court_grob, xmin = -53.5, xmax = 53.5, ymin = -31, ymax = 31) +  # Add the court image
  geom_point() +
  labs(x = "Shot X Coordinate", y = "Shot Y Coordinate", title = "Shot Chart") +
  theme_minimal() +
  coord_cartesian(xlim = c(-53.5, 53.5), ylim = c(-31, 31))

# Display the scatter plot
shot_chart