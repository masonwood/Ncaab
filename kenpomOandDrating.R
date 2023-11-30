library(dplyr)
library(rvest)
library(httr)
library(tibble)
library(readr)
library(ggplot2)
library(ggimage)

# Fetching the webpage
url <- "https://kenpom.com/"
webpage <- httr::GET(url, user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.110 Safari/537.3"))
html_data <- rvest::read_html(webpage)

# Extracting tables
tables <- html_data %>% html_nodes("table")

# Extracting the first table
kenpom_table <- tables %>% 
  .[[1]] %>% 
  html_table(fill = TRUE)

# Convert the first row to a character vector to ensure proper name handling
col_names <- as.character(unlist(kenpom_table[1, ]))

# Make the names unique
unique_col_names <- make.unique(col_names)

# Remove the first row and set the unique column names
kenpom_table <- kenpom_table[-1, ]
colnames(kenpom_table) <- unique_col_names

# Convert the data to a tibble
kenpom_table <- as_tibble(kenpom_table, .name_repair = "unique")

be_conf_table <- kenpom_table %>%
  filter(`Conf` == "BE")

team_logos_df <- read_csv("/Users/Mason/Desktop/testingdata/logos/team_ids.csv")

# Map the logos to the teams in be_conf_table
be_conf_table <- be_conf_table %>%
  left_join(team_logos_df, by = c("Team" = "team"))

# Construct the path to the logos
be_conf_table <- be_conf_table %>%
  mutate(logo_path = if_else(is.na(team.id), "/Users/Mason/Desktop/testingdata/logos/41.png", paste0("/Users/Mason/Desktop/testingdata/logos/", team.id, ".png")))

# Check for NA in logo_path and remove those rows
be_conf_table <- be_conf_table %>%
  filter(!is.na(logo_path))

# Ensure AdjO and AdjD are numeric
be_conf_table <- be_conf_table %>%
  mutate(AdjO = as.numeric(as.character(AdjO)),
         AdjD = as.numeric(as.character(AdjD)))

# Now, create the scatter plot with team logos
plot <- ggplot(be_conf_table, aes(x = AdjO, y = AdjD)) +
  geom_image(aes(image = logo_path), size = 0.125) + # Adjust size as needed
  scale_y_reverse() + # This will flip the y-axis so lower is better for defense
  labs(x = "Offensive", 
       y = "Defensive",
       title = "Kenpom Adjusted Efficiency Rating") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, margin = margin(b = 5)), # Add space below the title
        plot.margin = margin(10, 10, 10, 10)) # Adjust the plot margins (top, right, bottom, left)

ggsave("/Users/Mason/Desktop/rimages/kenpomeff.png", plot, width = 10, height = 6, dpi = 500)
