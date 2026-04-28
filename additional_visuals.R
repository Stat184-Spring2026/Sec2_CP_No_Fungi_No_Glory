# Additional Summary statistics and visualization
# Project: Mushroom Exploratory Data Analysis

library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(forcats)

# Load cleaned data
source("Data_Cleaning.R")

data_clean <- data_with_columns_and_string_values |>
  mutate(across(everything(), as.factor))

# Design standards
mushroom_colors <- c(
  "edible" = "#9FE3B5", #used green as green light, safe to eat
  "poisonous" = "#9C6ADE" #used purple to demonstrate poison
)

mushroom_theme <- theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11),
    axis.title = element_text(size = 11),
    legend.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


### Visual: Dot Plot - Gill Color Counts by Class ###
### Insight: Compares edible and poisonous counts without using bars ###

gill_counts <- data_clean |>
  count(`gill-color`, classes)

ggplot(gill_counts, aes(x = n, y = fct_reorder(`gill-color`, n), color = classes)) +
  geom_point(size = 4) +
  scale_color_manual(values = mushroom_colors) +
  mushroom_theme +
  labs(
    title = "Gill Color Distribution by Class",
    subtitle = "Dot position shows how common each gill color is by class",
    x = "Count",
    y = "Gill Color",
    color = "Class"
  )



### Visual: Bubble Plot - Spore Print Color and Class ###
### Insight: Larger bubbles show more common class-spore color combinations ###

spore_bubble <- data_clean |>
  count(`spore-print-color`, classes)

ggplot(spore_bubble, aes(x = classes, y = `spore-print-color`, size = n, color = classes)) +
  geom_point(alpha = 0.7) +
  scale_color_manual(values = mushroom_colors) +
  mushroom_theme +
  labs(
    title = "Spore Print Color by Mushroom Class",
    subtitle = "Bubble size shows how many mushrooms fall into each group",
    x = "Class",
    y = "Spore Print Color",
    size = "Count",
    color = "Class"
  )


### Visual: Density-style Count Plot - Population Type ###
### Insight: Shows how population types differ by mushroom class ###

population_counts <- data_clean |>
  count(population, classes)

ggplot(population_counts, aes(x = n, color = classes, fill = classes)) +
  geom_density(alpha = 0.3) +
  scale_color_manual(values = mushroom_colors) +
  scale_fill_manual(values = mushroom_colors) +
  mushroom_theme +
  labs(
    title = "Distribution of Population Counts by Class",
    subtitle = "Density shape shows how population categories vary by class",
    x = "Count Across Population Types",
    y = "Density",
    color = "Class",
    fill = "Class"
  )


### Visual: Top Poisonous Feature Combinations - Bubble Plot ###
### Insight: Shows the most common combinations among poisonous mushrooms ###

top_poisonous_patterns <- data_clean |>
  filter(classes == "poisonous") |>
  count(odor, `gill-color`, `spore-print-color`) |>
  arrange(desc(n)) |>
  slice_head(n = 10) |>
  mutate(pattern = paste(odor, `gill-color`, `spore-print-color`, sep = " / "))

ggplot(top_poisonous_patterns, aes(x = n, y = reorder(pattern, n), size = n)) +
  geom_point(color = "#9C6ADE", alpha = 0.7) +
  mushroom_theme +
  labs(
    title = "Top Poisonous Mushroom Feature Combinations",
    subtitle = "Larger bubbles show the most common poisonous patterns",
    x = "Number of Poisonous Mushrooms",
    y = "Feature Combination",
    size = "Count"
  )


### Visual 9: Faceted Dot Plot - Major Features ###
### Insight: Compares several important features in one organized view ###

major_features_long <- data_clean |>
  select(classes, odor, `gill-color`, `spore-print-color`, habitat, population, `ring-type`) |>
  pivot_longer(
    cols = -classes,
    names_to = "Feature",
    values_to = "Category"
  ) |>
  count(Feature, Category, classes)

ggplot(major_features_long, aes(x = n, y = fct_reorder(Category, n), color = classes)) +
  geom_point(size = 2.8) +
  facet_wrap(~ Feature, scales = "free_y") +
  scale_color_manual(values = mushroom_colors) +
  mushroom_theme +
  labs(
    title = "Major Mushroom Features by Class",
    subtitle = "Dot plots compare class patterns across several important features",
    x = "Count",
    y = "Category",
    color = "Class"
  )

