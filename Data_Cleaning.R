library(dplyr)

data <- read.table("data/agaricus-lepiota.data", header = FALSE, sep = ",")

data_with_columns <- data |>
  rename(
    `classes` = V1,
    `cap-shape` = V2,
    `cap-surface` = V3,
    `cap-color` = V4,
    `bruises?` = V5,
    `odor` = V6,
    `gill-attachment` = V7,
    `gill-spacing` = V8,
    `gill-size` = V9,
    `gill-color` = V10,
    `stalk-shape` = V11,
    `stalk-root` = V12,
    `stalk-surface-above-ring` = V13,
    `stalk-surface-below-ring` = V14,
    `stalk-color-above-ring` = V15,
    `stalk-color-below-ring` = V16,
    `veil-type` = V17,
    `veil-color` = V18,
    `ring-number` = V19,
    `ring-type` = V20,
    `spore-print-color` = V21,
    `population` = V22,
    `habitat` = V23
  )

data_with_columns_and_string_values <- data_with_columns |>
  mutate(
    classes = case_when(
      classes == "e" ~ "edible",
      classes == "p" ~ "poisonous",
      TRUE ~ classes
    ),
    
    `cap-shape` = case_when(
      `cap-shape` == "b" ~ "bell",
      `cap-shape` == "c" ~ "conical",
      `cap-shape` == "x" ~ "convex",
      `cap-shape` == "f" ~ "flat",
      `cap-shape` == "k" ~ "knobbed",
      `cap-shape` == "s" ~ "sunken",
      TRUE ~ `cap-shape`
    ),
    
    `cap-surface` = case_when(
      `cap-surface` == "f" ~ "fibrous",
      `cap-surface` == "g" ~ "grooves",
      `cap-surface` == "y" ~ "scaly",
      `cap-surface` == "s" ~ "smooth",
      TRUE ~ `cap-surface`
    ),
    
    `cap-color` = case_when(
      `cap-color` == "n" ~ "brown",
      `cap-color` == "b" ~ "buff",
      `cap-color` == "c" ~ "cinnamon",
      `cap-color` == "g" ~ "gray",
      `cap-color` == "r" ~ "green",
      `cap-color` == "p" ~ "pink",
      `cap-color` == "u" ~ "purple",
      `cap-color` == "e" ~ "red",
      `cap-color` == "w" ~ "white",
      `cap-color` == "y" ~ "yellow",
      TRUE ~ `cap-color`
    ),
    
    `bruises?` = case_when(
      `bruises?` == "t" ~ "bruises",
      `bruises?` == "f" ~ "no",
      TRUE ~ `bruises?`
    ),
    
    odor = case_when(
      odor == "a" ~ "almond",
      odor == "l" ~ "anise",
      odor == "c" ~ "creosote",
      odor == "y" ~ "fishy",
      odor == "f" ~ "foul",
      odor == "m" ~ "musty",
      odor == "n" ~ "none",
      odor == "p" ~ "pungent",
      odor == "s" ~ "spicy",
      TRUE ~ odor
    ),
    
    `gill-attachment` = case_when(
      `gill-attachment` == "a" ~ "attached",
      `gill-attachment` == "d" ~ "descending",
      `gill-attachment` == "f" ~ "free",
      `gill-attachment` == "n" ~ "notched",
      TRUE ~ `gill-attachment`
    ),
    
    `gill-spacing` = case_when(
      `gill-spacing` == "c" ~ "close",
      `gill-spacing` == "w" ~ "crowded",
      `gill-spacing` == "d" ~ "distant",
      TRUE ~ `gill-spacing`
    ),
    
    `gill-size` = case_when(
      `gill-size` == "b" ~ "broad",
      `gill-size` == "n" ~ "narrow",
      TRUE ~ `gill-size`
    ),
    
    `gill-color` = case_when(
      `gill-color` == "k" ~ "black",
      `gill-color` == "n" ~ "brown",
      `gill-color` == "b" ~ "buff",
      `gill-color` == "h" ~ "chocolate",
      `gill-color` == "g" ~ "gray",
      `gill-color` == "r" ~ "green",
      `gill-color` == "o" ~ "orange",
      `gill-color` == "p" ~ "pink",
      `gill-color` == "u" ~ "purple",
      `gill-color` == "e" ~ "red",
      `gill-color` == "w" ~ "white",
      `gill-color` == "y" ~ "yellow",
      TRUE ~ `gill-color`
    ),
    
    `stalk-shape` = case_when(
      `stalk-shape` == "e" ~ "enlarging",
      `stalk-shape` == "t" ~ "tapering",
      TRUE ~ `stalk-shape`
    ),
    
    `stalk-root` = case_when(
      `stalk-root` == "b" ~ "bulbous",
      `stalk-root` == "c" ~ "club",
      `stalk-root` == "u" ~ "cup",
      `stalk-root` == "e" ~ "equal",
      `stalk-root` == "z" ~ "rhizomorphs",
      `stalk-root` == "r" ~ "rooted",
      `stalk-root` == "?" ~ "missing",
      TRUE ~ `stalk-root`
    ),
    
    `stalk-surface-above-ring` = case_when(
      `stalk-surface-above-ring` == "f" ~ "fibrous",
      `stalk-surface-above-ring` == "y" ~ "scaly",
      `stalk-surface-above-ring` == "k" ~ "silky",
      `stalk-surface-above-ring` == "s" ~ "smooth",
      TRUE ~ `stalk-surface-above-ring`
    ),
    
    `stalk-surface-below-ring` = case_when(
      `stalk-surface-below-ring` == "f" ~ "fibrous",
      `stalk-surface-below-ring` == "y" ~ "scaly",
      `stalk-surface-below-ring` == "k" ~ "silky",
      `stalk-surface-below-ring` == "s" ~ "smooth",
      TRUE ~ `stalk-surface-below-ring`
    ),
    
    `stalk-color-above-ring` = case_when(
      `stalk-color-above-ring` == "n" ~ "brown",
      `stalk-color-above-ring` == "b" ~ "buff",
      `stalk-color-above-ring` == "c" ~ "cinnamon",
      `stalk-color-above-ring` == "g" ~ "gray",
      `stalk-color-above-ring` == "o" ~ "orange",
      `stalk-color-above-ring` == "p" ~ "pink",
      `stalk-color-above-ring` == "e" ~ "red",
      `stalk-color-above-ring` == "w" ~ "white",
      `stalk-color-above-ring` == "y" ~ "yellow",
      TRUE ~ `stalk-color-above-ring`
    ),
    
    `stalk-color-below-ring` = case_when(
      `stalk-color-below-ring` == "n" ~ "brown",
      `stalk-color-below-ring` == "b" ~ "buff",
      `stalk-color-below-ring` == "c" ~ "cinnamon",
      `stalk-color-below-ring` == "g" ~ "gray",
      `stalk-color-below-ring` == "o" ~ "orange",
      `stalk-color-below-ring` == "p" ~ "pink",
      `stalk-color-below-ring` == "e" ~ "red",
      `stalk-color-below-ring` == "w" ~ "white",
      `stalk-color-below-ring` == "y" ~ "yellow",
      TRUE ~ `stalk-color-below-ring`
    ),
    
    `veil-type` = case_when(
      `veil-type` == "p" ~ "partial",
      `veil-type` == "u" ~ "universal",
      TRUE ~ `veil-type`
    ),
    
    `veil-color` = case_when(
      `veil-color` == "n" ~ "brown",
      `veil-color` == "o" ~ "orange",
      `veil-color` == "w" ~ "white",
      `veil-color` == "y" ~ "yellow",
      TRUE ~ `veil-color`
    ),
    
    `ring-number` = case_when(
      `ring-number` == "n" ~ "none",
      `ring-number` == "o" ~ "one",
      `ring-number` == "t" ~ "two",
      TRUE ~ `ring-number`
    ),
    
    `ring-type` = case_when(
      `ring-type` == "c" ~ "cobwebby",
      `ring-type` == "e" ~ "evanescent",
      `ring-type` == "f" ~ "flaring",
      `ring-type` == "l" ~ "large",
      `ring-type` == "n" ~ "none",
      `ring-type` == "p" ~ "pendant",
      `ring-type` == "s" ~ "sheathing",
      `ring-type` == "z" ~ "zone",
      TRUE ~ `ring-type`
    ),
    
    `spore-print-color` = case_when(
      `spore-print-color` == "k" ~ "black",
      `spore-print-color` == "n" ~ "brown",
      `spore-print-color` == "b" ~ "buff",
      `spore-print-color` == "h" ~ "chocolate",
      `spore-print-color` == "r" ~ "green",
      `spore-print-color` == "o" ~ "orange",
      `spore-print-color` == "u" ~ "purple",
      `spore-print-color` == "w" ~ "white",
      `spore-print-color` == "y" ~ "yellow",
      TRUE ~ `spore-print-color`
    ),
    
    population = case_when(
      population == "a" ~ "abundant",
      population == "c" ~ "clustered",
      population == "n" ~ "numerous",
      population == "s" ~ "scattered",
      population == "v" ~ "several",
      population == "y" ~ "solitary",
      TRUE ~ population
    ),
    
    habitat = case_when(
      habitat == "g" ~ "grasses",
      habitat == "l" ~ "leaves",
      habitat == "m" ~ "meadows",
      habitat == "p" ~ "paths",
      habitat == "u" ~ "urban",
      habitat == "w" ~ "waste",
      habitat == "d" ~ "woods",
      TRUE ~ habitat
    )
  )
