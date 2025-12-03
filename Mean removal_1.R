# Clear environment
rm(list = ls())

# Load required packages
library(dplyr)
library(flextable)
library(officer)

# Define directory and file path
data_dir <- "C:/Users/DELL/Desktop/All_Days_analysis/data"
data_file <- file.path(data_dir, "Original_data_restructured_csv.csv")

# Create directory if missing
if (!dir.exists(data_dir)) {
  dir.create(data_dir, recursive = TRUE)
  cat("Created directory:", data_dir, "\n")
}

# Set working directory
setwd(data_dir)

# Load data
original.data <- read.csv(data_file)
str(original.data)

# --- Mean seed removal + SE by species, habitat, site, and day ---
table_data <- original.data %>%
  filter(cage == "opened_cage") %>%
  group_by(species, habitat, site, day) %>%
  summarise(
    Mean_Seed_Removal = mean(nr_seeds_remov, na.rm = TRUE),
    SE_Seed_Removal = sd(nr_seeds_remov, na.rm = TRUE) / sqrt(sum(!is.na(nr_seeds_remov))),
    .groups = "drop"
  ) %>%
  mutate(
    Mean_Seed_Removal = round(Mean_Seed_Removal, 2),
    SE_Seed_Removal = round(SE_Seed_Removal, 2)
  )

# --- Add overall mean across all days ---
overall_means <- original.data %>%
  filter(cage == "opened_cage") %>%
  group_by(species, habitat, site) %>%
  summarise(
    Overall_Mean_Removal = round(mean(nr_seeds_remov, na.rm = TRUE), 2),
    .groups = "drop"
  )

# Merge overall means into main table
table_data <- left_join(table_data, overall_means, by = c("species", "habitat", "site"))

# --- Merge Mean and SE into one column ---
table_data <- table_data %>%
  mutate(`Mean ± SE` = paste0(Mean_Seed_Removal, " ± ", SE_Seed_Removal)) %>%
  select(species, habitat, site, day, `Mean ± SE`, Overall_Mean_Removal)

# --- Save results to CSV ---
table_filename <- "mean_seed_removal_by_species_habitat_site_day.csv"
write.csv(table_data, file = file.path(data_dir, table_filename), row.names = FALSE)
cat("Saved CSV:", file.path(data_dir, table_filename), "\n")

# --- Flextable ---
ft <- flextable(table_data) %>%
  set_header_labels(
    species = "Species",
    habitat = "Habitat",
    site = "Site",
    day = "Day",
    `Mean ± SE` = "Mean ± SE",
    Overall_Mean_Removal = "Overall Mean Removal"
  ) %>%
  border_remove() %>%
  hline_top(part = "header", border = fp_border(color = "black", width = 1)) %>%
  hline(part = "header", border = fp_border(color = "black", width = 1)) %>%
  hline_bottom(part = "body", border = fp_border(color = "black", width = 1)) %>%
  font(fontname = "Times New Roman", part = "all") %>%
  align(align = "center", part = "body", j = c(4, 5, 6)) %>%
  align(align = "left", part = "body", j = c(1, 2, 3)) %>%
  width(j = c(1, 2, 3), width = 1.5) %>%
  width(j = c(4, 5, 6), width = 1.5)

# --- Save Word document ---
word_filename <- "mean_seed_removal_table_opened_cages.docx"
doc <- read_docx() %>% body_add_flextable(ft)
print(doc, target = file.path(data_dir, word_filename))
cat("Saved Word document:", file.path(data_dir, word_filename), "\n")
