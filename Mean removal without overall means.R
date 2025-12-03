# Clear environment
rm(list = ls())

# Load required packages
library(dplyr)
library(flextable)
library(officer)

# Set data output directory
data_dir <- "C:/Users/DELL/Desktop/All_Days_analysis/data/Original_data_restructured_csv.csv"
if (!dir.exists(data_dir)) {
  dir.create(data_dir, recursive = TRUE)
  cat("Created directory:", data_dir, "/n")
}
setwd(data_dir)

# Load data
original.data <- read.csv("Original_data_restructured_csv.csv")
str(original.data)

# Print summary statistics for nr_seeds_remov in opened_cage to diagnose small means
cat("/nSummary statistics for nr_seeds_remov (opened_cage only):/n")
opened_cage_data <- original.data %>% filter(cage == "opened_cage")
print(summary(opened_cage_data$nr_seeds_remov))
cat("Number of observations:", nrow(opened_cage_data), "/n")
cat("Unique sites:", unique(opened_cage_data$site), "/n")
cat("Missing values:", sum(is.na(opened_cage_data$nr_seeds_remov)), "/n")

# Calculate mean seed removal and standard error for opened cages, grouped by species, habitat, and site
table_data <- original.data %>%
  filter(cage == "opened_cage") %>%
  group_by(species, habitat, site) %>%
  summarise(
    Mean_Seed_Removal = mean(nr_seeds_remov, na.rm = TRUE),
    SE_Seed_Removal = sd(nr_seeds_remov, na.rm = TRUE) / sqrt(sum(!is.na(nr_seeds_remov)))
  ) %>%
  ungroup() %>%
  mutate(
    Mean_Seed_Removal = round(Mean_Seed_Removal, 2),
    SE_Seed_Removal = round(SE_Seed_Removal, 2)
  )

# Identify top and bottom values per species
table_data <- table_data %>%
  group_by(species) %>%
  mutate(
    is_top = Mean_Seed_Removal == max(Mean_Seed_Removal, na.rm = TRUE),
    is_bottom = Mean_Seed_Removal == min(Mean_Seed_Removal, na.rm = TRUE)
  ) %>%
  ungroup()

# Create top and bottom value data frames for ranking
top_values <- table_data %>%
  filter(is_top) %>%
  arrange(desc(Mean_Seed_Removal)) %>%
  mutate(rank_top = row_number()) %>%  # Sequential ranking to avoid ties issues
  select(species, habitat, site, Mean_Seed_Removal, rank_top)

bottom_values <- table_data %>%
  filter(is_bottom) %>%
  arrange(Mean_Seed_Removal) %>%
  mutate(rank_bottom = row_number()) %>%  # Sequential ranking
  select(species, habitat, site, Mean_Seed_Removal, rank_bottom)

# Merge rankings back into table_data with explicit key
table_data <- table_data %>%
  left_join(top_values %>% select(species, habitat, site, rank_top), 
            by = c("species", "habitat", "site")) %>%
  left_join(bottom_values %>% select(species, habitat, site, rank_bottom), 
            by = c("species", "habitat", "site")) %>%
  mutate(
    color = case_when(
      rank_top == 1 ~ "#FF0000",  # Deep red for highest top value
      rank_top == 2 ~ "#FF6666",  # Moderate red for second highest
      rank_top == 3 ~ "#FF9999",  # Light red for third highest
      rank_bottom == 1 ~ "#006600",  # Deep green for lowest bottom value
      rank_bottom == 2 ~ "#33CC33",  # Moderate green for second lowest
      rank_bottom == 3 ~ "#99FF99",  # Light green for third lowest
      TRUE ~ "#000000"  # Black for non-highlighted values
    ),
    bold = !is.na(rank_top) | !is.na(rank_bottom)  # Bold only highlighted values
  )

# Validate table_data
cat("/nValidating table_data:/n")
cat("Number of rows:", nrow(table_data), "/n")
cat("NA in color:", sum(is.na(table_data$color)), "/n")
cat("NA in bold:", sum(is.na(table_data$bold)), "/n")
if (sum(is.na(table_data$color)) > 0 | sum(is.na(table_data$bold)) > 0) {
  cat("Warning: NA values detected in color or bold columns. Check data merging./n")
}

# Print the table to console for verification
cat("/nTable of Mean Seed Removal and Standard Error by Species, Habitat, and Site (Opened Cages Only):/n")
print(table_data)

# Save the table as a CSV file with error handling
table_filename <- "mean_seed_removal_by_species_habitat_site_opened_cages.csv"
tryCatch({
  write.csv(table_data, file = file.path(data_dir, table_filename), row.names = FALSE)
  cat("Saved table:", file.path(data_dir, table_filename), "/n")
}, error = function(e) {
  cat("Error saving CSV file:", e$message, "/n")
  cat("Try running R as administrator or saving to C:/Users/HP/Documents./n")
})

# Create flextable with error handling
ft <- NULL
tryCatch({
  ft <- flextable(table_data) %>%
    set_header_labels(
      species = "Species",
      habitat = "Habitat",
      site = "Site",
      Mean_Seed_Removal = "Mean Seed Removal",
      SE_Seed_Removal = "Standard Error"
    ) %>%
    border_remove() %>%
    hline_top(part = "header", border = fp_border(color = "black", width = 1)) %>%
    hline(part = "header", border = fp_border(color = "black", width = 1)) %>%
    hline_bottom(part = "body", border = fp_border(color = "black", width = 1)) %>%
    font(fontname = "Times New Roman", part = "all") %>%
    align(align = "center", part = "body", j = c(4, 5)) %>%
    align(align = "left", part = "body", j = c(1, 2, 3)) %>%
    width(j = c(1, 2, 3), width = 1.5) %>%
    width(j = c(4, 5), width = 1.2) %>%
    color(j = "Mean_Seed_Removal", color = table_data$color) %>%
    bold(j = "Mean_Seed_Removal", bold = table_data$bold)
  cat("Flextable created successfully./n")
}, error = function(e) {
  cat("Error creating flextable:", e$message, "/n")
})

# Create and save Word document with error handling
word_filename <- "mean_seed_removal_table_opened_cages.docx"
tryCatch({
  if (is.null(ft)) stop("Flextable object 'ft' is not defined. Cannot save Word document.")
  doc <- read_docx() %>% body_add_flextable(ft)
  print(doc, target = file.path(data_dir, word_filename))
  cat("Saved Word document:", file.path(data_dir, word_filename), "/n")
}, error = function(e) {
  cat("Error saving Word document:", e$message, "/n")
  cat("Try running R as administrator or saving to C:/Users/HP/Documents./n")
})