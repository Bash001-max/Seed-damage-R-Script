# Clear environment
rm(list = ls())

# Load required packages
library(dplyr)
library(flextable)
library(officer)

# Set data output directory
data_dir <- "C:/Users/HP/Desktop/All_Days_analysis/data"
if (!dir.exists(data_dir)) {
  dir.create(data_dir, recursive = TRUE)
  cat("Created directory:", data_dir, "\n")
}
setwd(data_dir)

# Load data
original.data <- read.csv("Original_data_restructured_csv.csv")
str(original.data)

# Print summary statistics for nr_seeds_remov in opened_cage
cat("\nSummary statistics for nr_seeds_remov (opened_cage only):\n")
opened_cage_data <- original.data %>% filter(cage == "opened_cage")
print(summary(opened_cage_data$nr_seeds_remov))
cat("Number of observations:", nrow(opened_cage_data), "\n")
cat("Unique species:", unique(opened_cage_data$species), "\n")
cat("Unique sites:", unique(opened_cage_data$site), "\n")
cat("Unique days:", unique(opened_cage_data$day), "\n")
cat("Missing values in nr_seeds_remov:", sum(is.na(opened_cage_data$nr_seeds_remov)), "\n")

# Calculate the number of unique opened cages per species
cage_counts <- original.data %>%
  filter(cage == "opened_cage") %>%
  group_by(species) %>%
  summarise(
    Num_Cages = n_distinct(habitat, site, id)
  ) %>%
  mutate(
    Initial_Seeds = Num_Cages * 25  # 25 seeds per cage at start
  )

# Print cage counts for verification
cat("\nNumber of opened cages and initial seeds per species:\n")
print(cage_counts)

# Calculate total seeds replaced (sum of nr_seeds_remov for days 1 and 2 in opened cages)
replacements <- original.data %>%
  filter(cage == "opened_cage", day %in% c(1, 2)) %>%  # Only days 1 and 2 had replacements
  group_by(species) %>%
  summarise(
    Total_Replaced_Seeds = sum(nr_seeds_remov, na.rm = TRUE)
  )

# Combine initial seeds and replacements to get total seeds introduced
table_data <- cage_counts %>%
  left_join(replacements, by = "species") %>%
  mutate(
    Total_Replaced_Seeds = replace(Total_Replaced_Seeds, is.na(Total_Replaced_Seeds), 0),  # Handle NA replacements
    Total_Seeds_Introduced = Initial_Seeds + Total_Replaced_Seeds
  ) %>%
  select(species, Num_Cages, Initial_Seeds, Total_Replaced_Seeds, Total_Seeds_Introduced)

# Validate table_data
cat("\nValidating table_data:\n")
cat("Number of rows:", nrow(table_data), "\n")
cat("NA values in table_data:\n")
print(colSums(is.na(table_data)))

# Print the table to console for verification
cat("\nTable of Total Seeds Introduced by Species (Opened Cages Only):\n")
print(table_data)

# Save the table as a CSV file with error handling
table_filename <- "total_seeds_introduced_opened_cages.csv"
tryCatch({
  write.csv(table_data, file = file.path(data_dir, table_filename), row.names = FALSE)
  cat("Saved table:", file.path(data_dir, table_filename), "\n")
}, error = function(e) {
  cat("Error saving CSV file:", e$message, "\n")
  cat("Try running R as administrator or saving to C:/Users/HP/Documents.\n")
})

# Create flextable with error handling
ft <- NULL
tryCatch({
  ft <- flextable(table_data) %>%
    set_header_labels(
      species = "Species",
      Num_Cages = "Number of Cages",
      Initial_Seeds = "Initial Seeds",
      Total_Replaced_Seeds = "Replaced Seeds",
      Total_Seeds_Introduced = "Total Seeds Introduced"
    ) %>%
    border_remove() %>%
    hline_top(part = "header", border = fp_border(color = "black", width = 1)) %>%
    hline(part = "header", border = fp_border(color = "black", width = 1)) %>%
    hline_bottom(part = "body", border = fp_border(color = "black", width = 1)) %>%
    font(fontname = "Times New Roman", part = "all") %>%
    align(align = "center", part = "body", j = c(2, 3, 4, 5)) %>%
    align(align = "left", part = "body", j = 1) %>%
    width(j = 1, width = 1.5) %>%
    width(j = c(2, 3, 4, 5), width = 1.2)
  cat("Flextable created successfully.\n")
}, error = function(e) {
  cat("Error creating flextable:", e$message, "\n")
})

# Create and save Word document with error handling
word_filename <- "total_seeds_introduced_opened_cages.docx"
tryCatch({
  if (is.null(ft)) stop("Flextable object 'ft' is not defined. Cannot save Word document.")
  doc <- read_docx() %>% body_add_flextable(ft)
  print(doc, target = file.path(data_dir, word_filename))
  cat("Saved Word document:", file.path(data_dir, word_filename), "\n")
}, error = function(e) {
  cat("Error saving Word document:", e$message, "\n")
  cat("Try running R as administrator or saving to C:/Users/HP/Documents.\n")
})