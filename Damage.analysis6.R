# Clear environment
rm(list = ls())

# Set working directory for data
data_dir <- "C:/Users/HP/Desktop/All_Days_analysis/data"
if (!dir.exists(data_dir)) {
  cat("\nDirectory does not exist. Creating:", data_dir, "\n")
  dir.create(data_dir, recursive = TRUE)
}
setwd(data_dir)
cat("\nWorking directory set to:", getwd(), "\n")

# Set output directory for plots
plot_dir <- "C:/Users/HP/Desktop/All_Days_analysis/plots/seed damage plots"
if (!dir.exists(plot_dir)) {
  cat("\nPlot directory does not exist. Creating:", plot_dir, "\n")
  dir.create(plot_dir, recursive = TRUE)
}
cat("\nPlot directory set to:", plot_dir, "\n")

# Load required packages
library(dplyr)      # For data manipulation
library(tidyr)      # For pivot_longer (to recreate long_data)
library(officer)    # For creating Word document tables
library(ggplot2)    # For visualizations

# Load dataset
df_data_original <- read.csv("Original_data_restructured_csv.csv") # Main datasheet for Wilcoxon tests

# Check structure of df_data_original
cat("\nStructure of df_data_original:\n")
str(df_data_original)
cat("\nFirst 6 rows of df_data_original:\n")
print(head(df_data_original))
cat("\nUnique values in df_data_original$cage (before any cleaning):\n")
print(unique(df_data_original$cage))

# Recreate long_data_csv.csv from df_data_original
cat("\nRecreating long_data_csv.csv from Original_data_restructured_csv.csv...\n")
df_long_data <- df_data_original %>%
  pivot_longer(
    cols = c(total_animal_damage, total_ins_dam, total_fung_dam),
    names_to = "agent",
    values_to = "damage"
  ) %>%
  mutate(
    agent = dplyr::recode(agent,
                          "total_animal_damage" = "vertebrate",
                          "total_ins_dam" = "insect",
                          "total_fung_dam" = "fungi")
  )

# Attempt to write the file with error handling
output_file <- "long_data_csv.csv"
final_file_path <- output_file
tryCatch({
  write.csv(df_long_data, output_file, row.names = FALSE)
  cat("\nSuccessfully wrote", output_file, "\n")
}, error = function(e) {
  cat("\nError writing", output_file, ":", e$message, "\n")
  cat("\nTrying to write to a temporary directory...\n")
  temp_file <- file.path(tempdir(), output_file)
  write.csv(df_long_data, temp_file, row.names = FALSE)
  cat("\nWrote to temporary file:", temp_file, "\n")
  # Update df_long_data to use the temporary file
  df_long_data <<- read.csv(temp_file)
  final_file_path <<- temp_file
})

# Check structure of df_long_data
cat("\nStructure of df_long_data:\n")
str(df_long_data)
cat("\nFirst 6 rows of df_long_data:\n")
print(head(df_long_data))

# Check unique values in key columns before cleaning
cat("\nUnique values in df_data_original$cage (before cleaning):\n")
print(unique(df_data_original$cage))
cat("\nUnique values in df_long_data$cage (before cleaning):\n")
print(unique(df_long_data$cage))

# Clean cage values: trim whitespace, convert to lowercase, and ensure character type
df_data_original <- df_data_original %>%
  mutate(cage = as.character(cage),
         cage = trimws(cage),
         cage = tolower(cage))

df_long_data <- df_long_data %>%
  mutate(cage = as.character(cage),
         cage = trimws(cage),
         cage = tolower(cage))

# Check unique values after cleaning
cat("\nUnique values in df_data_original$cage (after cleaning):\n")
print(unique(df_data_original$cage))
cat("\nUnique values in df_long_data$cage (after cleaning):\n")
print(unique(df_long_data$cage))

# Recode cage labels for readability
df_data_original <- df_data_original %>%
  mutate(cage = dplyr::recode(cage,
                              "large_meshed_cage" = "Coarse_mesh",
                              "small_meshed_cage" = "Fine_mesh",
                              "opened_cage" = "Opened"))

df_long_data <- df_long_data %>%
  mutate(cage = dplyr::recode(cage,
                              "large_meshed_cage" = "Coarse_mesh",
                              "small_meshed_cage" = "Fine_mesh",
                              "opened_cage" = "Opened"))

# Check unique values after recoding
cat("\nUnique values in df_data_original$cage (after recoding):\n")
print(unique(df_data_original$cage))
cat("\nUnique values in df_long_data$cage (after recoding):\n")
print(unique(df_long_data$cage))

# --- Recommendation from Interpretation: Species Clarification ---
# Confirm species as Anthonotha noldeae for report
cat("Note: Anthonotha refers to Anthonotha noldeae\n")

# --- Analysis: Wilcoxon Tests (Using Original_data_restructured_csv.csv) ---
# Aggregate damage across days to get total damage per site
total_damage <- df_data_original %>%
  group_by(site, habitat, species, cage) %>%
  summarise(
    total_animal_damage = sum(total_animal_damage, na.rm = TRUE),
    total_ins_dam = sum(total_ins_dam, na.rm = TRUE),
    total_fung_dam = sum(total_fung_dam, na.rm = TRUE),
    .groups = "drop"
  )

# Run three Wilcoxon Signed-Rank Tests for pairwise comparisons
wilcox_vi <- wilcox.test(total_damage$total_animal_damage, 
                         total_damage$total_ins_dam, 
                         paired = TRUE, exact = FALSE)
wilcox_if <- wilcox.test(total_damage$total_ins_dam, 
                         total_damage$total_fung_dam, 
                         paired = TRUE, exact = FALSE)
wilcox_fv <- wilcox.test(total_damage$total_fung_dam, 
                         total_damage$total_animal_damage, 
                         paired = TRUE, exact = FALSE)

# Collect p-values
pvals <- c(wilcox_vi$p.value, wilcox_if$p.value, wilcox_fv$p.value)
names(pvals) <- c("Vert_vs_Ins", "Ins_vs_Fung", "Fung_vs_Vert")

# Apply Benjamini-Hochberg correction
pvals_adjusted_bh <- p.adjust(pvals, method = "BH")

# Print results
cat("\nUnadjusted p-values:\n")
print(pvals)
cat("\nAdjusted p-values (Benjamini-Hochberg):\n")
print(pvals_adjusted_bh)

# --- Recommendation from Interpretation: Effect Size Calculation ---
# Calculate median differences to quantify magnitude of damage differences
median_diff_vi <- median(total_damage$total_animal_damage - total_damage$total_ins_dam, na.rm = TRUE)
median_diff_if <- median(total_damage$total_ins_dam - total_damage$total_fung_dam, na.rm = TRUE)
median_diff_fv <- median(total_damage$total_fung_dam - total_damage$total_animal_damage, na.rm = TRUE)
cat("\nMedian difference (Vert - Ins):", median_diff_vi, "\n")
cat("Median difference (Ins - Fung):", median_diff_if, "\n")
cat("Median difference (Fung - Vert):", median_diff_fv, "\n")

# --- Recommendation from Interpretation: Stratified Analysis by Cage ---
# Test differences in Fine_mesh cages to account for structural zeros
total_damage_fine <- total_damage %>% filter(cage == "Fine_mesh")
wilcox_fine_if <- wilcox.test(total_damage_fine$total_ins_dam, 
                              total_damage_fine$total_fung_dam, 
                              paired = TRUE, exact = FALSE)
cat("\nWilcoxon test (Insect vs. Fungi in Fine_mesh cages):\n")
cat("p-value:", wilcox_fine_if$p.value, "\n")

# --- Recommendation from Interpretation: Habitat Effect Test ---
# Test if vertebrate damage differs between habitats
wilcox_habitat <- wilcox.test(total_animal_damage ~ habitat, data = total_damage)
cat("\nWilcoxon test (Vertebrate damage by Habitat):\n")
cat("p-value:", wilcox_habitat$p.value, "\n")

# --- Updated Recommendation: Quantify Species Effect ---
# Test vertebrate damage differences across species (pairwise comparisons)
wilcox_species <- pairwise.wilcox.test(total_damage$total_animal_damage, 
                                       total_damage$species, 
                                       p.adjust.method = "BH", 
                                       paired = FALSE)
cat("\nPairwise Wilcoxon tests (Vertebrate damage by Species):\n")
print(wilcox_species)

# --- New Recommendation: Summarize Median Damage by Cage and Species ---
# Summarize median damage by cage and species
cat("\nMedian damage by cage and species:\n")
damage_summary <- total_damage %>%
  group_by(cage, species) %>%
  summarise(
    median_vert = median(total_animal_damage, na.rm = TRUE),
    median_ins = median(total_ins_dam, na.rm = TRUE),
    median_fung = median(total_fung_dam, na.rm = TRUE),
    .groups = "drop"
  )
print(damage_summary)

# --- New Recommendation: Visualize Results ---
# Create boxplots for vertebrate and insect damage by cage and species
cat("\nGenerating boxplots for damage by cage and species...\n")
ggplot(total_damage, aes(x = cage, y = total_animal_damage, fill = species)) +
  geom_boxplot() +
  labs(y = "Damage", x = "Cage Type") +
  theme_minimal()
ggsave(file.path(plot_dir, "vertebrate_damage.png"), width = 8, height = 6)

ggplot(total_damage, aes(x = cage, y = total_ins_dam, fill = species)) +
  geom_boxplot() +
  labs(y = "Damage", x = "Cage Type") +
  theme_minimal()
ggsave(file.path(plot_dir, "insect_damage.png"), width = 8, height = 6)
cat("Boxplots saved as vertebrate_damage.png and insect_damage.png in", plot_dir, "\n")

# --- New Recommendation: Explore Temporal Patterns ---
# Analyze mean vertebrate damage by day and cage
cat("\nMean vertebrate damage by day and cage:\n")
temporal_summary <- df_data_original %>%
  group_by(day, cage) %>%
  summarise(mean_vert = mean(total_animal_damage, na.rm = TRUE), .groups = "drop")
print(temporal_summary)

# --- Recommendation from Interpretation: Report Integration ---
# Create a Word document table with Wilcoxon test results
doc <- read_docx() %>%
  body_add_par("Wilcoxon Test Results", style = "Normal") %>%
  body_add_table(data.frame(
    Comparison = c("Vert_vs_Ins", "Ins_vs_Fung", "Fung_vs_Vert"),
    P_Unadjusted = sprintf("%.2e", pvals),
    P_Adjusted = sprintf("%.2e", pvals_adjusted_bh)
  ), style = "Table Professional")
print(doc, target = "C:/Users/HP/Desktop/All_Days_analysis/wilcox_results.docx")
cat("\nWord document with Wilcoxon results saved to wilcox_results.docx\n")

# --- New Recommendation: Enhance Report ---
# Note: Incorporate wilcox_results.docx, vertebrate_damage.png, insect_damage.png, damage_summary, and temporal_summary into the report.
cat("Note: Incorporate wilcox_results.docx, vertebrate_damage.png, insect_damage.png, damage_summary, and temporal_summary into the report.\n")
cat("Suggested narrative: 'Wilcoxon tests confirmed vertebrates > insects > fungi (p < 0.001). Anthonotha experienced the highest vertebrate damage in Opened cages, supporting the species effect (Anthonotha > Isolona > Newtonia). No habitat effect was found (p = 0.6771). Boxplots and summaries highlight cage and species effects.'\n")

# Save workspace
save.image("damage_analysis.RData")