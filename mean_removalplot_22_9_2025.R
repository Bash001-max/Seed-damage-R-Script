# Clear environment
rm(list = ls())

# Load required packages
library(dplyr)
library(flextable)
library(officer)
library(ggplot2)

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

# --- Add overall mean across all days (species level only) ---
overall_means <- original.data %>%
  filter(cage == "opened_cage") %>%
  group_by(species) %>%
  summarise(
    Overall_Mean_Removal = round(mean(nr_seeds_remov, na.rm = TRUE), 2),
    .groups = "drop"
  )

# --- Merge overall means into main table (if needed elsewhere) ---
table_data <- left_join(
  table_data,
  overall_means,
  by = "species"
)

# --- Merge Mean and SE into one column for table output ---
table_data <- table_data %>%
  mutate(`Mean ± SE` = paste0(Mean_Seed_Removal, " ± ", SE_Seed_Removal)) %>%
  select(species, habitat, site, day, `Mean ± SE`, Overall_Mean_Removal)

# --- Save results to CSV ---
table_filename <- "mean_seed_removal_by_species_habitat_site_day.csv"
write.csv(table_data, file = file.path(data_dir, table_filename), row.names = FALSE)
cat("Saved CSV:", file.path(data_dir, table_filename), "\n")

# --- Flextable (optional Word export) ---
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

# -----------------------
# --- Bar chart block ---
# -----------------------


species_label_map <- c(
  "Anthonotha" = "Anthonotha noldeae",
  "Isolona"    = "Isolona deightonii",
  "Newtonia"   = "Newtonia buchananii"
)

# Helper label function: returns expression(italic("Full name")) for ggplot
label_fun <- function(x) {
  sapply(x, function(k) {
    if (!is.null(species_label_map[[k]])) {
      parse(text = paste0("italic('", species_label_map[[k]], "')"))[[1]]
    } else {
      # fallback: show the raw species code if not mapped
      k
    }
  })
}

# Build plot
plot <- ggplot(overall_means, aes(x = species, y = Overall_Mean_Removal, fill = species)) +
  geom_col(width = 0.6, color = "black") +
  scale_x_discrete(labels = label_fun) +
  scale_y_continuous(limits = c(0, 15), expand = expansion(mult = c(0, 0.03))) +
  theme_minimal(base_size = 14, base_family = "Times New Roman") +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),                   # remove grid lines
    axis.title.x = element_text(family = "Times New Roman", size = 14, color = "black"),
    axis.text.x  = element_text(family = "Times New Roman", size = 12, color = "black"),
    axis.title.y = element_text(family = "Times New Roman", size = 14, color = "black"),
    axis.text.y  = element_text(family = "Times New Roman", size = 12, color = "black")
  ) +
  labs(
    x = "Species",
    y = "Overall Mean Seed Removal"
  )

# Save plot
plot_file <- "overall_mean_seed_removal_barplot.png"
ggsave(file.path(data_dir, plot_file), plot, width = 8, height = 6, dpi = 600)
cat("Saved bar chart:", file.path(data_dir, plot_file), "\n")
