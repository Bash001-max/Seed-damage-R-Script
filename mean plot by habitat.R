# ============================================================
#   SEED REMOVAL ANALYSIS — FINAL VERSION (Red/Blue Colours + Solid Axes)
#   Plot: Species on X-axis | Bars colored by Habitat
#   Aggregation: Site-level means → Habitat-level means
# ============================================================

rm(list = ls())

library(dplyr)
library(flextable)
library(officer)
library(ggplot2)

# ------------------------------------------------------------
# Load data
# ------------------------------------------------------------
data_path <- "C:/Users/DELL/Desktop/All_Days_analysis/data/Original_data_restructured_csv.csv"
data_dir <- dirname(data_path)
setwd(data_dir)

original.data <- read.csv(basename(data_path))

# ------------------------------------------------------------
# Clean up habitat labels
# ------------------------------------------------------------
original.data$habitat <- tolower(trimws(original.data$habitat))
original.data$habitat <- ifelse(grepl("edge", original.data$habitat), "Forest Edge",
                                ifelse(grepl("core", original.data$habitat), "Forest Core",
                                       original.data$habitat))

# ------------------------------------------------------------
# STEP 1: Mean per site (opened cages only)
# ------------------------------------------------------------
site_means <- original.data %>%
  filter(cage == "opened_cage") %>%
  group_by(species, habitat, site) %>%
  summarise(
    Site_Mean_Removal = mean(nr_seeds_remov, na.rm = TRUE)
  ) %>%
  ungroup()

# ------------------------------------------------------------
# STEP 2: Average those site means per species × habitat
# ------------------------------------------------------------
table_data <- site_means %>%
  group_by(species, habitat) %>%
  summarise(
    Mean_Seed_Removal = mean(Site_Mean_Removal, na.rm = TRUE),
    SE_Seed_Removal = sd(Site_Mean_Removal, na.rm = TRUE) /
      sqrt(sum(!is.na(Site_Mean_Removal)))
  ) %>%
  ungroup() %>%
  mutate(
    Mean_Seed_Removal = round(Mean_Seed_Removal, 2),
    SE_Seed_Removal = round(SE_Seed_Removal, 2)
  )

# ------------------------------------------------------------
# Replace species codes with full scientific names
# ------------------------------------------------------------
table_data$species <- recode(table_data$species,
                             "Anthonotha" = "Anthonotha noldeae",
                             "Isolona" = "Isolona deightonii",
                             "Newtonia" = "Newtonia buchananii"
)

# ------------------------------------------------------------
# Visualization — sharp blue/red, solid black axes, no gridlines/title
# ------------------------------------------------------------
habitat_colors <- c("Forest Edge" = "blue", "Forest Core" = "red")

p <- ggplot(table_data, aes(x = species, y = Mean_Seed_Removal, fill = habitat)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  geom_errorbar(aes(ymin = Mean_Seed_Removal - SE_Seed_Removal,
                    ymax = Mean_Seed_Removal + SE_Seed_Removal),
                width = 0.2,
                position = position_dodge(width = 0.8)) +
  scale_fill_manual(values = habitat_colors, drop = FALSE) +
  scale_x_discrete(
    labels = c(
      expression(italic("Anthonotha noldeae")),
      expression(italic("Isolona deightonii")),
      expression(italic("Newtonia buchananii"))
    )
  ) +
  labs(x = "Species", y = "Mean Seed Removal (± SE)", fill = "Habitat") +
  theme_minimal(base_family = "Times New Roman") +
  theme(
    panel.grid = element_blank(),
    axis.text = element_text(size = 12, color = "black"),
    axis.title = element_text(size = 13, color = "black"),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 11),
    plot.title = element_blank(),
    axis.line = element_line(color = "black", linewidth = 0.8),   # Solid black axes
    axis.ticks = element_line(color = "black", linewidth = 0.8),  # Solid black ticks
    panel.border = element_blank()                                # No box border
  )

# ------------------------------------------------------------
# Save plot
# ------------------------------------------------------------
plot_dir <- "C:/Users/DELL/Desktop/All_Days_analysis/plots/mean_plots"
if (!dir.exists(plot_dir)) dir.create(plot_dir, recursive = TRUE)

ggsave(file.path(plot_dir, "Overall_Mean_Seed_Removal_Species_Habitat_RedBlue_Italic_SolidAxes.png"),
       plot = p, width = 8, height = 6, dpi = 300)

cat("✅ Plot saved successfully with solid black axes and italicized species names.\n")
cat("Check CSV file for numeric confirmation of each species' mean values.\n")
