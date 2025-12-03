# ============================================================
#   SEED REMOVAL ANALYSIS — OPTION B (Clean Layout, Solid Axes)
#   Plot: Site on X-axis | Bars colored by Species
#   Solid black x/y axis lines, no gridlines, no title
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
# Summarize by site × species (opened cages only)
# ------------------------------------------------------------
table_data <- original.data %>%
  filter(cage == "opened_cage") %>%
  group_by(site, species) %>%
  summarise(
    Mean_Seed_Removal = mean(nr_seeds_remov, na.rm = TRUE),
    SE_Seed_Removal = sd(nr_seeds_remov, na.rm = TRUE) /
      sqrt(sum(!is.na(nr_seeds_remov)))
  ) %>%
  ungroup() %>%
  mutate(
    Mean_Seed_Removal = round(Mean_Seed_Removal, 2),
    SE_Seed_Removal = round(SE_Seed_Removal, 2)
  )

# ------------------------------------------------------------
# Save summary table
# ------------------------------------------------------------
write.csv(
  table_data,
  file = file.path(data_dir, "mean_seed_removal_site_species.csv"),
  row.names = FALSE
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
# Visualization — solid axes, no gridlines, clean look
# ------------------------------------------------------------
species_colors <- c(
  "Anthonotha noldeae" = "red",
  "Isolona deightonii" = "blue",
  "Newtonia buchananii" = "green"
)

p <- ggplot(table_data, aes(x = site, y = Mean_Seed_Removal, fill = species)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  geom_errorbar(aes(ymin = Mean_Seed_Removal - SE_Seed_Removal,
                    ymax = Mean_Seed_Removal + SE_Seed_Removal),
                width = 0.2,
                position = position_dodge(width = 0.8)) +
  scale_fill_manual(values = species_colors,
                    labels = c(
                      expression(italic("Anthonotha noldeae")),
                      expression(italic("Isolona deightonii")),
                      expression(italic("Newtonia buchananii"))
                    )) +
  labs(x = "Site", y = "Mean Seed Removal (± SE)", fill = "Species") +
  theme_minimal(base_family = "Times New Roman") +
  theme(
    panel.grid = element_blank(),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 13),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 11),
    plot.title = element_blank(),
    # --- new additions below ---
    axis.line = element_line(color = "black", linewidth = 0.8),
    axis.ticks = element_line(color = "black", linewidth = 0.8)
  )

# ------------------------------------------------------------
# Save plot
# ------------------------------------------------------------
plot_dir <- "C:/Users/DELL/Desktop/All_Days_analysis/plots/mean_plots"
if (!dir.exists(plot_dir)) dir.create(plot_dir, recursive = TRUE)

ggsave(file.path(plot_dir, "Mean_Seed_Removal_By_Site_Species_SolidAxes.png"),
       plot = p, width = 8, height = 6, dpi = 300)

cat("✅ Plot saved successfully with solid black x/y axes and clean layout.\n")
