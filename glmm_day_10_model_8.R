# Load required packages
library(dplyr)
library(glmmTMB)
library(car)        # For Type II Wald chi-square tests
library(emmeans)    # For estimated marginal means
library(broom)      # For tidy outputs
library(openxlsx)   # For Excel export

# Load data
data_10day <- read.csv("C:/Users/DELL/Desktop/Day10_analysis/data/day_10_data_restructured_csv.csv")

# Ensure categorical variables are factors
data_10day$site <- as.factor(data_10day$site)
data_10day$species <- as.factor(data_10day$species)
data_10day$habitat <- as.factor(data_10day$habitat)

# Fit GLMM with glmmTMB (Poisson + zero inflation)
model <- glmmTMB(
  nr_seeds_remov ~ species * habitat + (1 | site),
  data = data_10day,
  family = poisson,
  ziformula = ~1
)

# Run Type II ANOVA (correct for glmmTMB models)
anova_res <- Anova(model, type = 2)

# Convert ANOVA results to tidy format
anova_table <- broom::tidy(anova_res) %>%
  rename(Term = term, ChiSq = statistic, Df = df, Pval = p.value) %>%
  mutate(
    Sig = case_when(
      Pval < 0.001 ~ "***",
      Pval < 0.01 ~ "**",
      Pval < 0.05 ~ "*",
      TRUE ~ ""
    )
  )

# Extract estimated marginal means (species and habitat only)
emm_species <- summary(emmeans(model, ~ species))
emm_habitat <- summary(emmeans(model, ~ habitat))

# Helper: format mean ± SE
format_mean_se <- function(df) {
  paste0(round(df$emmean, 2), " ± ", round(df$SE, 2))
}

# Combine emmeans info into one table
means_table <- bind_rows(
  emm_species %>% mutate(term = "species", level = as.character(species), mean_se = format_mean_se(.)),
  emm_habitat %>% mutate(term = "habitat", level = as.character(habitat), mean_se = format_mean_se(.))
)

# Add number of levels and Mean ± SE to ANOVA table
anova_table <- anova_table %>%
  left_join(
    means_table %>% group_by(term) %>%
      summarise(Levels = n(), .groups = "drop"),
    by = c("Term" = "term")
  ) %>%
  left_join(
    means_table %>% group_by(term) %>%
      summarise(`Mean ± SE` = paste(mean_se, collapse = "; "), .groups = "drop"),
    by = c("Term" = "term")
  ) %>%
  select(Term, Levels, Df, ChiSq, Pval, `Mean ± SE`, Sig)

# Print final ANOVA table
print(anova_table)

# ===== Export to Excel =====
wb <- createWorkbook()
addWorksheet(wb, "ANOVA Results")

# Write ANOVA results
writeData(wb, "ANOVA Results", anova_table, startRow = 1)

# Add footnote at the bottom
footnote <- "Note: Results are considered significant at p < 0.05 *, p < 0.01 **, and p < 0.001 ***. Exact p-values are reported where relevant."
writeData(wb, "ANOVA Results", footnote, startRow = nrow(anova_table) + 3)

# Save workbook
saveWorkbook(wb, "C:/Users/DELL/Desktop/Day10_analysis/plots/anova_results.xlsx", overwrite = TRUE)

# ===== Post-hoc Tests (merged, with Mean ± SE) =====

# Helper function to add Mean ± SE info
add_mean_se <- function(df, emm, comp_type) {
  df %>%
    left_join(
      emm %>%
        mutate(mean_se = paste0(round(emmean, 2), " ± ", round(SE, 2))) %>%
        select(contrast = !!sym(names(emm)[1]), mean_se),
      by = c("Contrast" = "contrast")
    ) %>%
    mutate(Comparison_Type = comp_type)
}

# Species pairwise comparisons
emm_species <- emmeans(model, ~ species)
posthoc_species <- pairs(emm_species) %>%
  broom::tidy() %>%
  rename_with(~ "Pval", contains("p.value")) %>%
  rename(
    Contrast = contrast,
    Estimate = estimate,
    SE = std.error
  ) %>%
  mutate(
    Sig = case_when(
      Pval < 0.001 ~ "***",
      Pval < 0.01 ~ "**",
      Pval < 0.05 ~ "*",
      TRUE ~ ""
    )
  ) %>%
  mutate(Comparison_Type = "Species")

# Habitat pairwise comparisons
emm_habitat <- emmeans(model, ~ habitat)
posthoc_habitat <- pairs(emm_habitat) %>%
  broom::tidy() %>%
  rename_with(~ "Pval", contains("p.value")) %>%
  rename(
    Contrast = contrast,
    Estimate = estimate,
    SE = std.error
  ) %>%
  mutate(
    Sig = case_when(
      Pval < 0.001 ~ "***",
      Pval < 0.01 ~ "**",
      Pval < 0.05 ~ "*",
      TRUE ~ ""
    )
  ) %>%
  mutate(Comparison_Type = "Habitat")

# Interaction pairwise comparisons (species within habitat)
emm_interaction <- emmeans(model, ~ species | habitat)
posthoc_interaction <- pairs(emm_interaction) %>%
  broom::tidy() %>%
  rename_with(~ "Pval", contains("p.value")) %>%
  rename(
    Contrast = contrast,
    Estimate = estimate,
    SE = std.error
  ) %>%
  mutate(
    # Extract habitat from the emmeans object and pair with contrasts
    Habitat = rep(levels(data_10day$habitat), each = length(unique(data_10day$species)) * (length(unique(data_10day$species)) - 1) / 2),
    Contrast = paste(Habitat, Contrast, sep = ": "),  # e.g., "Forest_Core: Anthonotha - Newtonia"
    Sig = case_when(
      Pval < 0.001 ~ "***",
      Pval < 0.01 ~ "**",
      Pval < 0.05 ~ "*",
      TRUE ~ ""
    )
  ) %>%
  mutate(Comparison_Type = "Species within Habitat") %>%
  select(Comparison_Type, Contrast, Estimate, SE, Pval, Sig)

# Merge all post-hoc results
posthoc_all <- bind_rows(posthoc_species, posthoc_habitat, posthoc_interaction) %>%
  select(Comparison_Type, Contrast, Estimate, SE, Pval, Sig)

# Add Mean ± SE values (from emmeans)
means_all <- bind_rows(
  as.data.frame(emm_species) %>% mutate(Comparison_Type = "Species"),
  as.data.frame(emm_habitat) %>% mutate(Comparison_Type = "Habitat"),
  as.data.frame(emm_interaction) %>% mutate(Comparison_Type = "Species within Habitat")
) %>%
  mutate(`Mean ± SE` = paste0(round(emmean, 2), " ± ", round(SE, 2))) %>%
  select(Comparison_Type, level = !!sym(names(.)[1]), `Mean ± SE`)

# Join means into post-hoc results
posthoc_all <- posthoc_all %>%
  left_join(means_all, by = "Comparison_Type")

# ===== Export to Excel =====
wb <- createWorkbook()
addWorksheet(wb, "ANOVA Results")
writeData(wb, "ANOVA Results", anova_table)

addWorksheet(wb, "Posthoc Results")
writeData(wb, "Posthoc Results", posthoc_all)

saveWorkbook(wb, "C:/Users/DELL/Desktop/Day10_analysis/plots/anova_results.xlsx", overwrite = TRUE)