## Clear environment
rm(list = ls())

# Load required packages
library(dplyr)
library(ggplot2)
library(papaja)

# Set input data directory
data_dir <- "C:/Users/DELL/Desktop/All_Days_analysis/data"

# Set output directory for plots
plot_dir <- "C:/Users/DELL/Desktop/All_Days_analysis/plots"

# Create plot directory if it does not exist
if (!dir.exists(plot_dir)) {
  dir.create(plot_dir, recursive = TRUE)
  cat("Created directory:", plot_dir, "\n")
}

# Create data directory if it does not exist
if (!dir.exists(data_dir)) {
  dir.create(data_dir, recursive = TRUE)
  cat("Created directory:", data_dir, "\n")
}

# Set working directory to data folder
setwd(data_dir)

# Load data
original.data <- read.csv("Original_data_restructured_csv.csv")
str(original.data)

# Calculate means
data_mean <- original.data %>%
  group_by(day, habitat, species, cage) %>%
  summarise_at(vars(nr_seeds_remov), list(Mean = mean), na.rm = TRUE)

# Calculate 95% confidence intervals
data.95 <- wsci(data = original.data, id = "id", dv = "nr_seeds_remov", 
                factors = c("day", "habitat", "species", "cage"), level = 0.95, method = "Morey")

# Merging data
graph.data <- merge(data_mean, data.95)
names(graph.data)[names(graph.data) == "nr_seeds_remov"] <- "confint"
graph.data$Mean <- as.numeric(graph.data$Mean)
graph.data$confint <- as.numeric(graph.data$confint)

# Debugging: Inspect graph.data
str(graph.data)
print(head(graph.data))
print(unique(graph.data$species))  # ["Anthonotha", "Isolona", "Newtonia"]
print(unique(graph.data$cage))    # ["large_meshed_cage", "opened_cage", "small_meshed_cage"]
print(unique(graph.data$habitat)) # Check habitat levels

# Ensure habitat is factor with correct levels
graph.data$habitat <- factor(graph.data$habitat, levels = c("Forest_Edge", "Forest_Core"))

# Defining habitat colors
habitat_colors <- c("Forest_Edge" = "#008df9", "Forest_Core" = "#e20134")

# Function to create a faceted plot for a given cage type
create_cage_facet_plot <- function(data, cage_type) {
  if (!is.data.frame(data)) stop("Input 'data' must be a data frame")
  
  plot_data <- data %>% 
    filter(cage == cage_type)
  
  cat("Rows in plot_data for", cage_type, ":", nrow(plot_data), "\n")
  cat("Habitats in plot_data:", unique(plot_data$habitat), "\n")
  
  if (nrow(plot_data) == 0) {
    cat("No data for", cage_type, "- skipping plot\n")
    return(NULL)
  }
  
  cage_title <- switch(cage_type,
                       "opened_cage" = "Opened Cages",
                       "large_meshed_cage" = "Large Meshed Cages",
                       "small_meshed_cage" = "Small Meshed Cages",
                       cage_type)
  
  ggplot(plot_data, aes(x = day, y = Mean, colour = habitat)) +
    geom_errorbar(aes(ymin = Mean - confint, ymax = Mean + confint), width = 0.5,
                  linewidth = 0.6) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 1.5) +
    facet_wrap(~ species, ncol = 3, scales = "fixed") +
    theme_light() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14, family = "Times New Roman"),
      strip.text = element_text(face = "italic", size = 10, family = "Times New Roman"),
      strip.background = element_rect(fill = "lightgray"),
      panel.spacing = unit(1, "lines"),
      axis.title = element_text(family = "Times New Roman"),
      axis.text = element_text(family = "Times New Roman"),
      legend.title = element_text(family = "Times New Roman"),
      legend.text = element_text(family = "Times New Roman")
    ) +
    scale_colour_manual(values = habitat_colors, drop = FALSE) +
    scale_y_continuous(limits = c(0, 25), expand = expansion(mult = c(0, 0))) +
    ylab("Mean Seed Removal") +
    xlab("Days") +
    ggtitle(cage_title) +
    labs(colour = "Habitat")
}

# Function for individual plots
create_cage_plot <- function(data, species_name, cage_type) {
  plot_data <- data %>% 
    filter(species == species_name, cage == cage_type)
  
  cat("Rows in plot_data for", species_name, cage_type, ":", nrow(plot_data), "\n")
  cat("Habitats in plot_data:", unique(plot_data$habitat), "\n")
  cat("Data preview for", species_name, cage_type, ":\n")
  print(head(plot_data))  # Added for debugging
  
  if (nrow(plot_data) == 0) {
    cat("No data for", species_name, cage_type, "- skipping plot\n")
    return(NULL)
  }
  
  cage_title <- switch(cage_type,
                       "opened_cage" = "Opened Cages",
                       "large_meshed_cage" = "Large Meshed Cages",
                       "small_meshed_cage" = "Small Meshed Cages",
                       cage_type)
  
  ggplot(plot_data, aes(x = day, y = Mean, colour = habitat)) +
    geom_errorbar(aes(ymin = Mean - confint, ymax = Mean + confint), width = 0.5,
                  linewidth = 0.6) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 1.5) +
    theme_light() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.title = element_text(hjust = 0.5, face = "italic", size = 12, family = "Times New Roman"),
      axis.title = element_text(family = "Times New Roman"),
      axis.text = element_text(family = "Times New Roman"),
      legend.title = element_text(family = "Times New Roman"),
      legend.text = element_text(family = "Times New Roman")
    ) +
    scale_colour_manual(values = habitat_colors, drop = FALSE) +
    scale_y_continuous(limits = c(0, 25), expand = expansion(mult = c(0, 0))) +
    ylab("Mean Seed Removal") +
    xlab("Days") +
    ggtitle(paste(species_name, "-", cage_title)) +
    labs(colour = "Habitat")
}

# Create faceted plots
cage_types <- c("opened_cage", "large_meshed_cage", "small_meshed_cage")

# Loop to generate, display, and save faceted plots
for (cage in cage_types) {
  p <- create_cage_facet_plot(graph.data, cage)
  if (!is.null(p)) {
    print(p) # Display plot in RStudio
    filename <- paste0("seed_removal_faceted_", cage, ".jpg")
    tryCatch({
      ggsave(filename, plot = p, path = plot_dir,
             width = 23, height = 14, units = "cm", dpi = 300, limitsize = TRUE)
      cat("Saved faceted plot:", file.path(plot_dir, filename), "\n")
    }, error = function(e) {
      cat("Error saving faceted plot for", cage, ":", e$message, "\n")
    })
  } else {
    cat("Skipping faceted plot for", cage, "- no plot generated\n")
  }
}

# Create and save individual plots
individual_plots <- list(
  Anthonotha_open = list(plot = create_cage_plot(graph.data, "Anthonotha", "opened_cage"), 
                         name = "Anthonotha_opened_cage"),
  Anthonotha_large = list(plot = create_cage_plot(graph.data, "Anthonotha", "large_meshed_cage"), 
                          name = "Anthonotha_large_meshed_cage"),
  Anthonotha_small = list(plot = create_cage_plot(graph.data, "Anthonotha", "small_meshed_cage"), 
                          name = "Anthonotha_small_meshed_cage"),
  Newtonia_open = list(plot = create_cage_plot(graph.data, "Newtonia", "opened_cage"), 
                       name = "Newtonia_opened_cage"),
  Newtonia_large = list(plot = create_cage_plot(graph.data, "Newtonia", "large_meshed_cage"), 
                        name = "Newtonia_large_meshed_cage"),
  Newtonia_small = list(plot = create_cage_plot(graph.data, "Newtonia", "small_meshed_cage"), 
                        name = "Newtonia_small_meshed_cage"),
  Isolona_open = list(plot = create_cage_plot(graph.data, "Isolona", "opened_cage"), 
                      name = "Isolona_opened_cage"),
  Isolona_large = list(plot = create_cage_plot(graph.data, "Isolona", "large_meshed_cage"), 
                       name = "Isolona_large_meshed_cage"),
  Isolona_small = list(plot = create_cage_plot(graph.data, "Isolona", "small_meshed_cage"), 
                       name = "Isolona_small_meshed_cage")
)

# Loop to generate, display, and save individual plots
for (plot_info in individual_plots) {
  p <- plot_info$plot
  name <- plot_info$name
  if (!is.null(p)) {
    print(p) # Display plot in RStudio
    filename <- paste0("seed_removal_", name, ".jpg")
    tryCatch({
      ggsave(filename, plot = p, path = plot_dir,
             width = 23, height = 14, units = "cm", dpi = 300, limitsize = TRUE)
      cat("Saved individual plot:", file.path(plot_dir, filename), "\n")
    }, error = function(e) {
      cat("Error saving individual plot for", name, ":", e$message, "\n")
    })
  } else {
    cat("Skipping individual plot for", name, "- no plot generated\n")
  }
}


