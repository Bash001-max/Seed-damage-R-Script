# Clear environment
rm(list = ls())

# Load required packages
library(dplyr)
library(papaja)

# Data directory
data_dir <- "C:/Users/HP/Desktop/All_Days_analysis/data"
if (!dir.exists(data_dir)) {
  dir.create(data_dir, recursive = TRUE)
  cat("Created directory:", data_dir, "\n")
}
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

# Filter for opened cages and specific days
plot_data <- graph.data %>%
  filter(cage == "opened_cage", day %in% c(2, 5, 10))

# Display results in the R output window
cat("Mean Seed Removal by Day, Habitat, and Species (Opened Cages)\n")
cat("------------------------------------------------------------\n")
cat(sprintf("%-5s %-12s %-12s %-10s %-10s\n", "Day", "Habitat", "Species", "Mean", "Confint"))
cat("------------------------------------------------------------\n")
for (i in 1:nrow(plot_data)) {
  cat(sprintf("%-5d %-12s %-12s %-10.2f %-10.2f\n", 
              plot_data$day[i], 
              plot_data$habitat[i], 
              plot_data$species[i], 
              plot_data$Mean[i], 
              plot_data$confint[i]))
}
cat("------------------------------------------------------------\n")