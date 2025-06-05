# Load required libraries
library(ggplot2)

# Access the relevant data from KFP
village <- KFP$vertex.static.attrs$village  # Village labels
toa <- KFP$toa  # Time of adoption (TOA)

# Create a data frame with village and toa information
data <- data.frame(village = village, toa = toa)

# Count the number of cases for each TOA (1 to 11) in each village
data_summary <- aggregate(
  x = list(Cases = data$toa),
  by = list(village = data$village, toa = data$toa),
  FUN = length
)

# Ensure all combinations of villages and TOA are present (fill missing combinations with 0)
all_combinations <- expand.grid(
  village = unique(data$village),
  toa = 1:11
)
data_summary <- merge(all_combinations, data_summary, by = c("village", "toa"), all.x = TRUE)
data_summary$Cases[is.na(data_summary$Cases)] <- 0

# Define the output directory for saving PNGs
output_dir <- "C:/Users/Usuario/Desktop/Doctorado/IV 2024/A - Proyecto de Tesis/2do paper/"

# Loop through each village and save a PNG for each time step
villages <- unique(data_summary$village)
for (v in villages) {
  # Filter data for the current village
  village_data <- subset(data_summary, village == v)
  
  # Create the plot for the current village
  p <- ggplot(village_data, aes(x = factor(toa), y = Cases, fill = factor(toa))) +
    geom_bar(stat = "identity") +
    labs(
      title = paste("Number of Cases by TOA in Village:", v),
      x = "Time of Adoption (TOA)",
      y = "Number of Cases"
    ) +
    scale_y_continuous(limits = c(0, 20)) + # Fix y-axis limit to 20
    theme_minimal() +
    theme(legend.position = "none") # Remove legend for simplicity
  
  # Save the plot as a PNG file
  png_filename <- paste0(output_dir, "village_", v, ".png")
  ggsave(png_filename, plot = p, width = 7, height = 5)
}


# Condensed plot


# Line 1: Load necessary libraries (ggplot2 for plotting, dplyr & tidyr for data manipulation)
library(ggplot2); library(dplyr); library(tidyr) # library(netdiffuser) # Ensure netdiffuser is loaded if KFP comes from it

# Line 2: Prepare the data summary in a single chained command
data_summary <- data.frame(village = KFP$vertex.static.attrs$village, toa = KFP$toa) %>%
  filter(!is.na(toa) & toa >= 1 & toa <= 11) %>% # Consider only valid TOAs in the 1-11 range
  count(village, toa, name = "Cases") %>%        # Count cases per village and TOA
  tidyr::complete(village = unique(KFP$vertex.static.attrs$village), toa = 1:11, fill = list(Cases = 0)) # Ensure all village/TOA combinations (1-11) exist, filling NAs with 0

# Line 3: Create and print the faceted plot in a single command
print(ggplot(data_summary, aes(x = factor(toa), y = Cases, fill = factor(toa))) + geom_bar(stat = "identity") + facet_wrap(~village) + scale_y_continuous(limits = c(0, 20)) + theme_minimal() + theme(legend.position = "none") + labs(x = "Time of Adoption (TOA)", y = "Number of Cases", title = "Cases by TOA Across Villages"))
