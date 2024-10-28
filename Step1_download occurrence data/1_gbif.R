library(rgbif)
library(dplyr)

# Read the CSV file containing the species list
species_data <- read.csv("species list.csv")

# Loop through each species to query distribution points
for (species in species_data$species) {
  # Query the distribution points for the species
  occ_data <- occ_search(scientificName = species)
  
  # Check if there is data
  if (!is.null(occ_data$data) && nrow(occ_data$data) > 0) {
    # Extract data and add species name
    occ_data_df <- as.data.frame(occ_data$data)
    occ_data_df$species <- species
    
    # Save the data as a CSV file, naming the file after the species
    file_name <- paste0(gsub(" ", "_", species), "_occurrences.csv")
    write.csv(occ_data_df, file_name, row.names = FALSE)
    
    message(paste("Data for species:", species, "saved to", file_name))
  } else {
    message(paste("No data found for species:", species))
  }
}
