library(spocc)
library(tidyverse)



# Download ALA dataset ----------------------------------------------------------------
# query is case-sensitive. Note that a maximum of 500 records can be returned at once
## "Aconitum heterophyllum" is an example species
ala <- occ(query='Aconitum heterophyllum', from='ala')
ala
ala_data <- ala$ala$data[[1]]

# Extract useful 13 columns
ala_title <- c('uuid', "taxonRank", "family", "genus", "species",
               "country", "stateProvince", "raw_occurrenceRemarks",
               "latitude", "longitude", "year", "month", "basisOfRecord")

ala_final <- ala_data %>%
  # Select required columns
  select(all_of(ala_title)) %>%
  # Remove records with missing latitude and longitude
  filter(!is.na(latitude) & !is.na(longitude)) %>%
  # Remove duplicate latitude and longitude records
  distinct(latitude, longitude, .keep_all = TRUE)

# Save the result
write_csv(ala_final, 'Aconitum heterophyllum_ALA.csv', na='')



# Download IDigBio data -------------------------------------------------------------
# Query is case sensitive
# "Aconitum heterophyllum" is an example species
idigbio <- occ(query='Aconitum heterophyllum', from='idigbio', has_coords = TRUE)
idigbio
idigbio_data <- idigbio$idigbio$data[[1]]

# The 14 columns of information to extract
idigbio_title <- c("datasetid", "taxonrank", "family", "genus", "canonicalname",
                   "longitude", "latitude", "continent", "country",
                   "stateprovince", "county", "locality",
                   "eventdate", "basisofrecord")

idigbio_final <- idigbio_data %>%
  # Select the desired columns
  select(all_of(idigbio_title)) %>%
  # Remove rows with missing latitude or longitude
  filter(!is.na(latitude) & !is.na(longitude)) %>%
  # Remove duplicate coordinates
  distinct(latitude, longitude, .keep_all = TRUE)

# Save the results
write_csv(idigbio_final, 'Aconitum heterophyllum_iDigBio.csv', na='')
