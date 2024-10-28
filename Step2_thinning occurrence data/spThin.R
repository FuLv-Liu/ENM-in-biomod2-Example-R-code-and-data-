

library('spThin')

data = read.csv("occurence data.csv")

# Create a loop to thin multiple species
species_list <- unique(data$species)
for (i in 1:length(species_list)) {
  thinned_dataset <- thin( loc.data = data[data$species == species_list[i],], 
                           lat.col = "decimalLatitude", long.col = "decimalLongitude", 
                           spec.col = "species", 
                           thin.par = 5, reps = 100, 
                           locs.thinned.list.return = TRUE, 
                           write.files = TRUE, 
                           max.files = 1, 
                           out.dir = paste0("thined/", species_list[i], "/"), out.base = paste0("", species_list[i]))
}
