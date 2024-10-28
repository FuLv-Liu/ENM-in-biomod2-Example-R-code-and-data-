library(rinat)
library(tidyverse)

# Download distribution data at the research level
download_filter_inaturalist <-
  function(taxon_name,
           bounds = c(25, 60, 45, 110),
           maxresults = 10000,
           geo = TRUE,
           quality = 'research'){
    print(taxon_name)
    data <- tryCatch({
      get_inat_obs(taxon_name=taxon_name, bounds=bounds,
                   maxresults=maxresults, geo=geo, quality=quality)
    },
    # Return error message
    error = function(e) e[[1]])
  }

######################################################################
# One species
task <- 'Aconitum heterophyllum'
data <- download_filter_inaturalist(task, NULL)
if (!is.character(data)) write_csv(data, paste0(task, '.csv'))

######################################################################
# Batch processing
tasks <- read_csv('species list.csv')
result <- map(tasks$species,
              download_filter_inaturalist)
flag <- map_lgl(result, function(type) !is.character(type))
final_result <- bind_rows(result[flag])
write_csv(final_result, 'tasks_occ.csv')
