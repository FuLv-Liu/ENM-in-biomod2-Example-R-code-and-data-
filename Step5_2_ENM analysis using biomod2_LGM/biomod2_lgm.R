library(biomod2)
library(raster)
library(dismo)
library(tidyverse)

# Aconitum heterophyllum is an example species
# Load occurrence data
csv_path <- 'Aconitum heterophyllum_thin1.csv'  # Modify
occ_data <- read.csv(csv_path)
head(occ_data)
occ_sp <- SpatialPoints(occ_data[, 2:3], CRS('EPSG:4326'))

# Load environmental variables
rst_current <- list.files('Current_autocorrelated/', '.tif$', full.names = TRUE)
rst_current <- raster::stack(rst_current)
plot(rst_current[[2]])
plot(occ_sp, add=TRUE)

# Generate random pseudo-absences
pseudo_n <- 1000
pa <- randomPoints(rst_current[[1]], pseudo_n, occ_sp)
colnames(pa) <- c('decimalLongitude', 'decimalLatitude')
plot(SpatialPoints(pa, CRS('EPSG:4326')), add=TRUE, col='blue')
occ_all <- bind_rows(occ_data[, 2:3], data.frame(pa))

# Format Data with true absences
myBiomodData <- BIOMOD_FormatingData(resp.name = 'Aconitum heterophyllum_LGM',  # Modify
                                     resp.var = c(rep(1, nrow(occ_data)), 
                                                  rep(0, pseudo_n)),
                                     expl.var = rst_current,
                                     resp.xy = occ_all)

# Create default modeling options
myBiomodOptions <- bm_ModelingOptions

# Model single models
myBiomodModelOut <- BIOMOD_Modeling(bm.format = myBiomodData,
                                    bm.options = myBiomodOptions,
                                    modeling.id = 'AllModels',
                                    CV.strategy = 'random',
                                    CV.nb.rep = 10,
                                    CV.perc = 0.7,
                                    # data.split.table = myBiomodCV,
                                    var.import = 3,
                                    metric.eval = c('TSS', 'ROC'),
                                    models = c("GBM", "CTA", "RF",
                                               "MAXNET", "GLM", "MARS", 
                                               "GAM", "ANN", "SRE", "FDA"),
                                    # do.full.models = FALSE,
                                    nb.cpu = 8)
myBiomodModelOut

# Get evaluation scores & variable importance
bm_eval <- get_evaluations(myBiomodModelOut)
bm_imp <- get_variables_importance(myBiomodModelOut)
bm_res <- bm_PlotResponseCurves(bm.out = myBiomodModelOut)
# Write model performance and variable importance
write_csv(bm_eval, 'Aconitum heterophyllum_LGM_eval.csv')  # Modify
write_csv(bm_imp, 'Aconitum heterophyllum_LGM_imp.csv')  # Modify
write_csv(bm_res$tab, 'Aconitum heterophyllum_LGM_ResponseCurves.csv')  # Modify

# Set output device as PNG
#png("ROC_TSS_LGM_pictures.png", width = 462, height = 196)
# pdf("ROC_TSSpictures.pdf")
# Plot EvalMean
#bm_PlotEvalMean(bm.out = myBiomodModelOut, c('ROC', 'TSS'))

# Save and close the PNG device
#dev.off()

# Represent evaluation scores & variable importance
bm_PlotEvalMean(bm.out = myBiomodModelOut, c('ROC', 'TSS'))

# Model ensemble models
myBiomodEM <- BIOMOD_EnsembleModeling(bm.mod = myBiomodModelOut,
                                      em.by = 'all',
                                      metric.select = c('ROC', 'TSS'),
                                      metric.select.thresh = c(0.8, 0.6),
                                      var.import = 3,
                                      metric.eval = c('ROC', 'TSS'),
                                      nb.cpu = 8)
myBiomodEM

# Get evaluation scores & variable importance
em_eval <- get_evaluations(myBiomodEM)
em_imp <- get_variables_importance(myBiomodEM)
em_res <- bm_PlotResponseCurves(bm.out = myBiomodEM)

# Write ensemble model performance and variable importance
write_csv(em_eval, 'Aconitum heterophyllum_LGM_eval_EM.csv')  # Modify
write_csv(em_imp, 'Aconitum heterophyllum_LGM_imp_EM.csv')  # Modify
write_csv(em_res$tab, 'Aconitum heterophyllum_LGM_ResponseCurves_EM.csv')  # Modify

# Project ensemble models (building single projections)
myBiomodEMProj <- BIOMOD_EnsembleForecasting(bm.em = myBiomodEM,
                                             proj.name = 'CurrentEM',
                                             new.env = rst_current,
                                             metric.binary = 'all',
                                             nb.cpu = 8,
                                             do.stack = FALSE)
myBiomodEMProj
plot(myBiomodEMProj)

# Projection --------------------------------------------------------------
# Load environmental variables
rst_l <- c('LGM_autocorrelated')
for (rst in rst_l){
  rst_other <- list.files(rst, '.tif$', full.names = T)
  rst_other <- raster::stack(rst_other)}


myBiomodEMProj <- BIOMOD_EnsembleForecasting(bm.em = myBiomodEM,
                                             proj.name = paste0(basename(rst), '_EM'),
                                             new.env = rst_other,
                                             metric.binary = 'all',
                                             nb.cpu = 8,
                                             do.stack=FALSE)
myBiomodEMProj
plot(myBiomodEMProj)
