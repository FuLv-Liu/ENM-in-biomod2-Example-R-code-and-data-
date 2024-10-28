library(biomod2)
library(raster)
library(dismo)
library(tidyverse)
#Make sure R has access to some folders
# Load occurrence
csv_path <- 'Aconitum heterophyllum_thin1.csv'  # 
occ_data <- read.csv(csv_path)
head(occ_data)
occ_sp <- SpatialPoints(occ_data[, 2:3], CRS('EPSG:4326'))

# Load environmental variables
rst_current <- list.files('Current_autocorrelated/', '.tif$', full.names = T)
rst_current <- raster::stack(rst_current)
plot(rst_current[[2]])
plot(occ_sp, add=T)

# (generate pseudoabsces)
pseudo_n <- 1000
pa <- randomPoints(rst_current[[1]], pseudo_n, occ_sp)
colnames(pa) <- c('decimalLongitude', 'decimalLatitude')
plot(SpatialPoints(pa, CRS('EPSG:4326')), add=T, col='blue')
occ_all <- bind_rows(occ_data[, 2:3], data.frame(pa))

# Format Data with true-absences
myBiomodData <- BIOMOD_FormatingData(resp.name = 'Aconitum heterophyllum',  #
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
                                    CV.nb.rep = 2,
                                    CV.perc = 0.7,
                                    # data.split.table = myBiomodCV,
                                    var.import = 3,
                                    metric.eval = c('TSS','ROC'),
                                    models = c("GBM", "CTA", "RF",
                                    "MAXNET", "GLM","MARS", 
                                    "GAM","ANN","SRE","FDA"),
                                    # do.full.models = FALSE,
                                    nb.cpu=8)
myBiomodModelOut

# Get evaluation scores & variables importance
bm_eval <- get_evaluations(myBiomodModelOut)
bm_imp <- get_variables_importance(myBiomodModelOut)
bm_res <- bm_PlotResponseCurves(bm.out = myBiomodModelOut)
#  Write the model performance and variable importance
write_csv(bm_eval, 'Aconitum heterophyllum_eval.csv')  # 
write_csv(bm_imp, 'Aconitum heterophyllum_imp.csv')  # 
write_csv(bm_res$tab, 'Aconitum heterophyllum_ResponseCurves.csv')  #

# Represent evaluation scores & variables importance
bm_PlotEvalMean(bm.out = myBiomodModelOut, c('ROC', 'TSS'))


# Model ensemble models
myBiomodEM <- BIOMOD_EnsembleModeling(bm.mod = myBiomodModelOut,
                                      em.by = 'all',
                                      metric.select = c('ROC', 'TSS'),
                                      metric.select.thresh = c(0.8, 0.6),
                                      var.import = 3,
                                      metric.eval = c('ROC','TSS'),
                                      nb.cpu = 8)
myBiomodEM

# Get evaluation scores & variables importance
em_eval <- get_evaluations(myBiomodEM)
em_imp <- get_variables_importance(myBiomodEM)
em_res <- bm_PlotResponseCurves(bm.out = myBiomodEM)

# Write ensemble model performance and variable importance
write_csv(em_eval, 'Aconitum heterophyllum_eval_EM.csv')  # 
write_csv(em_imp, 'Aconitum heterophyllum_imp_EM.csv')  # 
write_csv(em_res$tab, 'Aconitum heterophyllum_ResponseCurves_EM.csv')  # 

# Project ensemble models (building single projections)
myBiomodEMProj <- BIOMOD_EnsembleForecasting(bm.em = myBiomodEM,
                                             proj.name = 'CurrentEM',
                                             new.env = rst_current,
                                             metric.binary = 'all',
                                             nb.cpu = 8,
                                             do.stack=FALSE)
myBiomodEMProj
plot(myBiomodEMProj)



# Projection --------------------------------------------------------------
# Load environmental variables
rst_l <- c('SSP126_2050_autocorrelated')
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

# Load environmental variables
rst_l <- c('SSP126_2090_autocorrelated')
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

# Load environmental variables
rst_l <- c('SSP585_2050_autocorrelated')
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





# Load environmental variables
rst_l <- c('SSP585_2090_autocorrelated')
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
# 
Sys.time()

