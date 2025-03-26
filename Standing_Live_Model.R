



#####################
## Packages
#####################

library(sf)
library(terra)
library(corrplot)

#####################
## Data
#####################

setwd('N:/Research/Evangelista/EVANGELISTA GROUP_Data/Projects/NewMexicoHighlands/2025_Work/HPCC_Tree_Survival_Project/GIS Data')

##### Vector
##################

## HPCC Perimeter
HPCC = read_sf('HPCC_Perimeter/HPCC_Perimeter.shp')

## Training Points
training.points = read_sf('Training_Points/Training_Points.shp')


##### Raster
###################

## Single USFS Ortho image
usfs.ortho = terra::mosaic(terra::rast('Orthoimagery/2022_USFS_Imagery/Hermits_Peak/Hermits_Peak/Ortho_HermitsPeak2022_3610554_sw_3.tif'),
terra::rast('Orthoimagery/2022_USFS_Imagery/Hermits_Peak/Hermits_Peak/Ortho_HermitsPeak2022_3610554_sw_4.tif'))

## LiDAR
CHM = terra::rast('2023_LiDAR/Canopy_Height_Model_2023_Raster/HPCC_2023_chm.tif') |>
  terra::crop(usfs.ortho)


#####################
## Create Individual bands
#####################

RED = usfs.ortho$Ortho_HermitsPeak2022_3610554_sw_3_1
names(RED) <- "RED" ## Rename band to recognizable name

GREEN = usfs.ortho$Ortho_HermitsPeak2022_3610554_sw_3_2
names(GREEN) <- "GREEN"

BLUE = usfs.ortho$Ortho_HermitsPeak2022_3610554_sw_3_3
names(BLUE) <- "BLUE"

NIR = usfs.ortho$Ortho_HermitsPeak2022_3610554_sw_3_4
names(NIR) <- "NIR"

# Normalized Difference Vegetation Index
NDVI = (NIR - RED)/(NIR + RED)
names(NDVI) <- "NDVI"

# Enhanced Vegetation Index
EVI = (2.5*(NIR - RED)) / ((NIR+(6*RED)-(7.5*BLUE)) + 1)
names(EVI) <- "EVI"

# Soil adjusted vegetation index
SAVI = ((NIR - RED) / (NIR + RED + 0.5)) * 1.5
names(SAVI) <- "SAVI"


#####################
## Sampling
#####################

dat = training.points |>
  dplyr::mutate(RED = (terra::extract(RED, training.points)[,2]),
                GREEN = (terra::extract(GREEN, training.points)[,2]),
                BLUE = (terra::extract(BLUE, training.points)[,2]),
                NIR = (terra::extract(NIR, training.points)[,2]),
                NDVI = (terra::extract(NDVI, training.points)[,2]),
                EVI = (terra::extract(EVI, training.points)[,2]),
                SAVI = (terra::extract(SAVI, training.points)[,2]),
                CHM = (terra::extract(CHM, training.points)[,2]))


#####################
## Correlation Plot
#####################

corrplot::corrplot(cor(as.data.frame(dat) |> dplyr::select(RED, 
                                               GREEN,
                                               BLUE,
                                               NIR,
                                               NDVI,
                                               EVI,
                                               SAVI,
                                               CHM)),
                    method="number",shade.col=NA, tl.col="black", tl.srt=45)


#####################
## Modeling
#####################

## Multiple Logistic Regression
## Using this model because we are primarily trying to determine weather a tree is live or dead

logit.dat = training.points |>
  # dplyr::mutate(TreeCondBinary = as.factor(ifelse(TreeCond == "Live", 1, 0))) |> 
  cbind(dat)

model1 <- glm(as.factor(TreeCond) ~ RED + GREEN + BLUE + NIR + NDVI + EVI + SAVI + CHM, data = dat, family = "binomial")
model2 <- glm(as.factor(TreeCond) ~ RED + GREEN + BLUE + EVI + CHM, data = dat, family = "binomial")

summary(model1)
summary(model2)

### Model Validation

car::vif(model2)
#####################
## Plot
#####################
plot(NDVI)
plot(HPCC$geometry, add = T)


plot(CHM$Band_1)
plot(HPCC$geometry, add = T)


