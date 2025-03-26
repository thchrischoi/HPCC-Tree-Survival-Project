



#####################
## Packages
#####################

library(sf)
library(terra)

#####################
## Data
#####################

setwd('N:/Research/Evangelista/EVANGELISTA GROUP_Data/Projects/NewMexicoHighlands/2025_Work/HPCC_Tree_Survival_Project/GIS Data')

##### Vector
##################

## HPCC Perimeter
HPCC = read_sf('HPCC_Perimeter/HPCC_Perimeter.shp')

## Training Points
training_points = read_sf('Training_Points/Training_Points.shp')


##### Raster
###################

## Single USFS Ortho image
usfs.ortho = terra::mosaic(terra::rast('Orthoimagery/2022_USFS_Imagery/Hermits_Peak/Hermits_Peak/Ortho_HermitsPeak2022_3610554_sw_3.tif'),
terra::rast('Orthoimagery/2022_USFS_Imagery/Hermits_Peak/Hermits_Peak/Ortho_HermitsPeak2022_3610554_sw_4.tif'))

## LiDAR
lidar = terra::rast('2023_LiDAR/Canopy_Height_Model_2023_Raster/HPCC_2023_chm.tif') |>
  terra::crop(usfs.ortho)


#####################
## Create Individual bands
#####################

RED = usfs.ortho$Ortho_HermitsPeak2022_3610554_sw_3_1
GREEN = usfs.ortho$Ortho_HermitsPeak2022_3610554_sw_3_2
BLUE = usfs.ortho$Ortho_HermitsPeak2022_3610554_sw_3_3
NIR = usfs.ortho$Ortho_HermitsPeak2022_3610554_sw_3_4

# Normalized Difference Vegetation Index
NDVI = (NIR - RED)/(NIR + RED)

# Enhanced Vegetation Index
EVI = (2.5*(NIR - RED)) / ((NIR+(6*RED)-(7.5*BLUE)) + 1)

# Soil adjusted vegetation index
SAVI = ((NIR - RED) / (NIR + RED + 0.5)) * 1.5

#####################
## Plot
#####################
plot(NDVI)
plot(HPCC$geometry, add = T)


plot(lidar$Band_1)
plot(HPCC$geometry, add = T)


