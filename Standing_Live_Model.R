



#####################
## Packages
#####################

library(sf)
library(terra)
library(corrplot)
library(caret)  # For confusionMatrix function


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

logit.dat = training.points |>
  # dplyr::mutate(TreeCondBinary = as.factor(ifelse(TreeCond == "Live", 1, 0))) |> 
  cbind(dat)

set.seed(123)  # Ensures reproducibility
split.index <- sample(1:nrow(dat), size = 0.7 * nrow(dat))  # Randomly select 70% of rows

train.data <- logit.dat[split.index, ]  # 70% training data
test.data <- logit.dat[-split.index, ]  # 30% testing data

## Multiple Logistic Regression
## Using this model because we are primarily trying to determine weather a tree is live or dead



model1 <- glm(as.factor(TreeCond) ~ RED + GREEN + BLUE + NIR + NDVI + EVI + SAVI + CHM, data = train.data, family = "binomial")
model2 <- glm(as.factor(TreeCond) ~ RED + GREEN + BLUE + EVI + CHM, data = train.data, family = "binomial")
model3 <- glm(as.factor(TreeCond) ~ RED + NIR + NDVI + EVI + CHM, data = train.data, family = "binomial")
model4 <- glm(as.factor(TreeCond) ~ RED +  EVI + CHM, data = train.data, family = "binomial")



summary(model1)
summary(model2)
summary(model3)
summary(model4)

### Model Validation

test.data = as.data.frame(test.data)

# Predict probabilities on the test data
test.data$PredictedProbs <- predict(model4, newdata = test.data, type = "response")

# Convert probabilities to binary predictions (threshold = 0.5)
test.data$PredictedClass <- ifelse(test.data$PredictedProbs > 0.5, 1, 0)



# Convert to factors for confusion matrix
test.data$TreeCond <- as.factor(test.data$TreeCond)
test.data$PredictedClass <- as.factor(test.data$PredictedClass)

# Generate confusion matrix
conf.matrix <- caret::confusionMatrix(test.data$PredictedClass, test.data$TreeCondBinary)
print(conf.matrix)


#####################
## Plot
#####################
plot(NDVI)
plot(HPCC$geometry, add = T)


plot(CHM$Band_1)
plot(HPCC$geometry, add = T)


