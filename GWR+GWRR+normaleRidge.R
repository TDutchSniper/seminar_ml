# install.packages("RColorBrewer")
# install.packages("GWmodel")
# install.packages("readxl")
# install.packages("sf")
#install.packages("tidyverse")
#install.packages("caret")
#install.packages("dplyr)
#install.packages("SpatialML")



library(RColorBrewer)
library(GWmodel)
library(readxl)
library(sf)
library(MASS)
library(tidyverse)
library(caret)
library(ISLR)
library(dplyr)
library(SpatialML)
library(caret)

# Set working directory
setwd("C:/Users/semva/OneDrive/Documenten/Study/Seminar/Reproductie")

# Load data
data <- read_excel("C:/Users/wgcam/Downloads/Main_dataset_final.xlsx")


#Scaling continuous X variables:
data[, c(11, 12, 14, 19, 25, 38, 40, 41, 42, 46, 50, 54, 58, 62, 66, 70)] <- scale(data[,c(11, 12, 14, 19, 25, 38, 40, 41, 42, 46, 50, 54, 58, 62, 66, 70)])

data_use <- data
data_use <- data[, c(4, 6, 9, 11, 12, 14, 19, 25, 26, 28, 29, 30, 31,38, 40, 41, 42, 46, 50, 54, 58, 62, 66, 70)]


# Plot data
plot(data_use)

####################################
# Splitting Data into training, testing and validation
set.seed(123)
train_control <- trainControl(method = "cv", 
                              number = 5)

model <- train(log_price ~ house_dummy + building_dummy + room + bedroom + living_area + house_age + floor +
                 floor_dummy_missing + balcony + garden + safety_index + physical_index_objective + social_index + traveltime_primary_school +
                 traveltime_night_club + traveltime_subway_station + traveltime_gym + traveltime_supermarket + traveltime_bus_station + traveltime_park + traveltime_stadhuis,
               data = data_use, method = "ridge", trControl = train_control )

print(model)

model2 <- train(log_price ~ house_dummy + building_dummy + room + bedroom + living_area + house_age + floor +
                  floor_dummy_missing + balcony + garden + safety_index + physical_index_objective + social_index + traveltime_primary_school +
                  traveltime_night_club + traveltime_subway_station + traveltime_gym + traveltime_supermarket + traveltime_bus_station + traveltime_park + traveltime_stadhuis,
                data = data_use, method = "lm", trControl = train_control )
print(model2)

#
# Global Ridge Regression
fit <- lm.ridge(log_price ~ house_dummy + building_dummy + room + bedroom + living_area + house_age + floor +
                  floor_dummy_missing + balcony + garden + safety_index + physical_index_objective + social_index + traveltime_primary_school +
                  traveltime_night_club + traveltime_subway_station + traveltime_gym + traveltime_supermarket + traveltime_bus_station + traveltime_park + traveltime_stadhuis,
                data = data_use, lambda = 0.01)

# Regression: ADJUST THE REGRESSION HERE
regression <- lm(log_price ~ house_dummy + building_dummy + room + bedroom + living_area + house_age + floor +
                   floor_dummy_missing + balcony + garden + safety_index + physical_index_objective + social_index + traveltime_primary_school +
                   traveltime_night_club + traveltime_subway_station + traveltime_gym + traveltime_supermarket + traveltime_bus_station + traveltime_park + traveltime_stadhuis,
                 data = data_use)

###############################################################
# Convert to spatial data, using latitute and longitude
coordinates(data_use) <- ~latitude + longitude
# Distance matrix
coordinates <- coordinates(data_use)
dMat <- gw.dist(coordinates, focus = 0)

# Bandwidth selection
bw <- bw.gwr(formula=regression, data=data_use, approach = "CV", kernel = "gaussian", dMat = dMat)

# GWR
gwr <- gwr.basic(formula=regression, data=data_use, bw = bw, kernel = "gaussian", dMat = dMat)
print(gwr)

# For Ridge define lambda DOES NOT WORK -> Lambda moet nog op een manier gekozen worden + Gaussian bevestigen
# https://rdrr.io/cran/GWmodel/src/R/bw.sel.r
# vs
# https://rdrr.io/cran/GWmodel/src/R/collinearity.r
lambda <- 0.01
bwgwrlcr <- bw.gwr.lcr(formula=regression, data=data_use, kernel = "gaussian", dMat = dMat, lambda = lambda)
gwrlcr <- gwr.lcr(formula=regression, data=data_use, bw = bwgwrlcr, lambda = lambda, 
                   kernel = "gaussian", dMat = dMat)
print(gwrlcr)



## ATTEMPT TO PLOT IN ONE FIGURE
# Wijken <- st_read("./shapefile/wijkbuurtkaart_2023/wijken_2023_v1.shp")
# Wijkcodes <- c("WK059901", "WK059903", "WK059904", "WK059905", "WK059906", "WK059906",
#                "WK059908", "WK059910", "WK059912", "WK059914", "WK059915")
# Wijken <- Wijken[Wijken$WK_CODE %in% Wijkcodes,]
# 
# # data_sf <- gwr$SDF
# # data_sf <- st_as_sf(data_sf)
# # crs <- st_crs(Wijken)
# # 
# # # Assign the CRS to Points
# # proj_points <- st_set_crs(data_sf, crs)
# # joined <- st_join(Wijken, proj_points, join = st_within, left = FALSE)
# # 
# # plot(joined, max.plot = 79)





#-------------------------------------------------------------------------------
#K fold cross validation with GWRR:

set.seed(123) # For reproducibility
n <- nrow(data_use)
k <- 5 # Number of folds
fold_indices <- sample(1:k, n, replace = TRUE)
folds <- split(1:n, fold_indices)



predictions_r <- list()
mse_r <- vector("numeric",length = k)
rmse_r <- vector("numeric",length = k)
R_squared_r <- vector("numeric", length = k)

for (i in 1:k) {
  print(paste0("fold: ",i,"/",k))
  test_indices <- folds[[i]]
  train_indices <- setdiff(1:n, test_indices)
  
  train_data <- data_use[train_indices, ]
  test_data <- data_use[test_indices, ]
  
  # Fit GWR model on training data.
  # Distance matrix train
  coordinates_train <- coordinates(train_data)
  dMat_train <- gw.dist(coordinates_train, focus = 0)
  
  lambdak <- 0.01
  bwgwrlcrk <- bw.gwr.lcr(formula=regression, data=train_data, kernel = "gaussian", dMat = dMat_train, lambda = lambdak)

  # Predict on test set
  #gwr.predict doesnt take ridge into account; do predictions manually :(
  gwr.fit.k <- gwr.lcr(formula= regression, data=train_data, bw = bwgwrlcrk, lambda = lambdak, 
                       kernel = "gaussian", dMat = dMat_train)
  
  gwr.pred.k <- gwr.predict(formula = regression, data = train_data, predictdata = test_data, bw = bwgwrlcrk, 
                                        kernel = "gaussian", adaptive = FALSE, p = 2, theta = 0, dMat2 = dMat_train)
  predictions <- gwr.pred.k$SDF$prediction
  
  
  # Store results, e.g., predictions and maybe calculate some accuracy metric
  predictions_r[[i]] <- predictions
  
  #Calculating Mean squared errors using predictions:
  errors <- vector("numeric", length = length(predictions))
  for (j in 1:length(predictions)) {
    errors[j] = (test_data$log_price[j] - predictions[j])^2
  }
  mse_r[i] <- mean(errors)
  rmse_r[i] <- sqrt(mse_r[i])
  
  #Calculating R squared for each fold and fit:
  residuals <- gwr.pred.k$GW.arguments$formula$residuals
  SSR <- sum(residuals^2)
  diff <- train_data$log_price - mean(train_data$log_price)
  SST <- sum(diff^2)
  R_squared_r[i] <- 1 - SSR/SST
}

print("finished")

# Assess model performance across all folds, e.g., via RMSE, R-squared, etc.
final_rmse_r <- mean(rmse_r)
final_mse_r <- mean(mse_r)
final_R_squared_r <- mean(R_squared_r)


#-------------------------------------------------------------------------------  





  
#-------------------------------------------------------------------------------
#K fold cross validation with GWR WITHOUT ridge:

set.seed(815147) # For reproducibility
n <- nrow(data_use)
k <- 10 # Number of folds
fold_indices <- sample(1:k, n, replace = TRUE)
folds <- split(1:n, fold_indices)



predictions_wo <- list()
mse_wo <- vector("numeric",length = k)
rmse_wo <- vector("numeric",length = k)
R_squared_wo <- vector("numeric", length = k)

for (i in 1:k) {
  print(paste0("fold: ",i,"/",k))
  test_indices <- folds[[i]]
  train_indices <- setdiff(1:n, test_indices)
  
  train_data <- data_use[train_indices, ]
  test_data <- data_use[test_indices, ]
  
  # Fit GWR model on training data.
  # Distance matrix train
  coordinates_train <- coordinates(train_data)
  dMat_train <- gw.dist(coordinates_train, focus = 0)
  
  
  bwk <- bw.gwr(formula=regression, data=train_data, approach = "CV", kernel = "gaussian", dMat = dMat_train)

  # Predict on test set
  gwr.pred.k <- gwr.predict(formula = regression, data = train_data, predictdata = test_data, bw = bwk, 
                            kernel = "gaussian", adaptive = FALSE, p = 2, theta = 0, dMat2 = dMat_train)
  predictions <- gwr.pred.k$SDF$prediction
  
  
  # Store results, e.g., predictions and maybe calculate some accuracy metric
  predictions_wo[[i]] <- predictions
  
  #Calculating Mean squared errors using predictions:
  errors <- vector("numeric", length = length(predictions))
  for (j in 1:length(predictions)) {
    errors[j] = (test_data$log_price[j] - predictions[j])^2
  }
  mse_wo[i] <- mean(errors)
  rmse_wo[i] <- sqrt(mse_wo[i])
  
  #Calculating R squared for each fold and fit:
  residuals <- gwr.pred.k$GW.arguments$formula$residuals
  SSR <- sum(residuals^2)
  diff <- train_data$log_price - mean(train_data$log_price)
  SST <- sum(diff^2)
  R_squared_wo[i] <- 1 - SSR/SST
}

print("finished")

# Assess model performance across all folds, e.g., via RMSE, R-squared, etc.
final_rmse_wo <- mean(rmse_wo)
final_mse_wo <- mean(mse_wo)
final_R_squared_wo <- mean(R_squared_wo)
  
#-------------------------------------------------------------------------------






#-------------------------------------------------------------------------------
#Geographical Random Forest

#No K fold so split data into 80% en 20%
set.seed(815147)
data<- read_excel("/Users/whgrefhorst/Documents/Econometrie/Jaar 3/Seminar Machine Learning/Research/Data/main_dataset_metroutes.xlsx")
data_use <- data[, c(4, 6, 9, 11, 12, 14, 19, 25, 26, 28, 29, 30, 31,38, 40, 41, 42, 46, 50, 54, 58, 62, 66, 70)]
n_rows <- nrow(data_use)
n_rows_20_percent <- round(.2*n_rows)

indices_20_percent <- sample(1:n_rows, size = n_rows_20_percent)

df_test <- data[indices_20_percent,]
df_train <- data[-indices_20_percent,]

df_test[, c(11, 12, 14, 19, 25, 38, 40, 41, 42, 46, 50, 54, 58, 62, 66, 70)] <- scale(df_test[,c(11, 12, 14, 19, 25, 38, 40, 41, 42, 46, 50, 54, 58, 62, 66, 70)])
df_test<- df_test[, c(4, 6, 9, 11, 12, 14, 19, 25, 26, 28, 29, 30, 31,38, 40, 41, 42, 46, 50, 54, 58, 62, 66, 70)]

df_train[, c(11, 12, 14, 19, 25, 38, 40, 41, 42, 46, 50, 54, 58, 62, 66, 70)] <- scale(df_train[,c(11, 12, 14, 19, 25, 38, 40, 41, 42, 46, 50, 54, 58, 62, 66, 70)])
df_train<- df_train[, c(4, 6, 9, 11, 12, 14, 19, 25, 26, 28, 29, 30, 31,38, 40, 41, 42, 46, 50, 54, 58, 62, 66, 70)]

coordinates_df_test <- coordinates(df_test)
coordinates_df_train <- coordinates(df_train)

coordinates_df_train<-data.frame(coordinates_df_train[,12],coordinates_df_train[,13])

# # Tuning investigation
# Randomly select 15% of the data for tuning
num_obs_15percent <- round(0.15 * nrow(data))
sampled_indices <- sample(nrow(data), size = num_obs_15percent, replace = FALSE)
tuning_data <- data[sampled_indices, c(4, 6, 9, 11, 12, 14, 19, 25, 26, 28, 29,38, 40, 41, 42, 46, 50, 54, 58, 62, 66, 70)]
train_control <- trainControl(method = "cv",   # k-fold cross-validation
                              number = 5,     # Number of folds
                              verboseIter = TRUE)  # Print progress

tunegrid <- expand.grid(.mtry = (1:21))

grfmethod <- list(type = "Regression",
              library = "SpatialML",
              loop = NULL) 
parameters <- data.frame(parameter = c("mtry", "ntree"),
                  class = rep("numeric", 2),
                  label = c("mtry", "Number of trees"))
grfmethod$parameters <- parameters
grfmethod$grid<-tunegrid

grffit <- function(formula, dframe, bw, kernel, coords, ntree, forests, geo.weighted) { 
  SpatialML::grf(log_price ~ house_dummy + building_dummy + room + bedroom + living_area + house_age + floor +
                   floor_dummy_missing + balcony + garden + safety_index + physical_index_objective + social_index + traveltime_primary_school +
                   traveltime_night_club + traveltime_subway_station + traveltime_gym + traveltime_supermarket + traveltime_bus_station + traveltime_park + traveltime_stadhuis, 
                 dframe = df_train, bw = 600, kernel = "fixed", coords = coordinates_df_train,
                 ntree = 200, forests = TRUE, geo.weighted = TRUE)
}

grfmethod$fit <- grffit

grfPred <- function(modelFit, new.data, preProc = NULL, submodels = NULL)
  SpatialML::predict.grf(modelFit, newdata)
grfmethod$predict <- grfPred
  
prob<-NULL
#_____________________
model <- train(log_price ~ house_dummy + building_dummy + room + bedroom + living_area
               + house_age + floor + floor_dummy_missing + balcony + garden + traveltime_primary_school + safety_index
               + social_index + physical_index_objective + traveltime_night_club + traveltime_subway_station
               + traveltime_gym + traveltime_supermarket + traveltime_bus_station + traveltime_park + traveltime_stadhuis,
               data = df_train,
               method = grfmethod, importance = TRUE, tuneGrid = tunegrid,
               trControl = train_control)

plot(model)
#_____________________
set.seed(815147)
best_bw<-vector()
best_bw[1]=190
best_bw[2]=410
best_bw[3]=580
for( i in 9:21){
print("Tuning for mtry = ") 
print(i)
set.seed(815147)
grf_bw <- grf.bw(regression, dataset = df_train, kernel = "fixed", coords = coordinates_df_train,
                 bw.min = 60, bw.max = 600, step =10, trees = 200, geo.weighted = TRUE, mtry = i)
best_bw[i-4] <- grf_bw$Best.BW
rsquared<- grf_bw$tested.bandwidths
}
best_bw<-c(190,410,580,420, 190,410,580,420)

set.seed(815147)
grf_fit <- grf(log_price ~ house_dummy + building_dummy + room + bedroom + living_area + house_age + floor +
                 floor_dummy_missing + balcony + garden + safety_index + physical_index_objective + social_index + traveltime_primary_school +
                 traveltime_night_club + traveltime_subway_station + traveltime_gym + traveltime_supermarket + traveltime_bus_station + traveltime_park + traveltime_stadhuis, 
               dframe = df_train, bw = 600, kernel = "fixed", coords = coordinates_df_train,
               ntree = 200, forests = TRUE, geo.weighted = TRUE, print.results = TRUE)

#Predict geeft gelijk alleen de predictions:
grf_predictions <- predict.grf(grf_fit, new.data = df_test, x.var.name = "latitude", y.var.name = "longitude")
true_values <- df_test$log_price
errors_grf <- vector("numeric",length = length(true_values))
lengthpredictions<-length(grf_predictions)
for (j in 1:lengthpredictions) {
  errors_grf[j] = (true_values[j] - grf_predictions[j])^2
}

mse_grf <- mean(errors_grf)
rmse_grf <- sqrt(mse_grf)
mse_grf
rmse_grf
