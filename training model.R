#### Missing library loading
if ("pacman" %in% rownames(installed.packages()) == FALSE) {
  install.packages("pacman")
} else {
  pacman::p_load(ranger,e1071,gridExtra,tidyverse,svDialogs,rstudioapi)
}

#### Usefull functions for error ploting
pabserror_split <- function(pred,real,building,z) {
  AELAT <- abs(pred - real)
  errors <- data.frame(abs=AELAT,b=building)
  dens <- density(AELAT, na.rm = TRUE)
  df <- data.frame(x=dens$x, y=dens$y)
  probs <- c(0.75)
  quantiles <- quantile(AELAT, prob=probs)
  df$quant <- factor(findInterval(df$x,quantiles))
  p1 <- ggplot(df, aes(x,y)) + geom_line(size=1.5) + geom_ribbon(aes(ymin=0, ymax=y, fill=quant)) +
    scale_fill_brewer(guide="none") + 
    scale_x_continuous(breaks=seq(0,120,20)) +
    labs(x = "Absolute Error", y = "Density",
         title = paste("Density of global error in",z))
  p2 <- ggplot(errors,aes(abs,fill=b)) + geom_density(aes(y=..density..)) +
    facet_wrap(~b) + xlab("Absolute error") + 
    ggtitle("Densities of absolute error by building") + 
    labs(x = "Absolute Error", y = "Density",fill = "Building")
  p.both <- arrangeGrob(p1, p2)
  grid::grid.draw(p.both)
}

#### building prediction ####
metrics_b <- data.frame(matrix(ncol = 4, nrow = 0))
colnames(metrics_b) <- c("Accuracy","Kappa","Model","Set")

set.seed(123)

#### Ignacio: Brute force approach. Using all the WAPS a model without knowing what
#### it does. High risk of overffiting.
#### Using a method that uses all the WAPS
WAPs <- grep("WAP", names(trainingData), value=T)

## Train set:
Building_Model_ranger <- ranger::ranger(BUILDINGID~., data = trainingData[,c(WAPs,"BUILDINGID")])

Building_Model_ranger_cm_train <- confusionMatrix(trainingData$BUILDINGID, Building_Model_ranger$predictions) 

## Validation set:
Building_Model_ranger_val_pred <- predict(Building_Model_ranger, validationData[,c(WAPs)])

Building_Model_ranger_cm_val <- confusionMatrix(validationData$BUILDINGID, Building_Model_ranger_val_pred[[1]]) 

metrics_b[nrow(metrics_b)+1,] <- c(Building_Model_ranger_cm_train$overall[1:2],"AllWaps","Training")
metrics_b[nrow(metrics_b)+1,] <- c(Building_Model_ranger_cm_val$overall[1:2],"AllWaps","Validation")

#### Model to predict Building-floor with all the WAPs

BF_Model_ranger <- ranger::ranger(BUILDINGID_FLOOR~., data = trainingData[,c(WAPs,"BUILDINGID_FLOOR")]) 

BF_Model_ranger_cm_train <- confusionMatrix(trainingData$BUILDINGID_FLOOR,BF_Model_ranger$predictions)

BF_Model_ranger_pred_val <- predict(BF_Model_ranger,validationData[,c(WAPs)])

#### Accuracy: 0.73, Kappa: 0.70
BF_Model_ranger_cm_val <- confusionMatrix(validationData$BUILDINGID_FLOOR,BF_Model_ranger_pred_val[[1]])

#### Model to predict the Building based on the id's of the three WAPs for which
#### the highest signal is captured. Accuracy:

predictors_train <- grep("TopW",colnames(trainingData))
predictors_val   <- grep("TopW",colnames(validationData))

Building_ModelTop3_ranger <- ranger::ranger(BUILDINGID~., data = trainingData[,c(predictors_train,"BUILDINGID")])

#### Accuracy: 0.97, Kappa: 0.95 p-value < 2.2e-16
Building_ModelTop3_ranger_cm_train <- confusionMatrix(trainingData$BUILDINGID,Building_ModelTop3_ranger$predictions)

Building_ModelTop3_ranger_val_pred <- predict(Building_ModelTop3_ranger,validationData[,c(predictors_validation)])

#### Accuracy: 0.96, Kappa: 0.93, p-value: 2.023e-5
Building_ModelTop3_ranger_cm_val   <- confusionMatrix(validationData$BUILDINGID,Building_ModelTop3_ranger_val_pred$predictions)

metrics_b[nrow(metrics_b)+1,] <- c(Building_ModelTop3_ranger_cm_train$overall[1:2],"Top3Waps","Training")
metrics_b[nrow(metrics_b)+1,] <- c(Building_ModelTop3_ranger_cm_val$overall[1:2],"Top3Waps","Validation")

####
set.seed(123) 

metrics_bf <- metrics_bf <- data.frame(matrix(ncol = 4, nrow = 0))
colnames(metrics_bf) <- c("Accuracy","Kappa","Model","Set")

#### Model to predict Building-floor with all the WAPs

BF_Model_allwaps <- ranger::ranger(BUILDINGID_FLOOR~.,trainingData[,c(WAPs,"BUILDINGID_FLOOR")])

BF_Model_allwaps_cm_train <- confusionMatrix(trainingData$BUILDINGID_FLOOR,BF_Model_allwaps$predictions)

BF_Model_allwaps_pred_val <- predict(BF_Model_allwaps,validationData[,c(WAPs)])

BF_Model_allwaps_cm_val <- confusionMatrix(validationData$BUILDINGID_FLOOR,BF_Model_allwaps_pred_val$predictions)

metrics_bf[nrow(metrics_bf)+1,] <- c(BF_Model_allwaps_cm_train$overall[1:2],"AllWaps","Training")
metrics_bf[nrow(metrics_bf)+1,] <- c(BF_Model_allwaps_cm_val$overall[1:2],"AllWaps","Validation")

#### Model to predict the Building-floor based on the id's of the three WAPs for which
#### the highest signal is captured

BF_ModelTop3_ranger <- ranger::ranger(BUILDINGID_FLOOR~.,trainingData[,c("TopWap1", "TopWap2", "TopWap3",
                                                                                  "BUILDINGID_FLOOR")])

BF_ModelTop3_ranger_cm_train <- confusionMatrix(trainingData$BUILDINGID_FLOOR,BF_ModelTop3_ranger$predictions)

BF_ModelTop3_ranger_pred_val <- predict(BF_ModelTop3_ranger,validationData[,c("TopWap1", "TopWap2", "TopWap3")])


BF_ModelTop3_ranger_cm_val   <- confusionMatrix(validationData$BUILDINGID_FLOOR,BF_ModelTop3_ranger_pred_val$predictions) 

metrics_bf[nrow(metrics_bf)+1,] <- c(BF_ModelTop3_ranger_cm_train$overall[1:2],"Top3Waps","Training")
metrics_bf[nrow(metrics_bf)+1,] <- c(BF_ModelTop3_ranger_cm_val$overall[1:2],"Top3Waps","Validation")

#### Ignacio: In order to create models specific for each building, you should
#### use the WAPS and the predicted building. 


#### latitude prediction ####
metrics_lat <- metrics_lat <- data.frame(matrix(ncol = 5, nrow = 0))
colnames(metrics_lat) <- c("RMSE","Rsquared","MAE","Model","Set")

set.seed(123)

Latitude_Model <- ranger(LATITUDE~., data = trainingData[,c(WAPs,"LATITUDE")])

#### RMSE: 19.0 (meters), Rsquared: 0.92, MAE: 10.9 (meters)
Latitude_Model_pred_train_metrics <- postResample(Latitude_Model$predictions, trainingData$LATITUDE)

Latitude_Model_pred_val <- predict(Latitude_Model, validationData[,c(WAPs)])

#### RMSE: 17.4 (meters), Rsquared: 0.94, MAE: 10.6 (meters)
Latitude_Model_pred_val_metrics <- postResample(Latitude_Model_pred_val$predictions, validationData$LATITUDE)

metrics_lat[nrow(metrics_lat)+1,] <- c(Latitude_Model_pred_train_metrics[1:3],"AllWaps","Training")
metrics_lat[nrow(metrics_lat)+1,] <- c(Latitude_Model_pred_val_metrics[1:3],"AllWaps","Validation")

pabserror_split(Latitude_Model_pred_val$predictions,validationData$LATITUDE,
          validationData$BUILDINGID,"LATITUDE")


##### longitude prediction ####
metrics_lon <- metrics_lon <- data.frame(matrix(ncol = 5, nrow = 0))
colnames(metrics_lon) <- c("RMSE","Rsquared","MAE","Model","Set")

set.seed(123)

Longitude_Model <- ranger(LONGITUDE~., data = trainingData[,c(WAPs,"LONGITUDE")])

#### RMSE: 27.6 (meters), Rsquared: 0.95, MAE: 14.0 (meters)
Longitude_Model_metrics_train <- postResample(Longitude_Model$predictions, trainingData$LONGITUDE)

Longitude_Model_pred_val <- predict(Longitude_Model,validationData[,c(WAPs)])

#### RMSE: 19.1 (meters), Rsquared: 0.98, MAE: 11.9 (meters)
Longitude_Model_metrics_val <-postResample(Longitude_Model_train_metrics_pred_val$predictions,validationData$LONGITUDE)

#lon errors
metrics_lon[nrow(metrics_lon)+1,] <- c(Longitude_Model_metrics_train[1:3],"AllWaps","Training")
metrics_lon[nrow(metrics_lon)+1,] <- c(Longitude_Model_metrics_val[1:3],"AllWaps","Validation")


pabserror_split(Longitude_Model_pred_val$predictions,validationData$LONGITUDE,
                validationData$BUILDINGID,"LONGITUDE")

#### Reading the input file of the blind dataset:

current_folder <- dirname(rstudioapi::getSourceEditorContext()$path)
file <- dlgInput("Please enter the name of the blind dataset file: ", Sys.info()["user"])$res
full <- paste(current_folder,"/",file,sep = "")

if (file.exists(full)) {
  blindData <- read_csv(full)
}else{
  while(file.exists(full)==FALSE) {
    full <- dlgInput("The typed file doesn't exist, please enter the correct name for the file: ", Sys.info()["user"])$res
  }
  blindData <- read_csv(full)
}

blindData <- read_csv(full)

#### Preprocessing and feature engenieering of the blind dataset.

blindData <- drop_allna_cols(drop_allna_rows(preproc(blindData)))
blindData <- blindData[,c(WAPs)]
blindData <- replace_values(blindData)

HighestWap_blind <- names(blindData)[apply(blindData[,c(WAPs)], 1, which.max)]

blindData$Top3Waps <- apply(blindgData[,c(WAPs)], 1, function(x) 
  paste(names(head(sort(x, decreasing = TRUE), 3)), collapse = " "))

blindData <- separate(blindData, Top3Waps, c("TopWap1", "TopWap2","TopWap3"))

blindData$TopWap1 <- factor(blindData$TopWap1, levels = WAPs)
blindData$TopWap2 <- factor(blindData$TopWap2, levels = WAPs)
blindData$TopWap3 <- factor(blindData$TopWap3, levels = WAPs)

#### Applying the models to the blind dataset.

get_floor <- function(vec) {
  d <- mapply(function(x) unlist(strsplit(x, "_"))[2],vec)
  return(d)
}  

BF_allwaps_pred <- predict(BF_Model_allwaps,blindData[,c(WAPs)])
BF_Top3_pred    <- predict(BF_ModelTop3_ranger,blindData[,c(WAPs)])

BF_allwaps <- get_floor(BF_allwaps_pred$predictions)
BF_Top3    <- get_floor(BF_Top3_pred$predictions)

Pred_Lat <- predict(Latitude_Model,blindData[,c(WAPs)])
Pred_Lon <- predict(Longitude_Model,blindData[,c(WAPs)])

results_allwaps <- data.frame(matrix(ncol = 3, nrow = 0))
results_allwaps[,ncol(results_allwaps)+1] <- Pred_Lat
results_allwaps[,ncol(results_allwaps)+1] <- Pred_Lon
results_allwaps[,ncol(results_allwaps)+1] <- BF_allwaps

results_Top3 <- data.frame(matrix(ncol = 3, nrow = 0))
results_Top3[,ncol(results_Top3)+1] <- Pred_Lat
results_Top3[,ncol(results_Top3)+1] <- Pred_Lon
results_Top3[,ncol(results_Top3)+1] <- BF_Top3

#### Saving results files
write.csv(results_allwaps, file = "all_waps.csv", row.names = FALSE)
write.csv(results_Top3, file = "top3.csv", row.names = FALSE)
