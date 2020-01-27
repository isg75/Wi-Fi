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

#### Building-floor prfediction ####

BF_Model_ranger <- ranger::ranger(BUILDINGID_FLOOR~., data = trainingData[,c(WAPs,"BUILDINGID_FLOOR")]) 

BF_Model_ranger_cm_train <- confusionMatrix(trainingData$BUILDINGID_FLOOR,BF_Model_ranger$predictions)

BF_Model_ranger_pred_val <- predict(BF_Model_ranger,validationData[,c(WAPs)])

BF_Model_ranger_cm_val <- confusionMatrix(validationData$BUILDINGID_FLOOR,BF_Model_ranger_pred_val[[1]])

### Model to predict the Building based on the id's of the three WAPs for which
### the highest signal is captured. Accuracy:

predictors_train <- grep("TopW",colnames(trainingData))
predictors_train <- c(predictors_train,which(colnames(trainingData)=="BUILDINGID"))
predictors_validation   <- grep("TopW",colnames(validationData))
predictors_validation   <- c(predictors_val,which(colnames(validationData)=="BUILDINGID"))

Building_ModelTop3_ranger <- ranger::ranger(BUILDINGID~., data = trainingData[,c(predictors_train)])

###
Building_ModelTop3_ranger_cm_train <- confusionMatrix(trainingData$BUILDINGID,Building_ModelTop3_ranger$predictions)

Building_ModelTop3_ranger_val_pred <- predict(Building_ModelTop3_ranger,validationData[,c(predictors_validation)])

###
Building_ModelTop3_ranger_cm_val   <- confusionMatrix(validationData$BUILDINGID,Building_ModelTop3_ranger_val_pred$predictions)

metrics_b[nrow(metrics_b)+1,] <- c(Building_ModelTop3_ranger_cm_train$overall[1:2],"Top3Waps","Training")
metrics_b[nrow(metrics_b)+1,] <- c(Building_ModelTop3_ranger_cm_val$overall[1:2],"Top3Waps","Validation")

metrics_b[,c(1,2)] <- mapply(as.numeric,metrics_b[,c(1,2)])

#### Plotting error metrics for building
ggplot(metrics_b,aes(x=Set,y=Accuracy,fill=Set)) + geom_col() + facet_wrap(~Model) +
  ggtitle("Accuracy of models for predicting building ID")
ggplot(metrics_b,aes(x=Set,y=Kappa,fill=Set)) + geom_col() + facet_wrap(~Model) +
  ggtitle("Kappa of models for predicting building ID") 
  
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

#### Plotting error metrics for building-floor    
metrics_bf[,c(1,2)] <- mapply(as.numeric,metrics_bf[,c(1,2)])
ggplot(metrics_bf,aes(x=Set,y=Accuracy,fill=Set)) + geom_col() + facet_wrap(~Model) +
  ggtitle("Accuracy of models for predicting building-floor")
ggplot(metrics_bf,aes(x=Set,y=Kappa,fill=Set)) + geom_col() + facet_wrap(~Model) +
  ggtitle("Kappa of models for predicting building-floor") 

#### Ignacio: In order to create models specific for each building, you should
#### use the WAPS and the predicted building. 


#### latitude prediction ####
metrics_lat <- metrics_lat <- data.frame(matrix(ncol = 5, nrow = 0))
colnames(metrics_lat) <- c("RMSE","Rsquared","MAE","Model","Set")

set.seed(123)

Latitude_Model <- ranger(LATITUDE~., data = trainingData[,c(WAPs,"LATITUDE")])

### 
Latitude_Model_pred_train_metrics <- postResample(Latitude_Model$predictions, trainingData$LATITUDE)

Latitude_Model_pred_val <- predict(Latitude_Model, validationData[,c(WAPs)])

### 
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

###
Longitude_Model_metrics_train <- postResample(Longitude_Model$predictions, trainingData$LONGITUDE)

Longitude_Model_pred_val <- predict(Longitude_Model,validationData[,c(WAPs)])

Longitude_Model_metrics_val <-postResample(Longitude_Model_train_metrics_pred_val$predictions,validationData$LONGITUDE)

#lon errors
metrics_lon[nrow(metrics_lon)+1,] <- c(Longitude_Model_metrics_train[1:3],"AllWaps","Training")
metrics_lon[nrow(metrics_lon)+1,] <- c(Longitude_Model_metrics_val[1:3],"AllWaps","Validation")


pabserror_split(Longitude_Model_pred_val$predictions,validationData$LONGITUDE,
                validationData$BUILDINGID,"LONGITUDE")

#### TO DO: Hi Ferran, I made a lot of cleanup of your code. I removed tens of copies 
# of the original dataset. You need to avoid creating so many copies of the original
# dataframe in order to save memory. Otherwise you can run in troubles. 
# If you don't need all the data to creatye model, simply subset the data you submit
# for model training. 
#
# On the other hand: Your scripts, were a crazy compilation of models to get the
# best performance. Is best to have only a few ones (as you can see I deleted 
# many of your models), and try to undersand the most of the them.
# You didn't make any ERROR ANALISYS which is one of the main 
# goals of this task. For example, I missed a 3D plot of locations whe the buildin ID
# was correctly/incorrectly predicted. The same as well for thre building-floor.
# It would be nice to have also plots showing differences between models. Are the
# models making mistakes in the same place or not? If so, what it tells you about
# the data?
#
# You have two scripts. On a first sigth, one is for preprocessing the data and
# the other to generate models. However, in fact that was not the case. In the
# first script you included some basic models. Try to refactor your code in order
# to put preprocessing in one script and the model generation in other.
#
# In addition, the code related with the blind dataset, should be separated from 
# the main code. That's included on another script which hasn't been tested
# deliberatelly in order to  give you some homework.
#
# Remember to embed repetitive pieces of code in a single function, which you can use
# later. I gave you lots of examples in your code as (drop_allna_rows, drop_allna_cols,
# ...) This sort of things are going to be used in every dataset. Therefore they 
# are good candidates to be placed inside a function. Other examples can be for 
# error reporting. 
# If you have problems writing your own functions, practise, practise, and practise.
# When writing a function, ask yourselve what do you want to do. This will tell you
# what need to be your function arguments and what it will output. If you want,
# do a concrete example, and then replace the names of the inputs by something generic.
#
# Also, try to load all the libraries from the beggining, specially when your second
# script uses variables generated by a previous one.
