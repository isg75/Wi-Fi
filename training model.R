#### Missing library loading
if ("pacman" %in% rownames(installed.packages()) == FALSE) {
  install.packages("pacman")
} else {
  pacman::p_load(ranger,e1071)
}

#### building prediction ####


# ranger all waps #Accuracy: 0.98 / Kappa: 0.97 / P-Value: < 2.2e-16

set.seed(123)

#### Ignacio: Brute force approach. Using all the WAPS a model without knowing what
#### it does. High risk of overffiting.
#### Using a method that 
#Building_Model_1 <- ranger(BUILDINGID~., data = Training_Building_1)
WAPs <- grep("WAP", names(trainingData), value=T)

Building_Model_ranger <- ranger::ranger(BUILDINGID~., data = trainingData[,c(WAPs,"BUILDINGID")])

Building_Model_ranger_cm_train <- confusionMatrix(trainingData$BUILDINGID, Building_Model_ranger$predictions) 

Building_Model_ranger_val_pred <- predict(Building_Model_ranger, validationData[,c(WAPs)])

#### Results on validation set: Accuracy: 0.98, Kappa: 0.9, P-value = 0.001
Building_Model_ranger_cm_val <- confusionMatrix(validationData$BUILDINGID, Building_Model_ranger_val_pred[[1]]) 

#### Using Ranger to predict Building-floor:
# ranger all waps not in BF Errors  #Accuracy: 0.85 / Kappa: 0.84 / P-Value: < 2.2e-16
BF_Model_ranger <- ranger::ranger(BUILDINGID_FLOOR~., data = trainingData[,c(WAPs,"BUILDINGID_FLOOR")]) 

BF_Model_ranger_cm_train <- confusionMatrix(trainingData$BUILDINGID_FLOOR,BF_Model_ranger$predictions)

BF_Model_ranger_pred_val <- predict(BF_Model_ranger,validationData[,c(WAPs)])

#### Accuracy: 0.73, Kappa: 0.70
BF_Model_ranger_cm_val <- confusionMatrix(validationData$BUILDINGID_FLOOR,BF_Model_ranger_pred_val[[1]])

# ranger top3waps #Accuracy: 0.9982 / Kappa: 0.9972 / P-Value: < 2.2e-16
Training_Building_2 <- subset(Data_T0_T3W, select = -c(LONGITUDE, LATITUDE, BUILDINGID_FLOOR, FLOOR,
                                                       SPACEID, RELATIVEPOSITION, USERID, PHONEID,
                                                       TIMESTAMP))

set.seed(123); Building_Model_2 <- ranger(BUILDINGID~., data = Training_Building_2)
Pred_Building_Model_2 <- predict(Building_Model_2, Testing)

confusionMatrix(table(Testing$BUILDINGID, Pred_Building_Model_2$predictions))

rm("Training_Building_2", "Building_Model_2", "Pred_Building_Model_2")

# ranger top3waps not in B Error #Accuracy: 0.9991 / Kappa: 0.9986 / P-Value: < 2.2e-16
Training_Building_8 <- subset(Data_T0_T3W_BE, select = -c(LONGITUDE, LATITUDE, BUILDINGID_FLOOR, FLOOR,
                                                       SPACEID, RELATIVEPOSITION, USERID, PHONEID,
                                                       TIMESTAMP))

set.seed(123); Building_Model_8 <- ranger(BUILDINGID~., data = Training_Building_8)
Pred_Building_Model_8 <- predict(Building_Model_8, Testing)

confusionMatrix(table(Testing$BUILDINGID, Pred_Building_Model_8$predictions))

rm("Training_Building_8", "Building_Model_8", "Pred_Building_Model_8")

# ranger top3waps not in BF Error #Accuracy: 0.9721 / Kappa: 0.9559 / P-Value: < 2.2e-16
Training_Building_9 <- subset(Data_T0_T3W_BFE, select = -c(LONGITUDE, LATITUDE, BUILDINGID_FLOOR, FLOOR,
                                                          SPACEID, RELATIVEPOSITION, USERID, PHONEID,
                                                          TIMESTAMP))

set.seed(123); Building_Model_9 <- ranger(BUILDINGID~., data = Training_Building_9)
Pred_Building_Model_9 <- predict(Building_Model_9, Testing)

confusionMatrix(table(Testing$BUILDINGID, Pred_Building_Model_9$predictions))

rm("Training_Building_9", "Building_Model_9", "Pred_Building_Model_9")

# ranger top1waps #Accuracy: 0.9973 / Kappa: 0.9957 / P-Value: < 2.2e-16
Training_Building_5 <- subset(Data_T0_T1W, select = -c(LONGITUDE, LATITUDE, BUILDINGID_FLOOR, FLOOR,
                                                       SPACEID, RELATIVEPOSITION, USERID, PHONEID,
                                                       TIMESTAMP))

set.seed(123); Building_Model_5 <- ranger(BUILDINGID~., data = Training_Building_5)
Pred_Building_Model_5 <- predict(Building_Model_5, Testing)

confusionMatrix(table(Testing$BUILDINGID, Pred_Building_Model_5$predictions))

rm("Training_Building_5", "Building_Model_5", "Pred_Building_Model_5")

# ranger top1waps not in BErrors #Accuracy: 0.9991 / Kappa: 0.9986 / P-Value: < 2.2e-16
Training_Building_6 <- subset(Data_T0_T1W_BE, select = -c(LONGITUDE, LATITUDE, BUILDINGID_FLOOR, FLOOR,
                                                       SPACEID, RELATIVEPOSITION, USERID, PHONEID,
                                                       TIMESTAMP))

set.seed(123); Building_Model_6 <- ranger(BUILDINGID~., data = Training_Building_6)
Pred_Building_Model_6 <- predict(Building_Model_6, Testing)

confusionMatrix(table(Testing$BUILDINGID, Pred_Building_Model_6$predictions))

rm("Training_Building_6", "Building_Model_6", "Pred_Building_Model_6")

# ranger top1waps not in BFErrors #Accuracy: 0.9712 / Kappa: 0.9545 / P-Value: < 2.2e-16
Training_Building_7 <- subset(Data_T0_T1W_BFE, select = -c(LONGITUDE, LATITUDE, BUILDINGID_FLOOR, FLOOR,
                                                          SPACEID, RELATIVEPOSITION, USERID, PHONEID,
                                                          TIMESTAMP))

set.seed(123); Building_Model_7 <- ranger(BUILDINGID~., data = Training_Building_7)
Pred_Building_Model_7 <- predict(Building_Model_7, Testing)

confusionMatrix(table(Testing$BUILDINGID, Pred_Building_Model_7$predictions))

rm("Training_Building_7", "Building_Model_7", "Pred_Building_Model_7")

# floor prediction ####
Testing$BUILDINGID <- Pred_Building_Model_8$predictions

# ranger all waps #Accuracy: 0.9091 / Kappa: 0.8727  / P-Value: < 2.2e-16
Training_Floor_1 <- subset(Data_T0, select = -c(LONGITUDE, LATITUDE, BUILDINGID_FLOOR, SPACEID,
                                                RELATIVEPOSITION, USERID, PHONEID, TIMESTAMP))

set.seed(123); Floor_Model_1 <- ranger(FLOOR~., data = Training_Floor_1)
Pred_Floor_Model_1 <- predict(Floor_Model_1, Testing)

confusionMatrix(table(Testing$FLOOR, Pred_Floor_Model_1$predictions))

rm("Training_Floor_1", "Floor_Model_1", "Pred_Floor_Model_1")

# ranger all waps not in BErrors #Accuracy: 0.9064 / Kappa: 0.8665 / P-Value: < 2.2e-16
Training_Floor_3 <- subset(Data_T0_BE, select = -c(LONGITUDE, LATITUDE, BUILDINGID_FLOOR, SPACEID,
                                                   RELATIVEPOSITION, USERID, PHONEID, TIMESTAMP))

set.seed(123); Floor_Model_3 <- ranger(FLOOR~., data = Training_Floor_3)
Pred_Floor_Model_3 <- predict(Floor_Model_3, Testing)

confusionMatrix(table(Testing$FLOOR, Pred_Floor_Model_3$predictions))

rm("Training_Floor_3", "Floor_Model_3", "Pred_Floor_Model_3")

# ranger all waps not in BFErrors #Accuracy: 0.7003 / Kappa: 0.5867 / P-Value: < 2.2e-16
Training_Floor_4 <- subset(Data_T0_BFE, select = -c(LONGITUDE, LATITUDE, BUILDINGID_FLOOR, SPACEID,
                                                    RELATIVEPOSITION, USERID, PHONEID, TIMESTAMP))

set.seed(123); Floor_Model_4 <- ranger(FLOOR~., data = Training_Floor_4)
Pred_Floor_Model_4 <- predict(Floor_Model_4, Testing)

confusionMatrix(table(Testing$FLOOR, Pred_Floor_Model_4$predictions))

rm("Training_Floor_4", "Floor_Model_4", "Pred_Floor_Model_4")

# ranger top1waps #Accuracy: 0.9118 / Kappa: 0.8764 / P-Value: < 2.2e-16
Training_Floor_5 <- subset(Data_T0_T1W, select = -c(LONGITUDE, LATITUDE, BUILDINGID_FLOOR, SPACEID,
                                                    RELATIVEPOSITION, USERID, PHONEID, TIMESTAMP))

set.seed(123); Floor_Model_5 <- ranger(FLOOR~., data = Training_Floor_5)
Pred_Floor_Model_5 <- predict(Floor_Model_5, Testing)

confusionMatrix(table(Testing$FLOOR, Pred_Floor_Model_5$predictions))

rm("Training_Floor_5", "Floor_Model_5", "Pred_Floor_Model_5")

# ranger Top1Waps not in B Errors #Accuracy: 0.91 / Kappa: 0.874 / P-Value: < 2.2e-16
Training_Floor_10 <- subset(Data_T0_T1W_BE, select = -c(LONGITUDE, LATITUDE, BUILDINGID_FLOOR, SPACEID,
                                                        RELATIVEPOSITION, USERID, PHONEID, TIMESTAMP))

set.seed(123); Floor_Model_10 <- ranger(FLOOR~., data = Training_Floor_10)
Pred_Floor_Model_10 <- predict(Floor_Model_10, Testing)

confusionMatrix(table(Testing$FLOOR, Pred_Floor_Model_10$predictions))

rm("Training_Floor_10", "Floor_Model_10", "Pred_Floor_Model_10")

# ranger Top1Waps not in BF Errors #Accuracy: 0.7102 / Kappa: 0.6003 / P-Value: < 2.2e-16
Training_Floor_8 <- subset(Data_T0_T1W_BFE, select = -c(LONGITUDE, LATITUDE, BUILDINGID_FLOOR, SPACEID,
                                                        RELATIVEPOSITION, USERID, PHONEID, TIMESTAMP))

set.seed(123); Floor_Model_8 <- ranger(FLOOR~., data = Training_Floor_8)
Pred_Floor_Model_8 <- predict(Floor_Model_8, Testing)

confusionMatrix(table(Testing$FLOOR, Pred_Floor_Model_8$predictions))

rm("Training_Floor_8", "Floor_Model_8", "Pred_Floor_Model_8")

# ranger top3waps #Accuracy: 0.9136 / Kappa: 0.8789 / P-Value: < 2.2e-16
Training_Floor_6 <- subset(Data_T0_T3W, select = -c(LONGITUDE, LATITUDE, BUILDINGID_FLOOR, SPACEID,
                                                    RELATIVEPOSITION, USERID, PHONEID, TIMESTAMP))

set.seed(123); Floor_Model_6 <- ranger(FLOOR~., data = Training_Floor_6)
Pred_Floor_Model_6 <- predict(Floor_Model_6, Testing)

confusionMatrix(table(Testing$FLOOR, Pred_Floor_Model_6$predictions))

rm("Training_Floor_6", "Floor_Model_6", "Pred_Floor_Model_6")

# ranger Top3Waps not in B Errors #Accuracy: 0.9127 / Kappa: 0.8777 / P-Value: < 2.2e-16
Training_Floor_9 <- subset(Data_T0_T3W_BE, select = -c(LONGITUDE, LATITUDE, BUILDINGID_FLOOR, SPACEID,
                                                       RELATIVEPOSITION, USERID, PHONEID, TIMESTAMP))

set.seed(123); Floor_Model_9 <- ranger(FLOOR~., data = Training_Floor_9)
Pred_Floor_Model_9 <- predict(Floor_Model_9, Testing)

confusionMatrix(table(Testing$FLOOR, Pred_Floor_Model_9$predictions))

rm("Training_Floor_9", "Floor_Model_9", "Pred_Floor_Model_9")

# ranger Top3Waps not in BF Errors #Accuracy: 0.7057 / Kappa: 0.5946 / P-Value: < 2.2e-16
Training_Floor_7 <- subset(Data_T0_T3W_BFE, select = -c(LONGITUDE, LATITUDE, BUILDINGID_FLOOR, SPACEID,
                                                        RELATIVEPOSITION, USERID, PHONEID, TIMESTAMP))

set.seed(123); Floor_Model_7 <- ranger(FLOOR~., data = Training_Floor_7)
Pred_Floor_Model_7 <- predict(Floor_Model_7, Testing)

confusionMatrix(table(Testing$FLOOR, Pred_Floor_Model_7$predictions))

rm("Training_Floor_7", "Floor_Model_7", "Pred_Floor_Model_7")

# floor prediction by building ####
# ranger top 3 waps
# building 0 #Accuracy: 0.9721 / Kappa: 0.9605 / P-Value: < 2.2e-16
Training_Floor_2 <- subset(Data_T0_T3W, select = -c(LONGITUDE, LATITUDE, BUILDINGID_FLOOR, SPACEID,
                                                    RELATIVEPOSITION, USERID, PHONEID, TIMESTAMP))
Training_Floor_2_b0 <- filter(Training_Floor_2, BUILDINGID == 0)
Testing_Floor_2_b0 <- filter(Testing, BUILDINGID == 0)

set.seed(123); Floor_Model_2_b0 <- ranger(FLOOR~., data = Training_Floor_2_b0)
Pred_Floor_Model_2_b0 <- predict(Floor_Model_2_b0, Testing_Floor_2_b0)

confusionMatrix(table(Testing_Floor_2_b0$FLOOR, Pred_Floor_Model_2_b0$predictions))

rm("Training_Floor_2", "Floor_Model_2_b0", "Pred_Floor_Model_2_b0", "Testing_Floor_2_b0",
   "Training_Floor_2_b0")

# building 1 #Accuracy: 0.8007 / Kappa: 0.7111 / P-Value: < 2.2e-16
Training_Floor_2 <- subset(Data_T0_T3W, select = -c(LONGITUDE, LATITUDE, BUILDINGID_FLOOR, SPACEID,
                                                    RELATIVEPOSITION, USERID, PHONEID, TIMESTAMP))
Training_Floor_2_b1 <- filter(Training_Floor_2, BUILDINGID == 1)
Testing_Floor_2_b1 <- filter(Testing, BUILDINGID == 1)

set.seed(123); Floor_Model_2_b1 <- ranger(FLOOR~., data = Training_Floor_2_b1)
Pred_Floor_Model_2_b1 <- predict(Floor_Model_2_b1, Testing_Floor_2_b1)

confusionMatrix(table(Testing_Floor_2_b1$FLOOR, Pred_Floor_Model_2_b1$predictions))

rm("Training_Floor_2", "Floor_Model_2_b1", "Pred_Floor_Model_2_b1", "Testing_Floor_2_b1",
   "Training_Floor_2_b1")

# building 2 #Accuracy: 0.9291 / Kappa: 0.9033 / P-Value: < 2.2e-16
Training_Floor_2 <- subset(Data_T0_T3W, select = -c(LONGITUDE, LATITUDE, BUILDINGID_FLOOR, SPACEID,
                                                    RELATIVEPOSITION, USERID, PHONEID, TIMESTAMP))
Training_Floor_2_b2 <- filter(Training_Floor_2, BUILDINGID == 2)
Testing_Floor_2_b2 <- filter(Testing, BUILDINGID == 2)

set.seed(123); Floor_Model_2_b2 <- ranger(FLOOR~., data = Training_Floor_2_b2)
Pred_Floor_Model_2_b2 <- predict(Floor_Model_2_b2, Testing_Floor_2_b2)

confusionMatrix(table(Testing_Floor_2_b2$FLOOR, Pred_Floor_Model_2_b2$predictions))

rm("Training_Floor_2", "Floor_Model_2_b2", "Pred_Floor_Model_2_b2", "Testing_Floor_2_b2",
   "Training_Floor_2_b2")

# latitude prediction ####
Testing$FLOOR <- Pred_Floor_Model_6$predictions

# ranger all waps #RMSE: 9.1803261 / R2: 0.9843157 / MAE: 6.2691966 +- 0.394956
Training_Latitude_1 <- subset(Data_T0, select = -c(LONGITUDE, BUILDINGID_FLOOR, SPACEID,
                                                   RELATIVEPOSITION, USERID, PHONEID, TIMESTAMP))

set.seed(123); Latitude_Model_1 <- ranger(LATITUDE~., data = Training_Latitude_1)
Pred_Latitude_Model_1 <- predict(Latitude_Model_1, Testing)

postResample(Pred_Latitude_Model_1$predictions, Testing$LATITUDE)

rm("Training_Latitude_1", "Latitude_Model_1", "Pred_Latitude_Model_1")

AELAT <- abs(Pred_Latitude_Model_1$predictions - Testing$LATITUDE)
qt(0.975, df = length(AELAT)-1) * sd(AELAT) / sqrt(length(AELAT))

# ranger all waps not in B Errors #RMSE: 9.2741441 / R2: 0.9839362 / MAE: 6.3171675 +- 0.3998763
Training_Latitude_3 <- subset(Data_T0_BE, select = -c(LONGITUDE, BUILDINGID_FLOOR, SPACEID,
                                                      RELATIVEPOSITION, USERID, PHONEID, TIMESTAMP))

set.seed(123); Latitude_Model_3 <- ranger(LATITUDE~., data = Training_Latitude_3)
Pred_Latitude_Model_3 <- predict(Latitude_Model_3, Testing)

postResample(Pred_Latitude_Model_3$predictions, Testing$LATITUDE)

rm("Training_Latitude_3", "Latitude_Model_3", "Pred_Latitude_Model_3")

AELAT <- abs(Pred_Latitude_Model_3$predictions - Testing$LATITUDE)
qt(0.975, df = length(AELAT)-1) * sd(AELAT) / sqrt(length(AELAT))

# ranger all waps not in BF Errors #RMSE: 14.6148419 / R2: 0.9623722 / MAE: 10.0271157 +- 0.6261765
Training_Latitude_4 <- subset(Data_T0_BFE, select = -c(LONGITUDE, BUILDINGID_FLOOR, SPACEID,
                                                       RELATIVEPOSITION, USERID, PHONEID, TIMESTAMP))

set.seed(123); Latitude_Model_4 <- ranger(LATITUDE~., data = Training_Latitude_4)
Pred_Latitude_Model_4 <- predict(Latitude_Model_4, Testing)

postResample(Pred_Latitude_Model_4$predictions, Testing$LATITUDE)

rm("Training_Latitude_4", "Latitude_Model_4", "Pred_Latitude_Model_4")

AELAT <- abs(Pred_Latitude_Model_4$predictions - Testing$LATITUDE)
qt(0.975, df = length(AELAT)-1) * sd(AELAT) / sqrt(length(AELAT))

# ranger top 1 waps #RMSE: 8.9397231 / R2: 0.9849022 / MAE: 6.1450834 +- 0.3823783
Training_Latitude_5 <- subset(Data_T0_T1W, select = -c(LONGITUDE, BUILDINGID_FLOOR, SPACEID,
                                                       RELATIVEPOSITION, USERID, PHONEID, TIMESTAMP))

set.seed(123); Latitude_Model_5 <- ranger(LATITUDE~., data = Training_Latitude_5)
Pred_Latitude_Model_5 <- predict(Latitude_Model_5, Testing)

postResample(Pred_Latitude_Model_5$predictions, Testing$LATITUDE)

rm("Training_Latitude_5", "Latitude_Model_5", "Pred_Latitude_Model_5")

AELAT <- abs(Pred_Latitude_Model_5$predictions - Testing$LATITUDE)
qt(0.975, df = length(AELAT)-1) * sd(AELAT) / sqrt(length(AELAT))

# ranger top 1 waps not in B Errors #RMSE: 8.9686377 / R2: 0.9848771 / MAE: 6.1581590 +- 0.3839937
Training_Latitude_6 <- subset(Data_T0_T1W_BE, select = -c(LONGITUDE, BUILDINGID_FLOOR, SPACEID,
                                                          RELATIVEPOSITION, USERID, PHONEID, TIMESTAMP))

set.seed(123); Latitude_Model_6 <- ranger(LATITUDE~., data = Training_Latitude_6)
Pred_Latitude_Model_6 <- predict(Latitude_Model_6, Testing)

postResample(Pred_Latitude_Model_6$predictions, Testing$LATITUDE)

rm("Training_Latitude_6", "Latitude_Model_6", "Pred_Latitude_Model_6")

AELAT <- abs(Pred_Latitude_Model_6$predictions - Testing$LATITUDE)
qt(0.975, df = length(AELAT)-1) * sd(AELAT) / sqrt(length(AELAT))

# ranger top 1 waps not in BF Errors #RMSE: 14.2960935 / R2: 0.9635749 / MAE: 9.6042089 +- 0.6236426
Training_Latitude_7 <- subset(Data_T0_T1W_BFE, select = -c(LONGITUDE, BUILDINGID_FLOOR, SPACEID,
                                                           RELATIVEPOSITION, USERID, PHONEID, TIMESTAMP))

set.seed(123); Latitude_Model_7 <- ranger(LATITUDE~., data = Training_Latitude_7)
Pred_Latitude_Model_7 <- predict(Latitude_Model_7, Testing)

postResample(Pred_Latitude_Model_7$predictions, Testing$LATITUDE)

rm("Training_Latitude_7", "Latitude_Model_7", "Pred_Latitude_Model_7")

AELAT <- abs(Pred_Latitude_Model_7$predictions - Testing$LATITUDE)
qt(0.975, df = length(AELAT)-1) * sd(AELAT) / sqrt(length(AELAT))

# ranger top 3 waps #RMSE: 8.8442755 / R2: 0.9852682 / MAE: 6.0498723 +- 0.379938
Training_Latitude_8 <- subset(Data_T0_T3W, select = -c(LONGITUDE, BUILDINGID_FLOOR, SPACEID,
                                                       RELATIVEPOSITION, USERID, PHONEID, TIMESTAMP))

set.seed(123); Latitude_Model_8 <- ranger(LATITUDE~., data = Training_Latitude_8)
Pred_Latitude_Model_8 <- predict(Latitude_Model_8, Testing)

postResample(Pred_Latitude_Model_8$predictions, Testing$LATITUDE)

rm("Training_Latitude_8", "Latitude_Model_8", "Pred_Latitude_Model_8")

# ranger top 3 waps not in B Errors #RMSE: 8.9509433 / R2: 0.9849085 / MAE: 6.1048953 +- 0.3855085
Training_Latitude_2 <- subset(Data_T0_T3W_BE, select = -c(LONGITUDE, BUILDINGID_FLOOR, SPACEID,
                                                          RELATIVEPOSITION, USERID, PHONEID, TIMESTAMP))

set.seed(123); Latitude_Model_2 <- ranger(LATITUDE~., data = Training_Latitude_2)
Pred_Latitude_Model_2 <- predict(Latitude_Model_2, Testing)

postResample(Pred_Latitude_Model_2$predictions, Testing$LATITUDE)

rm("Training_Latitude_2", "Latitude_Model_2", "Pred_Latitude_Model_2")

AELAT <- abs(Pred_Latitude_Model_2$predictions - Testing$LATITUDE)
qt(0.975, df = length(AELAT)-1) * sd(AELAT) / sqrt(length(AELAT))

# ranger top 3 waps not in BF Errors #RMSE: 14.4181519 / R2: 0.9631389 / MAE: 9.7712006 +- 0.6243909
Training_Latitude_9 <- subset(Data_T0_T3W_BFE, select = -c(LONGITUDE, BUILDINGID_FLOOR, SPACEID,
                                                           RELATIVEPOSITION, USERID, PHONEID, TIMESTAMP))

set.seed(123); Latitude_Model_9 <- ranger(LATITUDE~., data = Training_Latitude_9)
Pred_Latitude_Model_9 <- predict(Latitude_Model_9, Testing)

postResample(Pred_Latitude_Model_9$predictions, Testing$LATITUDE)

rm("Training_Latitude_9", "Latitude_Model_9", "Pred_Latitude_Model_9")

AELAT <- abs(Pred_Latitude_Model_9$predictions - Testing$LATITUDE)
qt(0.975, df = length(AELAT)-1) * sd(AELAT) / sqrt(length(AELAT))

#lat errors
AELAT <- abs(Pred_Latitude_Model_8$predictions - Testing$LATITUDE)
plot(density(AELAT, na.rm = TRUE))

dens <- density(AELAT, na.rm = TRUE)
df <- data.frame(x=dens$x, y=dens$y)
probs <- c(0.75)
quantiles <- quantile(AELAT, prob=probs)
df$quant <- factor(findInterval(df$x,quantiles))
ggplot(df, aes(x,y)) + geom_line(size=1.5) + geom_ribbon(aes(ymin=0, ymax=y, fill=quant)) +
  scale_fill_brewer(guide="none") + labs(x = "Absolute Error", y = "Density", title = "Latitude")

qt(0.975, df = length(AELAT)-1) * sd(AELAT) / sqrt(length(AELAT))

Testing1 <- Testing
Testing1$AELAT <- AELAT
plot(Testing1$AELAT[Testing1$BUILDINGID == 0], ylim = c(0,50), type = "l", lwd = 2,
     xlab = "Observations", ylab = "Absolute Error", main = "Latitude Building 0")
abline(h = 6.0498723, lwd = 2, col = "red")

plot(Testing1$AELAT[Testing1$BUILDINGID == 1], ylim = c(0,50), type = "l", lwd = 2,
     xlab = "Observations", ylab = "Absolute Error", main = "Latitude Building 1")
abline(h = 6.0498723, lwd = 2, col = "red")

plot(Testing1$AELAT[Testing1$BUILDINGID == 2], ylim = c(0,50), type = "l", lwd = 2,
     xlab = "Observations", ylab = "Absolute Error", main = "Latitude Building 2")
abline(h = 6.0498723, lwd = 2, col = "red")

# longitude prediction ####
Testing$LATITUDE <- Pred_Latitude_Model_8$predictions

# ranger all waps #RMSE: 9.0911327 / R2: 0.9944196 / MAE: 6.5108926 +- 0.3736637
Training_Longitude_1 <- subset(Data_T0, select = -c(BUILDINGID_FLOOR, SPACEID, RELATIVEPOSITION, USERID,
                                                    PHONEID, TIMESTAMP))

set.seed(123); Longitude_Model_1 <- ranger(LONGITUDE~., data = Training_Longitude_1)
Pred_Longitude_Model_1 <- predict(Longitude_Model_1, Testing)

postResample(Pred_Longitude_Model_1$predictions, Testing$LONGITUDE)

rm("Training_Longitude_1", "Longitude_Model_1", "Pred_Longitude_Model_1")

AELON <- abs(Pred_Longitude_Model_1$predictions - Testing$LONGITUDE)
qt(0.975, df = length(AELON)-1) * sd(AELON) / sqrt(length(AELON))

# ranger all waps not in B Errors #RMSE: 9.2211555 / R2: 0.9942527 / MAE: 6.5973237 +- 0.3794117
Training_Longitude_3 <- subset(Data_T0_BE, select = -c(BUILDINGID_FLOOR, SPACEID, RELATIVEPOSITION,
                                                       USERID, PHONEID, TIMESTAMP))

set.seed(123); Longitude_Model_3 <- ranger(LONGITUDE~., data = Training_Longitude_3)
Pred_Longitude_Model_3 <- predict(Longitude_Model_3, Testing)

postResample(Pred_Longitude_Model_3$predictions, Testing$LONGITUDE)

rm("Training_Longitude_3", "Longitude_Model_3", "Pred_Longitude_Model_3")

AELON <- abs(Pred_Longitude_Model_3$predictions - Testing$LONGITUDE)
qt(0.975, df = length(AELON)-1) * sd(AELON) / sqrt(length(AELON))

# ranger all waps not in BF Errors #RMSE: 15.603050 / R2: 0.983524 / MAE: 11.042969 +- 0.6491766
Training_Longitude_4 <- subset(Data_T0_BFE, select = -c(BUILDINGID_FLOOR, SPACEID, RELATIVEPOSITION,
                                                        USERID, PHONEID, TIMESTAMP))

set.seed(123); Longitude_Model_4 <- ranger(LONGITUDE~., data = Training_Longitude_4)
Pred_Longitude_Model_4 <- predict(Longitude_Model_4, Testing)

postResample(Pred_Longitude_Model_4$predictions, Testing$LONGITUDE)

rm("Training_Longitude_4", "Longitude_Model_4", "Pred_Longitude_Model_4")

AELON <- abs(Pred_Longitude_Model_4$predictions - Testing$LONGITUDE)
qt(0.975, df = length(AELON)-1) * sd(AELON) / sqrt(length(AELON))

# ranger top 1 waps #RMSE: 9.1189577 / R2: 0.9943484 / MAE: 6.5204635 +- 0.3754323
Training_Longitude_5 <- subset(Data_T0_T1W, select = -c(BUILDINGID_FLOOR, SPACEID, RELATIVEPOSITION,
                                                        USERID, PHONEID, TIMESTAMP))

set.seed(123); Longitude_Model_5 <- ranger(LONGITUDE~., data = Training_Longitude_5)
Pred_Longitude_Model_5 <- predict(Longitude_Model_5, Testing)

postResample(Pred_Longitude_Model_5$predictions, Testing$LONGITUDE)

rm("Training_Longitude_5", "Longitude_Model_5", "Pred_Longitude_Model_5")

AELON <- abs(Pred_Longitude_Model_5$predictions - Testing$LONGITUDE)
qt(0.975, df = length(AELON)-1) * sd(AELON) / sqrt(length(AELON))

# ranger top 1 waps not in B Errors #RMSE: 9.1385637 / R2: 0.9943319 / MAE: 6.5396171 +- 0.3759299
Training_Longitude_6 <- subset(Data_T0_T1W_BE, select = -c(BUILDINGID_FLOOR, SPACEID, RELATIVEPOSITION,
                                                           USERID, PHONEID, TIMESTAMP))

set.seed(123); Longitude_Model_6 <- ranger(LONGITUDE~., data = Training_Longitude_6)
Pred_Longitude_Model_6 <- predict(Longitude_Model_6, Testing)

postResample(Pred_Longitude_Model_6$predictions, Testing$LONGITUDE)

rm("Training_Longitude_6", "Longitude_Model_6", "Pred_Longitude_Model_6")

AELON <- abs(Pred_Longitude_Model_6$predictions - Testing$LONGITUDE)
qt(0.975, df = length(AELON)-1) * sd(AELON) / sqrt(length(AELON))

# ranger top 1 waps not in BF Errors #RMSE: 15.1117601 / R2: 0.9844295 / MAE: 10.4929693 +- 0.6404492
Training_Longitude_8 <- subset(Data_T0_T1W_BFE, select = -c(BUILDINGID_FLOOR, SPACEID, RELATIVEPOSITION,
                                                           USERID, PHONEID, TIMESTAMP))

set.seed(123); Longitude_Model_8 <- ranger(LONGITUDE~., data = Training_Longitude_8)
Pred_Longitude_Model_8 <- predict(Longitude_Model_8, Testing)

postResample(Pred_Longitude_Model_8$predictions, Testing$LONGITUDE)

rm("Training_Longitude_8", "Longitude_Model_8", "Pred_Longitude_Model_8")

AELON <- abs(Pred_Longitude_Model_8$predictions - Testing$LONGITUDE)
qt(0.975, df = length(AELON)-1) * sd(AELON) / sqrt(length(AELON))

# ranger top 3 waps #RMSE: 8.9504587 / R2: 0.9945651 / MAE: 6.3864270 +- 0.3693097
Training_Longitude_7 <- subset(Data_T0_T3W, select = -c(BUILDINGID_FLOOR, SPACEID, RELATIVEPOSITION,
                                                        USERID, PHONEID, TIMESTAMP))

set.seed(123); Longitude_Model_7 <- ranger(LONGITUDE~., data = Training_Longitude_7)
Pred_Longitude_Model_7 <- predict(Longitude_Model_7, Testing)

postResample(Pred_Longitude_Model_7$predictions, Testing$LONGITUDE)

rm("Training_Longitude_7", "Longitude_Model_7", "Pred_Longitude_Model_7")

# ranger top 3 waps not in B Errors #RMSE: 9.0405160 / R2: 0.9944651 / MAE: 6.4427687 +- 0.3734998
Training_Longitude_2 <- subset(Data_T0_T3W_BE, select = -c(BUILDINGID_FLOOR, SPACEID, RELATIVEPOSITION,
                                                           USERID, PHONEID, TIMESTAMP))

set.seed(123); Longitude_Model_2 <- ranger(LONGITUDE~., data = Training_Longitude_2)
Pred_Longitude_Model_2 <- predict(Longitude_Model_2, Testing)

postResample(Pred_Longitude_Model_2$predictions, Testing$LONGITUDE)

rm("Training_Longitude_2", "Longitude_Model_2", "Pred_Longitude_Model_2")

AELON <- abs(Pred_Longitude_Model_2$predictions - Testing$LONGITUDE)
qt(0.975, df = length(AELON)-1) * sd(AELON) / sqrt(length(AELON))

# ranger top 3 waps not in BF Errors #RMSE: 15.3006619 / R2: 0.9840877 / MAE: 10.6491135 +- 0.6470323
Training_Longitude_9 <- subset(Data_T0_T3W_BFE, select = -c(BUILDINGID_FLOOR, SPACEID, RELATIVEPOSITION,
                                                            USERID, PHONEID, TIMESTAMP))

set.seed(123); Longitude_Model_9 <- ranger(LONGITUDE~., data = Training_Longitude_9)
Pred_Longitude_Model_9 <- predict(Longitude_Model_9, Testing)

postResample(Pred_Longitude_Model_9$predictions, Testing$LONGITUDE)

rm("Training_Longitude_9", "Longitude_Model_9", "Pred_Longitude_Model_9")

AELON <- abs(Pred_Longitude_Model_9$predictions - Testing$LONGITUDE)
qt(0.975, df = length(AELON)-1) * sd(AELON) / sqrt(length(AELON))

#lon errors
AELON <- abs(Pred_Longitude_Model_7$predictions - Testing$LONGITUDE)
plot(density(AELON, na.rm = TRUE))

dens <- density(AELON, na.rm = TRUE)
df <- data.frame(x=dens$x, y=dens$y)
probs <- c(0.75)
quantiles <- quantile(AELON, prob=probs)
df$quant <- factor(findInterval(df$x,quantiles))
ggplot(df, aes(x,y)) + geom_line(size=1.5) + geom_ribbon(aes(ymin=0, ymax=y, fill=quant)) +
  scale_fill_brewer(guide="none") + labs(x = "Absolute Error", y = "Density", title = "Longitude")

qt(0.975, df = length(AELON)-1) * sd(AELON) / sqrt(length(AELON))

Testing1$AELON <- AELON
plot(Testing1$AELON[Testing1$BUILDINGID == 0], ylim = c(0,50), type = "l", lwd = 2,
     xlab = "Observations", ylab = "Absolute Error", main = "Longitude Building 0")
abline(h = 6.3864270, lwd = 2, col = "red")

plot(Testing1$AELON[Testing1$BUILDINGID == 1], ylim = c(0,50), type = "l", lwd = 2,
     xlab = "Observations", ylab = "Absolute Error", main = "Longitude Building 1")
abline(h = 6.3864270, lwd = 2, col = "red")

plot(Testing1$AELON[Testing1$BUILDINGID == 2], ylim = c(0,60), type = "l", lwd = 2,
     xlab = "Observations", ylab = "Absolute Error", main = "Longitude Building 2")
abline(h = 6.3864270, lwd = 2, col = "red")


rm("dens", "df", "probs", "quantiles", "AELON", "AELAT", "Testing1")


#plot(Testing1$AELAT[Testing1$BUILDINGID == 1], ylim = c(0,50), col = Testing1$FLOOR, pch = 19,
#     xlab = "Observations", ylab = "Absolute Error", main = "Latitude Building 1")
#abline(h = 6.0498723, lwd = 2)
#legend(0, 50, legend = levels(Testing1$FLOOR), col = unique(Testing1$FLOOR), pch=19, title="Floor",
#       horiz=TRUE)

# extract csv
Testing$LONGITUDE <- Pred_Longitude_Model_7$predictions

# submission 1 (8, 6, 8, 7)
Testing <- subset(Testing, select = c(LONGITUDE, LATITUDE, FLOOR))
Testing$FLOOR <- as.numeric(Testing$FLOOR)
Testing <- Testing[, c(2, 1, 3)]


write.csv(Testing, file = "submission 1.csv", row.names = FALSE)

# submission 2 (6, 6, 8, 7)
Testing <- subset(Testing, select = c(LONGITUDE, LATITUDE, FLOOR))
Testing$FLOOR <- as.numeric(Testing$FLOOR)
Testing <- Testing[, c(2, 1, 3)]

write.csv(Testing, file = "submission 2.csv", row.names = FALSE)
#write.table(Testing, file = "submission 2.csv",row.names=FALSE, na="",col.names=FALSE, sep=",")

# submission 3 (8, 6, 8, 7) lat lon independant
Testing <- subset(Testing, select = c(LONGITUDE, LATITUDE, FLOOR))
Testing$FLOOR <- as.numeric(Testing$FLOOR)
Testing <- Testing[, c(2, 1, 3)]

write.csv(Testing, file = "submission 3.csv", row.names = FALSE)

