#### Ignacio: Missing loading of libraries!!!
if ("pacman" %in% rownames(installed.packages()) == FALSE) {
  install.packages("pacman")
} else {
    pacman::p_load(readr,dplyr,magrittr,lubridate,ggplot2,plotly,reshape2,tidyr,caret)
}


# Load data
trainingData   <- read_csv("UBIQUM/MENTOR/DA119/Ferran Donoso/Wi-Fi/datasets/trainingData.csv")
validationData <- read_csv("UBIQUM/MENTOR/DA119/Ferran Donoso/Wi-Fi/datasets/validationData.csv")


# Pre processing ####
# feature type
# trainingData$FLOOR <- as.factor(trainingData$FLOOR)
# trainingData$BUILDINGID <- as.factor(trainingData$BUILDINGID)
# trainingData$SPACEID <- as.factor(trainingData$SPACEID)
# trainingData$RELATIVEPOSITION <- as.factor(trainingData$RELATIVEPOSITION)
# trainingData$USERID <- as.factor(trainingData$USERID)
# trainingData$PHONEID <- as.factor(trainingData$PHONEID)
# trainingData$TIMESTAMP <- as_datetime(trainingData$TIMESTAMP)
# 
# validationData$FLOOR <- as.factor(validationData$FLOOR)
# validationData$BUILDINGID <- as.factor(validationData$BUILDINGID)
# validationData$SPACEID <- as.factor(validationData$SPACEID)
# validationData$RELATIVEPOSITION <- as.factor(validationData$RELATIVEPOSITION)
# validationData$USERID <- as.factor(validationData$USERID)
# validationData$PHONEID <- as.factor(validationData$PHONEID)
# validationData$TIMESTAMP <- as_datetime(validationData$TIMESTAMP)

#### Ignacio: A better way to do it. 
#### In this way you do two things in one:
#### 1)-Sets the following variables to be categorical:
#### "FLOOR","BUILDINGID","SPACEID","RELATIVEPOSITION","USERID","PHONEID"
#### 2)-Set the variable "TIMESTAMP" as DateTime.
#### Inputs: the dataframe
#### Output: the dataframe with the corresponding modifications.
#### In this way you can prevent typing too much.
#### You can apply the same function on training and validation sets.

preproc <- function(df) {
  cols <- c("FLOOR","BUILDINGID","SPACEID","RELATIVEPOSITION","USERID","PHONEID")
  df %<>% mutate_at(cols, funs(factor(.)))
  df$TIMESTAMP <- as_datetime(df$TIMESTAMP,origin="1970-01-01",tz="Europe/Madrid")
  return(df)
}

trainingData   <- preproc(trainingData)
validationData <- preproc(validationData)

#### Ignacio: Ferran, you should justify the need to merge both datasets.
#### Why they need to be merged if so? In principle, the validation is 
#### to validate your models...
#### Moreover, if you create a combined data, it's better to call it in another
#### way.
# combine data # only for final test
combinedData <- bind_rows(trainingData, validationData)

# NA
sum(is.na(trainingData), na.rm = TRUE)
sum(is.na(validationData), na.rm = TRUE)

#### Ignacio:
# duplicated rows #637
if (sum(duplicated(trainingData)) != 0) {
  trainingData <- trainingData[!duplicated(trainingData), ]
}

if (sum(duplicated(validationData)) != 0) {
  validationData <- validationData[!duplicated(validationData), ]
}

#### Ignacio: Data exploration.
#### Those plots are not the best ones, as the dataset is 3D and the plots
#### are 2D projections. Therefore, you can't see how the observations were 
#### taken in each floor. Are all the floors evenly sampled? 
#### The initial idea to compare both datasets samplings was ok, but the 
#### approach is not the best one because you can't see how they overlap. 
#### One possibility is is to add a new column to each dataset specifiying if 
#### to which kind of dataset corresponds. Then we you merge both, you can
#### color according to this column. ;). Moreover, ggplot can do a better job
#### than plot base function. Even more, you can "send" the ggplot to plot_ly
#### and you will be able to interact with the plot.
#lat lon plot

trainingData$Set <- "Train"
validationData$Set <- "Validation"
combinedData <- bind_rows(trainingData, validationData)
combinedData$Set <- as.factor(combinedData$Set)

p <- plot_ly(combinedData, x = ~LONGITUDE, y = ~LATITUDE, z = ~FLOOR, color = ~Set, colors = c('cyan', 'red'),
             size = 1.2) %>% add_markers() %>%
             layout(scene = list(xaxis = list(title = 'Longitude'),
                            yaxis = list(title = 'Latitude'),
                            zaxis = list(title = 'Floor')))
p

# rows with all na
# combined data #73
#### Ignacio: Avoid making copies and copies of your datasets, specially when they
#### are big.

trainingData[trainingData == 100] <- NA
validationData[validationData == 100] <- NA

#### Ignacio: Explain better what you are doing and why. You are trying yo figure
#### out in which locations the user doesn't have any WIFI signal. In those locations
#### the user will get NA for all the WAPS. 
#### For this purpose, you can create a function to drop the columns for which all
#### the values are NA's

drop_allna_cols <- function(df) {
  ind <- apply(df[,c(1:520)], 1, function(x) all(is.na(x)))
  df <- df[!ind,]
  return(df)
}


trainingData   <- drop_allna_cols(trainingData)
validationData <- drop_allna_cols(validationData)

#### Ignacio: Explain better your purpose. You want to determine which waps
#### were never used. In this case, all the rows of a particular WAP will  be
#### NA.
# columns that have the same value for every row
# training data #55
WAPs_train <- grep("WAP", names(trainingData), value=T)
WAPs_val   <- grep("WAP", names(validationData), value=T)

not_used_waps_train <- apply(trainingData[,c(WAPs_train)],2,function(x) all(is.na(x)))
not_used_waps_val   <- apply(validationData[,c(WAPs_val)],2,function(x) all(is.na(x)))

s <- which(not_used_waps_train==TRUE)
p <- which(not_used_waps_val==TRUE)

#### Ignacio: You can't use WAPs which were not available in any of the datasets
#### observations
not_used_waps <- union(s,p)

#### Ignacio: We free memory
rm(not_used_waps_train,not_used_waps_val,s,p)

# remove the found columns
trainingData   <- trainingData[, -c(not_used_waps)]
validationData <- validationData[, -c(not_used_waps)]
combinedData <- bind_rows(trainingData, validationData)
combinedData$Set <- as.factor(combinedData$Set)

#### Ignacio: Updating columns which corresponds to waps. As both datasets have
#### the same number of WAPs we only need to define this variable once.
WAPs <- grep("WAP", names(trainingData), value=T)

#relocate very good and very bad signals
dat1 <- melt(combinedData[, c(WAPs,"Set")])

ggplot(dat1,aes(x=value,fill=Set)) +
  geom_density(alpha = 0.2) +
  xlab("RSSI Signal value") +
  ggtitle("Densities of WAP signals for each dataset")

#### Ignacio: Obtaning the signal value for which the density is highest.
#### Moreover, why do you pick the value of "a" based on the training set?
a <- density(dat1$value, na.rm = TRUE)$x[which.max(density(dat1$value, na.rm = TRUE)$y)]

#### Ignacio: replacing values smaller than a by a. Ferran, what is the idea behind
#### this approach? This should be explained. At the same time, you are replacing 
#### signal higher than -29 by -29. There is any reason for chosing this particular
#### value? 

#transform datasets
replace_values <- function(x,value) {
  ifelse(x < value,x<-value,ifelse(x > -29,x <- -29, x <- x))
}

trainingData[,c(WAPs)] <- mapply(replace_values,trainingData[,c(WAPs)],a)
trainingData[,c(WAPs)] <- mapply(replace_values,trainingData[,c(WAPs)],a)

# create BUILDINGID_FLOOR feature
build_floor <- function(df) {
  df <- unite(df, BUILDINGID_FLOOR, c("BUILDINGID", "FLOOR"), sep = "_", remove = FALSE)
  df$BUILDINGID_FLOOR <- as.factor(df$BUILDINGID_FLOOR)
  return(df)
}

trainingData   <- build_floor(trainingData)
validationData <- build_floor(validationData)

#### First model to predict the combination of building-flor ####
####
#### Ignacio: For each observation, we determine which WAP gives the highest signal.
#### As the WAPs doesn't move, if we know in which building-floor each wap is located,
#### we simply need to to know which is the WAP with the highest signal in order to
#### know were we are just simply predicting to be the most frequent building-floor
#### in which the highest signal of the WAP was found.
####
#### However this predictive model has several weak points: 
####
#### 1)-Not allways the most frequent location is the real one. 
####
#### 2)-if in a future dataset the WAP with the higest signal is not in the current model because it was 
#### previously discarded, the model will not be able to provide a prediction.
####
#### First step: Finding out for each observation, which is the WAP which gives
#### the strongest signal.
HighestWap_train <- names(trainingData)[apply(trainingData[,c(WAPs)], 1, which.max)]

#### Second step: For each observation we find out in which building_floor the 
#### observation was recorded.
Highest_loc_train <- data.frame("WAP"=HighestWap_train,
                "BFLocation"=trainingData$BUILDINGID_FLOOR)

#### Third step:We find out for each WAP what was the most frequent building-floor
#### in which the highest signal was recorded for it and we store if on a dataframe.
multiple <- c()
for ( i in unique(Highest_loc_train$WAP) ) {
  multiple <- rbind(multiple,
                    c(i,names(which.max(table(Highest_loc_train[which(Highest_loc_train$WAP == i),]$BFLocation)))))
}

multiple <- as.data.frame(multiple)
colnames(multiple) <- c("WAP","Most_Frequent_location")
multiple$Most_Frequent_location <- factor(multiple$Most_Frequent_location,
                                          levels = levels(trainingData$BUILDINGID_FLOOR))

#### Evaluating the model. In order to do that, we will append a new column 
#### to the Highest_loc_train dataframe with the prediction.
Highest_loc_train$Prediction <- sapply(Highest_loc_train$WAP,function(x) multiple[which(multiple$WAP==x),]$Most_Frequent_location )

#### Getting the confussion Matrix.
confusionMatrix(trainingData$BUILDINGID_FLOOR, Highest_loc_train$Prediction)
#### As it can be seen, the accuracy is pretty small: 0.53, while the kappa
#### is 0.48. Therefore this model can be used as a benchmark, but not for
#### production.
#### Close inspection of the Confusion Matrix (CM henceforth) shows that some
#### waps sometimes give the strongest signal in two different buildings. 
#### A possible explanation to is will be that in the current building-floor
#### the wifi coverage is poor. In order to dig more in this respect , a deeper
#### inspection of the errors should be conducted.
#### For this purpose, on can add and extra column which a mismatch betwee the
#### real location and the predicted coded in two colors: green = correct, red
#### = incorrect.
trainingData$BFPred <- sapply(Highest_loc_train,
                              ifelse(BFLocation == Prediction,"Green","Red"))

#### On the other hand, the same CM inspection shows that is almost block diagonal,
#### therefore this model could be much better in predicting only the building. 
#### Let's see it performs. 

#### In order to do this, we only need to get the first digit of the predicted
#### building-floor.

# building prediction #Accuracy: 0.9925
BPrediction <- BFPrediction
BPrediction$location <- str_sub(BPrediction$location, start = 1, end = -3)
BPrediction$BUILDINGID_FLOOR <- str_sub(BPrediction$BUILDINGID_FLOOR, start = 1, end = -3)

confusionMatrix(table(BPrediction$BUILDINGID_FLOOR, BPrediction$location))

# explore waps that affect multiple buildings #12
BPrediction$Error <- BPrediction$location == BPrediction$BUILDINGID_FLOOR
BPredictionErrors <- BPrediction[BPrediction$Error == FALSE, ]
unique(BPredictionErrors$WAP)
BErrors <- unique(BPrediction[BPrediction$Error == FALSE, ]$WAP)
Data_T0_BE <- Data_T0[, !(colnames(Data_T0) %in% BErrors), drop = FALSE]

Data_T1 <- cbind(Data_T1, Data_T0[, c("LONGITUDE", "LATITUDE", "FLOOR", "BUILDINGID", "SPACEID",
                                              "RELATIVEPOSITION", "USERID", "PHONEID", "TIMESTAMP")])

for (i in BErrors) {
  x <- Data_T1[, i]
  plot(Data_T1$BUILDINGID_FLOOR, x, ylim = c(-110, 5))
}

for (i in BErrors) {
  x <- Data_T1[, i]
  plot(density(x, na.rm = TRUE))
}

rm("HighestWap", "BFLocation", "BFPrediction", "BFPredictionErrors", "BPrediction",
   "BPredictionErrors", "Data_T1", "x", "i")

# phones ####
# phones with signal 0 #phones 19 and 23 have 137 signals
WAPs <- grep("WAP", names(trainingData), value=T)
rowsw0 <- which(trainingData[, c(WAPs)] == 0, arr.ind=TRUE)
Data_T0test <- trainingData[rowsw0, ]

table(Data_T0test$PHONEID)

# phones with signal stronger than -30 #phones 19 and 23 have have 700+ signals
rowsw30 <- which(trainingData[, c(WAPs)] > -30, arr.ind=TRUE)

Data_T0test <- Data_T0[rowsw30, ]

table(Data_T0test$PHONEID)

# phones with signal weaker than -90 #phone 23 have 55.000+ signals, phone 13 have 15.000+ signals
rowsw90 <- which(trainingData[, c(WAPs)] < -90, arr.ind=TRUE)

Data_T0test <- Data_T0[rowsw90, ]

table(Data_T0test$PHONEID)

# remove rows with a signal of 0 #worse
#rowsw0 <- as.data.frame(rowsw0)

#Data_T0 <- Data_T0[-rowsw0$row, ]
#Data_T0_BE <- Data_T0_BE[-rowsw0$row, ]
#Data_T0_BFE <- Data_T0_BFE[-rowsw0$row, ]

# remove phone 23 #worse
trainingData[is.na(trainingData)] <- -100

#Data_T0 <- subset(Data_T0, !PHONEID == "23")
#Data_T0_BE <- subset(Data_T0_BE, !PHONEID == "23")
#Data_T0_BFE <- subset(Data_T0_BFE, !PHONEID == "23")

rm("rowsw0", "rowsw30", "rowsw90", "Data_T0test")

# top 3 waps ####
# top 3 waps for each observation #276
Data_T0_T3W <- subset(Data_T0, select = -c(LONGITUDE, LATITUDE, BUILDINGID_FLOOR, FLOOR, BUILDINGID,
                                           SPACEID, RELATIVEPOSITION, USERID, PHONEID, TIMESTAMP))

Data_T0_T3W$Top3Waps <- apply(Data_T0_T3W, 1, function(x) 
  paste(names(head(sort(x, decreasing = TRUE), 3)), collapse = " "))

Data_T0_T3W <- separate(Data_T0_T3W, Top3Waps, c("TopWap1", "TopWap2","TopWap3"))

T3W <- unique(c(Data_T0_T3W$TopWap1, Data_T0_T3W$TopWap2, Data_T0_T3W$TopWap3))
Data_T0_T3W <- Data_T0[, T3W]
Data_T0_T3W <- cbind(Data_T0_T3W, Data_T0[, c("LONGITUDE", "LATITUDE", "FLOOR", "BUILDINGID", "SPACEID",
                                                "RELATIVEPOSITION", "USERID", "PHONEID", "TIMESTAMP",
                                                "BUILDINGID_FLOOR")])

rm("T3W")

# top 3 waps (without BF Errors) for each observation #223
Data_T0_T3W_BFE <- subset(Data_T0, select = -c(LONGITUDE, LATITUDE, BUILDINGID_FLOOR, FLOOR, BUILDINGID,
                                               SPACEID, RELATIVEPOSITION, USERID, PHONEID, TIMESTAMP))
Data_T0_T3W_BFE <- Data_T0_T3W_BFE[, !(colnames(Data_T0_T3W_BFE) %in% BFErrors), drop = FALSE]

Data_T0_T3W_BFE$Top3Waps <- apply(Data_T0_T3W_BFE, 1, function(x) 
  paste(names(head(sort(x, decreasing = TRUE), 3)), collapse = " "))

Data_T0_T3W_BFE <- separate(Data_T0_T3W_BFE, Top3Waps, c("TopWap1", "TopWap2","TopWap3"))

T3W_BFE <- unique(c(Data_T0_T3W_BFE$TopWap1, Data_T0_T3W_BFE$TopWap2, Data_T0_T3W_BFE$TopWap3))
Data_T0_T3W_BFE <- Data_T0[, T3W_BFE]
Data_T0_T3W_BFE <- cbind(Data_T0_T3W_BFE, Data_T0[, c("LONGITUDE", "LATITUDE", "FLOOR", "BUILDINGID",
                                                      "SPACEID", "RELATIVEPOSITION", "USERID",
                                                      "PHONEID", "TIMESTAMP", "BUILDINGID_FLOOR")])

rm("T3W_BFE")

# top 3 waps (without B Errors) for each observation #267
Data_T0_T3W_BE <- subset(Data_T0, select = -c(LONGITUDE, LATITUDE, BUILDINGID_FLOOR, FLOOR, BUILDINGID,
                                              SPACEID, RELATIVEPOSITION, USERID, PHONEID, TIMESTAMP))
Data_T0_T3W_BE <- Data_T0_T3W_BE[, !(colnames(Data_T0_T3W_BE) %in% BErrors), drop = FALSE]

Data_T0_T3W_BE$Top3Waps <- apply(Data_T0_T3W_BE, 1, function(x) 
  paste(names(head(sort(x, decreasing = TRUE), 3)), collapse = " "))

Data_T0_T3W_BE <- separate(Data_T0_T3W_BE, Top3Waps, c("TopWap1", "TopWap2","TopWap3"))

T3W_BE <- unique(c(Data_T0_T3W_BE$TopWap1, Data_T0_T3W_BE$TopWap2, Data_T0_T3W_BE$TopWap3))
Data_T0_T3W_BE <- Data_T0[, T3W_BE]
Data_T0_T3W_BE <- cbind(Data_T0_T3W_BE, Data_T0[, c("LONGITUDE", "LATITUDE", "FLOOR", "BUILDINGID",
                                                    "SPACEID", "RELATIVEPOSITION", "USERID", "PHONEID",
                                                    "TIMESTAMP", "BUILDINGID_FLOOR")])

rm("T3W_BE")

# top 1 wap
# top 1 wap for each observation #246
Data_T0_T1W <- subset(Data_T0, select = -c(LONGITUDE, LATITUDE, BUILDINGID_FLOOR, FLOOR, BUILDINGID,
                                           SPACEID, RELATIVEPOSITION, USERID, PHONEID, TIMESTAMP))

Data_T0_T1W$Top1Waps <- apply(Data_T0_T1W, 1, function(x) 
  paste(names(head(sort(x, decreasing = TRUE), 1)), collapse = " "))

T1W <- unique(Data_T0_T1W$Top1Waps)
Data_T0_T1W <- Data_T0[, T1W]
Data_T0_T1W <- cbind(Data_T0_T1W, Data_T0[, c("LONGITUDE", "LATITUDE", "FLOOR", "BUILDINGID", "SPACEID",
                                              "RELATIVEPOSITION", "USERID", "PHONEID", "TIMESTAMP",
                                              "BUILDINGID_FLOOR")])

rm("T1W")

# top 1 wap (without BF Errors) for each observation #152
Data_T0_T1W_BFE <- subset(Data_T0, select = -c(LONGITUDE, LATITUDE, BUILDINGID_FLOOR, FLOOR, BUILDINGID,
                                               SPACEID, RELATIVEPOSITION, USERID, PHONEID, TIMESTAMP))
Data_T0_T1W_BFE <- Data_T0_T1W_BFE[, !(colnames(Data_T0_T1W_BFE) %in% BFErrors), drop = FALSE]

Data_T0_T1W_BFE$Top1Waps <- apply(Data_T0_T1W_BFE, 1, function(x) 
  paste(names(head(sort(x, decreasing = TRUE), 1)), collapse = " "))

T1W_BFE <- unique(Data_T0_T1W_BFE$Top1Waps)
Data_T0_T1W_BFE <- Data_T0[, T1W_BFE]
Data_T0_T1W_BFE <- cbind(Data_T0_T1W_BFE, Data_T0[, c("LONGITUDE", "LATITUDE", "FLOOR", "BUILDINGID",
                                                      "SPACEID", "RELATIVEPOSITION", "USERID",
                                                      "PHONEID", "TIMESTAMP", "BUILDINGID_FLOOR")])

rm("T1W_BFE")

# top 1 wap (without B Errors) for each observation #237
Data_T0_T1W_BE <- subset(Data_T0, select = -c(LONGITUDE, LATITUDE, BUILDINGID_FLOOR, FLOOR, BUILDINGID,
                                              SPACEID, RELATIVEPOSITION, USERID, PHONEID, TIMESTAMP))
Data_T0_T1W_BE <- Data_T0_T1W_BE[, !(colnames(Data_T0_T1W_BE) %in% BErrors), drop = FALSE]

Data_T0_T1W_BE$Top1Waps <- apply(Data_T0_T1W_BE, 1, function(x) 
  paste(names(head(sort(x, decreasing = TRUE), 1)), collapse = " "))

T1W_BE <- unique(Data_T0_T1W_BE$Top1Waps)
Data_T0_T1W_BE <- Data_T0[, T1W_BE]
Data_T0_T1W_BE <- cbind(Data_T0_T1W_BE, Data_T0[, c("LONGITUDE", "LATITUDE", "FLOOR", "BUILDINGID",
                                                    "SPACEID", "RELATIVEPOSITION", "USERID", "PHONEID",
                                                    "TIMESTAMP", "BUILDINGID_FLOOR")])

rm("T1W_BE")
rm("BErrors", "BFErrors", "WAPs")
gc()

validationData[validationData == 100] <- -100


#normalize test ## didnt work ##
#Data_T0[Data_T0 == -100] <- NA
#WAPs <- grep("WAP", names(Data_T0), value=T)
#Data_T01 <- Data_T0[, WAPs]
#Data_T01 <- abs(Data_T01)
#Data_T01 <- as.data.frame(t(apply(Data_T01, 1, function(x, ...)(x - min(x, ...))/(max(x, ...) - min(x, ...)))))

#Data_T0[is.na(Data_T0)] <- -100

# test set pre processing for final test ##
#validationData <- read_csv("C:/Users/FDL_4/OneDrive/Escritorio/Course/Module 4/Task 1/UJIndoorLoc/Copy of Copy of testData - Copy of Copy of testData.csv")
#View(validationData)
#str(validationData[520:529])

#validationData$FLOOR <- as.factor(validationData$FLOOR)
#validationData$BUILDINGID <- as.factor(validationData$BUILDINGID)
#validationData$SPACEID <- as.factor(validationData$SPACEID)
#validationData$RELATIVEPOSITION <- as.factor(validationData$RELATIVEPOSITION)
#validationData$USERID <- as.factor(validationData$USERID)
#validationData$PHONEID <- as.factor(validationData$PHONEID)
#validationData$TIMESTAMP <- as_datetime(validationData$TIMESTAMP)

#WAPs <- grep("WAP", names(validationData), value=T)
#validationData1 <- validationData[, WAPs]
#validationData1[validationData1 == 100] <- NA
#validationData1[validationData1 < a] <- a
#validationData1[validationData1 > -29] <- -29
#validationData <- cbind(validationData1, validationData[, c("LONGITUDE", "LATITUDE", "FLOOR", "BUILDINGID", "SPACEID",
#                                                            "RELATIVEPOSITION", "USERID", "PHONEID", "TIMESTAMP")])
#validationData[is.na(validationData)] <- -100


