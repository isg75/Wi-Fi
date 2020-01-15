#### Ignacio: Missing loading of libraries!!!
if ("pacman" %in% rownames(installed.packages()) == FALSE) {
  install.packages("pacman")
} else {
    pacman::p_load(readr,dplyr,magrittr,lubridate,ggplot2,plotly,reshape2,tidyr,
                   caret,stringr)
}


#### Load data ###
trainingData   <- read_csv("UBIQUM/MENTOR/DA119/Ferran Donoso/Wi-Fi/datasets/trainingData.csv")
validationData <- read_csv("UBIQUM/MENTOR/DA119/Ferran Donoso/Wi-Fi/datasets/validationData.csv")

#### Pre processing ####
#### Ignacio: A better way to do it. 
#### In this way you do several things in one:
#### 1)-Sets the following variables to be categorical:
#### "FLOOR","BUILDINGID"
#### 2)-Set the variable "TIMESTAMP" as DateTime.
#### 3)-Remove columns "SPACEID","RELATIVEPOSITION", "USERID","PHONEID"
#### as they are not used. Moreover, the model can't depend on such features
#### as otherwise a new user will not be able to get his/her position as 
#### there is no guarantee that a new USERID and PHONEID values being used
#### in the model.
#### 4)-Removing duplicated observations.
#### 5)-Removing the "TIMESTAMP" as the model shouldn't depend on time.
#### Inputs: the dataframe
#### Output: the dataframe with the corresponding modifications.
#### In this way you can prevent typing too much.
#### You can apply the same function on training and validation sets.

preproc <- function(df) {
  df$SPACEID <- NULL
  df$RELATIVEPOSITION <- NULL
  df$USERID <- NULL
  df$PHONEID <- NULL
  df$TIMESTAMP <- NULL
  cols <- c("FLOOR","BUILDINGID")
  df[,cols] <- lapply(df[,cols],as.factor)
  if (sum(duplicated(df)) != 0) {
    df <- df[!duplicated(df), ]
  }
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

#### Ignacio: Data exploration. ####
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

#trainingData[trainingData == 100] <- NA
#validationData[validationData == 100] <- NA

#### Ignacio: Explain better what you are doing and why. You are trying yo figure
#### out in which locations the user doesn't have any WIFI signal. In those locations
#### the user will get NA for all the WAPS. 
#### For this purpose, you can create a function to drop the columns for which all
#### the values are NA's
#### This function should be included in the preprocessing function. However it 
#### has been deliberatelly left here for illustrative purposes.

drop_allna_rows <- function(df) {
  ind <- apply(df[,c(1:520)], 1, function(x) all(is.na(x)))
  df <- df[!ind,]
  return(df)
}

trainingData   <- drop_allna_rows(trainingData)
validationData <- drop_allna_rows(validationData)

#### Feature selection: Filtering WAPS ####
#### Ignacio: Explain better your purpose. You want to determine which waps
#### were never used. In this case, all the columns of a particular WAP will
#### be NA.
# columns that have the same value for every row
# training data #55
WAPs_train <- grep("WAP", names(trainingData), value=T)
WAPs_val   <- grep("WAP", names(validationData), value=T)

#### The not used WAPs doesn't need to be the same in both datasets. Therefore,
#### if we want the most general model, we need to avoid working with any useless
#### WAPS.

not_used_waps_train <- apply(trainingData[,c(WAPs_train)],2,function(x) all(is.na(x)))
not_used_waps_val   <- apply(validationData[,c(WAPs_val)],2,function(x) all(is.na(x)))

s <- which(not_used_waps_train==TRUE)
p <- which(not_used_waps_val==TRUE)

#### Ignacio: You can't use WAPs which were not available in any of the datasets
#### observations.

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

p0 <- ggplot(dat1,aes(x=value,fill=Set)) +
  geom_density(alpha = 0.2) +
  xlab("RSSI Signal value") +
  ggtitle("Densities of WAP signals for each dataset")

p0

#### Feature engenieering ###
#### Ignacio: Obtaning the signal value for which the density is highest.
#### Moreover, why do you pick the value of "a" based on the training set?
#a <- density(dat1$value, na.rm = TRUE)$x[which.max(density(dat1$value, na.rm = TRUE)$y)]

#### Ignacio: replacing values smaller than a by a. Ferran, what is the idea behind
#### this approach? This should be explained. At the same time, you are replacing 
#### signal higher than -29 by -29. There is any reason for chosing this particular
#### value? 

#transform datasets.
#### We replace too small values by "NA" and values in the [-30,0] range to be
#### -30 as values in this range are unrealistic.
#### See: https://support.randomsolutions.nl/827069-Best-dBm-Values-for-Wifi

replace_values <- function(x) {
  if ( x != 100) {
    return(ifelse(x < -90,x<- -90,ifelse(x > -30,x <- -30, x <- x)))
  }
}  


trainingData[,c(WAPs)]   <- mapply(replace_values,trainingData[,c(WAPs)])
validationData[,c(WAPs)] <- mapply(replace_values,validationData[,c(WAPs)])

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

#### Getting the confussion Matrix of the model on the training set ####
BFPred_cm <- confusionMatrix(trainingData$BUILDINGID_FLOOR, Highest_loc_train$Prediction)
#### As it can be seen, the accuracy is pretty small: 0.54, while the kappa
#### is 0.50. Therefore this model can only be used as a benchmark, but not for
#### production.
#### Close inspection of the Confusion Matrix (CM henceforth) shows that some
#### waps sometimes give the strongest signal in two different buildings. 
#### A possible explanation to is will be that in the current building-floor
#### the wifi coverage is poor. In order to dig more in this respect , a deeper
#### inspection of the errors should be conducted.
#### For this purpose, on can add and extra column which a mismatch betwee the
#### real location and the predicted coded in two colors: green = correct, red
#### = incorrect.
correctness <- function(x,y){
  return(ifelse(x==y,"Green","Red"))
}

trainingData$Error_BFPred <- mapply(correctness,Highest_loc_train$BFLocation,Highest_loc_train$Prediction)
trainingData$Error_BFPred <- as.factor(trainingData$Error_BFPred)

# Plotting the locations where this simple model makes the mistakes.
p1 <- plot_ly(trainingData, x = ~LONGITUDE, y = ~LATITUDE, z = ~FLOOR, color = ~Error_BFPred, colors = c('cyan', 'red'),
             size = 1.2) %>% add_markers() %>%
  layout(scene = list(xaxis = list(title = 'Longitude'),
                      yaxis = list(title = 'Latitude'),
                      zaxis = list(title = 'Floor')),
         title="Location of the errors")
p1

#### On the other hand, the same CM inspection shows that is almost block diagonal,
#### therefore this model could be much better in predicting only the building. 
#### Let's see it performs. 

#### In order to do this, we only need to get the first digit of the predicted
#### building-floor.

#### building prediction #Accuracy: 0.97 Kappa: 0.96 on the training set.
#### By simple inspection of the Confussion Matrix, it can be seen that most
#### of the mistakes made by this simple model takes place in the second building
#### which is between the others. Signal receptions from WAPs of the others buildings
#### confuses the model.
Highest_loc_train$BLocation   <- str_sub(Highest_loc_train$BFLocation, start = 1, end = 1)
Highest_loc_train$BPrediction <- str_sub(Highest_loc_train$Prediction, start = 1, end = 1)

Highest_loc_train$BLocation   <- as.factor(Highest_loc_train$BLocation)
Highest_loc_train$BPrediction <- as.factor(Highest_loc_train$BPrediction)

#### As it could be expected, this new modelhas better accuracy and kappa on the
#### training set.
#### Accuracy: 0.97, Kappa: 0.96
BPredic_cm <- confusionMatrix(Highest_loc_train$BLocation,Highest_loc_train$BPrediction)


# explore waps that affect multiple buildings #12
#### Ignacio: The rigth question is, which waps are being detected in several buildings
#### and where? Those WAPs should be removed in order to avoid confusing the models
#### Another interesting question is why this happens.
Highest_loc_train$BFCorrect <- Highest_loc_train$BFLocation == Highest_loc_train$Prediction
BadWAPs <- unique(Highest_loc_train[which(Highest_loc_train$BFCorrect==FALSE),]$WAP)
trainingData   <- trainingData[,!(colnames(trainingData) %in% BadWAPs), drop = FALSE]
validationData <- validationData[,!(colnames(validationData) %in% BadWAPs), drop = FALSE]

Highest_loc_train <- Highest_loc_train[which(Highest_loc_train$BFCorrect==TRUE),]

trainingData$Top3Waps <- apply(trainingData[,c(WAPs)], 1, function(x) 
  paste(names(head(sort(x, decreasing = TRUE), 3)), collapse = " "))

trainingData <- separate(trainingData, Top3Waps, c("TopWap1", "TopWap2","TopWap3"))

trainingData$TopWap1 <- factor(trainingData$TopWap1, levels = WAPs)
trainingData$TopWap2 <- factor(trainingData$TopWap2, levels = WAPs)
trainingData$TopWap3 <- factor(trainingData$TopWap3, levels = WAPs)

trainingData[trainingData == 100] <- -100
validationData[validationData == 100] <- -100


