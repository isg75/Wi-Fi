# Load data
trainingData <- read_csv("C:/Users/FDL_4/OneDrive/Escritorio/Course/Module 4/Task 1/UJIndoorLoc/trainingData.csv")
validationData <- read_csv("C:/Users/FDL_4/OneDrive/Escritorio/Course/Module 4/Task 1/UJIndoorLoc/validationData.csv")

# Pre processing ####
# feature type
trainingData$FLOOR <- as.factor(trainingData$FLOOR)
trainingData$BUILDINGID <- as.factor(trainingData$BUILDINGID)
trainingData$SPACEID <- as.factor(trainingData$SPACEID)
trainingData$RELATIVEPOSITION <- as.factor(trainingData$RELATIVEPOSITION)
trainingData$USERID <- as.factor(trainingData$USERID)
trainingData$PHONEID <- as.factor(trainingData$PHONEID)
trainingData$TIMESTAMP <- as_datetime(trainingData$TIMESTAMP)

validationData$FLOOR <- as.factor(validationData$FLOOR)
validationData$BUILDINGID <- as.factor(validationData$BUILDINGID)
validationData$SPACEID <- as.factor(validationData$SPACEID)
validationData$RELATIVEPOSITION <- as.factor(validationData$RELATIVEPOSITION)
validationData$USERID <- as.factor(validationData$USERID)
validationData$PHONEID <- as.factor(validationData$PHONEID)
validationData$TIMESTAMP <- as_datetime(validationData$TIMESTAMP)

# combine data
trainingData <- bind_rows(trainingData, validationData)

str(trainingData[520:529])
trainingData$SPACEID <- as.factor(trainingData$SPACEID)
trainingData$RELATIVEPOSITION <- as.factor(trainingData$RELATIVEPOSITION)
trainingData$USERID <- as.factor(trainingData$USERID)
trainingData$PHONEID <- as.factor(trainingData$PHONEID)
rm("validationData")

# NA
sum(is.na(trainingData), na.rm = TRUE)
sum(is.na(validationData), na.rm = TRUE)

# duplicated rows #637
trainingData <- trainingData[!duplicated(trainingData), ]

#lat lon plot
plot(trainingData$LONGITUDE, trainingData$LATITUDE, pch = 19, main = "Training Data",
     xlab = "Longitude", ylab = "Latitude")

plot(validationData$LONGITUDE, validationData$LATITUDE, pch = 19, main = "Validation Data",
     xlab = "Longitude", ylab = "Latitude")

#lat lon building 1 second floor
trainingDataq <- filter(trainingData, BUILDINGID == 1)
trainingDataq <- filter(trainingDataq, FLOOR == 2)

validationDataq <- filter(validationData, BUILDINGID == 1)
validationDataq <- filter(validationDataq, FLOOR == 2)

plot(trainingDataq$LONGITUDE, trainingDataq$LATITUDE, pch = 19, main = "Training Data",
     xlab = "Longitude", ylab = "Latitude")

plot(validationDataq$LONGITUDE, validationDataq$LATITUDE, pch = 19, main = "Validation Data",
     xlab = "Longitude", ylab = "Latitude")

rm("trainingDataq", "validationDataq")

# rows with all na
# combined data #73
Data_T0 <- trainingData
trainingData <- subset(trainingData, select = -c(LONGITUDE, LATITUDE, FLOOR, BUILDINGID, SPACEID,
                                                 RELATIVEPOSITION, USERID, PHONEID, TIMESTAMP))
trainingData[trainingData == 100] <- NA

ind <- apply(trainingData, 1, function(x) all(is.na(x)))
sum(ind)
Data_T0 <- Data_T0[!ind, ]
trainingData <- trainingData[!ind, ]

trainingData <- cbind(trainingData, Data_T0[, c("LONGITUDE", "LATITUDE", "FLOOR", "BUILDINGID", "SPACEID",
                                                "RELATIVEPOSITION", "USERID", "PHONEID", "TIMESTAMP")])

rm("ind")

# columns that have the same value for every row
# training data #55
WAPs <- grep("WAP", names(Data_T0), value=T)
colvar0_td <- apply(Data_T0[, c(WAPs)], 2, function(x) var(x, na.rm = T) == 0)
Waps0var_td <- names(Data_T0[, c(WAPs)])[colvar0_td]

# remove the found columns
Data_T0 <- Data_T0[, !(colnames(Data_T0) %in% Waps0var_td), drop = FALSE]
trainingData <- trainingData[, !(colnames(trainingData) %in% Waps0var_td), drop = FALSE]
WAPs <- grep("WAP", names(Data_T0), value=T)

rm("colvar0_td", "Waps0var_td")

#relocate very good and very bad signals
WAPs <- grep("WAP", names(Data_T0), value=T)
dat1 <- melt(Data_T0[, WAPs])
dat1[dat1 == 100] <- NA
plot(density(dat1$value, na.rm = TRUE))

dat2 <- melt(validationData[1:520])
dat2[dat2 == 100] <- NA
plot(density(dat2$value, na.rm = TRUE))

plot(density(dat1$value, na.rm = TRUE), xaxt="n", lwd = 2, main = "RSSI Density", xlab = "RSSI")
lines(density(dat2$value, na.rm = TRUE), col="red", lwd = 2)
axis(1, at = seq(-100, 0, by = 10))
legend(-45, 0.15, col=c("black", "red"), c("Training", "Validation"), pch=c(19))

density(dat1$value, na.rm = TRUE)$x[which.max(density(dat1$value, na.rm = TRUE)$y)]

a <- density(dat1$value, na.rm = TRUE)$x[which.max(density(dat1$value, na.rm = TRUE)$y)]

dat1$value[dat1$value < a] <- a
dat2$value[dat2$value < a] <- a

dat1$value[dat1$value > -29] <- -29
dat2$value[dat2$value > -29] <- -29

#transform training
WAPs <- grep("WAP", names(Data_T0), value=T)
Data_T0t <- Data_T0[, WAPs]
Data_T0t[Data_T0t == 100] <- NA
Data_T0t[Data_T0t < a] <- a
Data_T0t[Data_T0t > -29] <- -29
Data_T0 <- cbind(Data_T0t, Data_T0[, c("LONGITUDE", "LATITUDE", "FLOOR", "BUILDINGID", "SPACEID",
                                                      "RELATIVEPOSITION", "USERID", "PHONEID", "TIMESTAMP")])
Data_T0[is.na(Data_T0)] <- 100

plot(density(Data_T0t$WAP173, na.rm = TRUE))

#transform validation
WAPs <- grep("WAP", names(validationData), value=T)
validationData1 <- validationData[, WAPs]
validationData1[validationData1 == 100] <- NA
validationData1[validationData1 < a] <- a
validationData1[validationData1 > -29] <- -29
validationData <- cbind(validationData1, validationData[, c("LONGITUDE", "LATITUDE", "FLOOR", "BUILDINGID", "SPACEID",
                                                      "RELATIVEPOSITION", "USERID", "PHONEID", "TIMESTAMP")])
validationData[is.na(validationData)] <- 100

plot(density(validationData1$WAP173, na.rm = TRUE))

#rm("dat1", "dat2", "a", "Data_T0t", "validationData1")

# create BUILDINGID_FLOOR feature
Data_T0 <- unite(Data_T0, BUILDINGID_FLOOR, c("BUILDINGID", "FLOOR"), sep = "_", remove = FALSE)
Data_T0$BUILDINGID_FLOOR <- as.factor(Data_T0$BUILDINGID_FLOOR)

# find waps that affect multiple buildings and floors ####
# highest wap signal for each observation
Data_T1 <- Data_T0
Data_T0[Data_T0 == 100] <- -100
Data_T1[Data_T1 == 100] <- NA
Data_T1 <- subset(Data_T1, select = -c(LONGITUDE, LATITUDE, BUILDINGID_FLOOR, FLOOR, BUILDINGID,
                                            SPACEID, RELATIVEPOSITION, USERID, PHONEID, TIMESTAMP))

HighestWap <- names(Data_T1)[apply(Data_T1, 1, which.max)]

# to which building_floor every wap is sending his highest signal
Data_T1$BUILDINGID_FLOOR <- Data_T0$BUILDINGID_FLOOR

WAPs <- grep("WAP", names(Data_T1), value=T)
BFLocation <- apply(Data_T1[, c(WAPs)], 2, function (x)
  names(which.max(table(Data_T1[which(!is.na(x)), "BUILDINGID_FLOOR"]))))

# building and floor prediction #Accuracy: 0.7722
HighestWap <- as.data.frame(HighestWap)
names(HighestWap)[1] <- "WAP"
BFLocation <- as.data.frame(BFLocation)

BFLocation <- data.frame(WAP = rownames(BFLocation), location = BFLocation$BFLocation)

BFPrediction <- left_join(HighestWap, BFLocation)
BFPrediction$BUILDINGID_FLOOR <- Data_T0$BUILDINGID_FLOOR

confusionMatrix(table(BFPrediction$BUILDINGID_FLOOR, BFPrediction$location))

# explore waps that affect multiple buildings and floors #175
BFPrediction$Error <- BFPrediction$location == BFPrediction$BUILDINGID_FLOOR
BFPredictionErrors <- BFPrediction[BFPrediction$Error == FALSE, ]
unique(BFPredictionErrors$WAP)
BFErrors <- unique(BFPrediction[BFPrediction$Error == FALSE, ]$WAP)
Data_T0_BFE <- Data_T0[, !(colnames(Data_T0) %in% BFErrors), drop = FALSE]

plot(Data_T1$BUILDINGID_FLOOR, Data_T1$WAP173)
plot(density(Data_T1$WAP173, na.rm = TRUE))

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


#normalize test
#Data_T0[Data_T0 == -100] <- NA
#WAPs <- grep("WAP", names(Data_T0), value=T)
#Data_T01 <- Data_T0[, WAPs]
#Data_T01 <- abs(Data_T01)
#Data_T01 <- as.data.frame(t(apply(Data_T01, 1, function(x, ...)(x - min(x, ...))/(max(x, ...) - min(x, ...)))))

#Data_T0[is.na(Data_T0)] <- -100

# test set pre processing
validationData <- read_csv("C:/Users/FDL_4/OneDrive/Escritorio/Course/Module 4/Task 1/UJIndoorLoc/Copy of Copy of testData - Copy of Copy of testData.csv")
View(validationData)
str(validationData[520:529])

validationData$FLOOR <- as.factor(validationData$FLOOR)
validationData$BUILDINGID <- as.factor(validationData$BUILDINGID)
validationData$SPACEID <- as.factor(validationData$SPACEID)
validationData$RELATIVEPOSITION <- as.factor(validationData$RELATIVEPOSITION)
validationData$USERID <- as.factor(validationData$USERID)
validationData$PHONEID <- as.factor(validationData$PHONEID)
validationData$TIMESTAMP <- as_datetime(validationData$TIMESTAMP)

WAPs <- grep("WAP", names(validationData), value=T)
validationData1 <- validationData[, WAPs]
validationData1[validationData1 == 100] <- NA
validationData1[validationData1 < a] <- a
validationData1[validationData1 > -29] <- -29
validationData <- cbind(validationData1, validationData[, c("LONGITUDE", "LATITUDE", "FLOOR", "BUILDINGID", "SPACEID",
                                                            "RELATIVEPOSITION", "USERID", "PHONEID", "TIMESTAMP")])
validationData[is.na(validationData)] <- -100


