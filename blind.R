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
