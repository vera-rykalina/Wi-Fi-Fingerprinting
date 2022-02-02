## Wi-Fi Locationing Modeling Validation

#### Load Packages  #####

pacman:: p_load("rstudioapi", "readr","dplyr", "tidyr", "ggplot2", "plotly", 
                "data.table", "reshape2","ggridges", "party",
                "esquisse", "caret", "randomForest",
                "hablar", "wesanderson","C50", "class", "rminer",                               "kernlab", "randomForest", "ggmap")


#### Setting Working Directory ####
current_working_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(current_working_dir)
getwd()

### Training Data
raw.table.train <- read_csv("./trainingData.csv") # dot - current directory
dim(raw.table.train)
head(raw.table.train)

## Validation Data
raw.table.validation <- read_csv("./validationData.csv") # dot - current directory
dim(raw.table.validation)
head(raw.table.validation)

## View features
glimpse(raw.table.train[,510:529])
str(raw.table.train[,510:529])

glimpse(raw.table.validation[,510:529])
str(raw.table.validation[,510:529])

## Preparing a long table
wide.table.train <- raw.table.train

long.table.train <- raw.table.train %>%
  gather(WAPid, WAPvalue, WAP001:WAP520)  
nrow(long.table.train)
colnames(long.table.train)

###################################

wide.table.validation <- raw.table.validation
nrow(wide.table.validation)

long.table.validation <- raw.table.validation %>%
  gather(WAPid, WAPvalue, WAP001:WAP520)  
nrow(long.table.validation)
colnames(long.table.validation)
head(long.table.validation)

#### Data Preprocessing ####

#### NA"s ####

wide.table.train[wide.table.train == 100] <- -105
head(wide.table.train)

long.table.train[,11][long.table.train[, 11] == 100] <- -105
long.table.train <- filter(long.table.train, long.table.train$WAPvalue != -105)

#################################################################################

wide.table.validation[wide.table.validation == 100] <- -105
head(wide.table.validation)

long.table.validation[,11][long.table.validation[, 11] == 100] <- -105
long.table.validation <- filter(long.table.validation, long.table.validation$WAPvalue != -105)


#### Data Cleaning ####

## Training

# Wide table

wide.table.train <- wide.table.train %>% convert(num(LONGITUDE, LATITUDE),   
                                                 fct(BUILDINGID, FLOOR, USERID, PHONEID,
                                                     RELATIVEPOSITION, SPACEID),
                                                 dtm(TIMESTAMP))


levels(wide.table.train$BUILDINGID) <- c("TI", "TD","TC")
levels(wide.table.train$RELATIVEPOSITION) <- c("Inside", "Outside")

# Long Table
long.table.train <- long.table.train %>% convert(num(LONGITUDE, LATITUDE),   
                                                 fct(BUILDINGID, FLOOR, USERID, PHONEID,
                                                     RELATIVEPOSITION, SPACEID),
                                                 dtm(TIMESTAMP))

levels(long.table.train$BUILDINGID) <- c("TI", "TD","TC")
levels(long.table.train$RELATIVEPOSITION) <- c("Inside", "Outside")




##########################################

## Validation
# Wide Table
wide.table.validation <- wide.table.validation %>% convert(num(LONGITUDE, LATITUDE),   
                                                 fct(BUILDINGID, FLOOR, USERID, PHONEID,
                                                     RELATIVEPOSITION, SPACEID),
                                                 dtm(TIMESTAMP))

levels(wide.table.validation$BUILDINGID) <- c("TI", "TD","TC")
levels(wide.table.validation$RELATIVEPOSITION) <- c("Inside", "Outside")

# Long Table

long.table.validation <- long.table.validation %>% convert(num(LONGITUDE, LATITUDE),   
                                                 fct(BUILDINGID, FLOOR, USERID, PHONEID,
                                                     RELATIVEPOSITION, SPACEID),
                                                 dtm(TIMESTAMP))

levels(long.table.validation$BUILDINGID) <- c("TI", "TD","TC")
levels(long.table.validation$RELATIVEPOSITION) <- c("Inside", "Outside")


#### Constant Values Check ####
nrow(wide.table.validation)
ncol(wide.table.validation)
nrow(wide.table.train)
ncol(wide.table.train)

# Training Table
constant.value.train <- nearZeroVar(wide.table.train[1:520], saveMetrics = TRUE) 
sum(constant.value.train$zeroVar == TRUE)  # 55 WAPs have values with variance =0 (constants)
wide.table.train <- wide.table.train[-which(constant.value.train$zeroVar == TRUE)] # removing the constants as they might affect modeling


###########################################################

# Validation Table
constant.value.validation <- nearZeroVar(wide.table.validation[1:520], saveMetrics = TRUE) 
wide.table.validation <- wide.table.validation[-which(constant.value.train$zeroVar == TRUE)] # removing the same constants as in Training set- 55 


############################################################

post.var.waps.train <- grep("WAP", names(wide.table.train), value = TRUE) # grep 465 WAPs remaining after applying zeroVar
dim(wide.table.train)

post.var.waps.validation <- grep("WAP", names(wide.table.validation), value = TRUE) # grep 465 WAPs remaining after applying zeroVar
dim(wide.table.validation)


# Duplicates (there no duplicates in both tables)
anyDuplicated(wide.table.validation)


# Treatment of Duplicate Values in Training Tables
wide.table.train <- unique(wide.table.train)
long.table.train <- unique(long.table.train)
dim(wide.table.train)

# Treatment of Duplicate Values in Training Tables
wide.table.validation <- unique(wide.table.validation)
long.table.validation <- unique(long.table.validation)
dim(wide.table.validation)

#### Exploratory Analysis #####

# Training Table -> There are too high signals in  building TC
ggplot(data = long.table.train) +
  aes(x = WAPvalue, fill = FLOOR) +
  geom_histogram(bins = 30, color="black") +
  labs(x='Value of WAP', y='Counts', title='WAP Distribution based on Location', subtitle = "Specified by Floor") +
  scale_fill_manual(name="Floor", values = wes_palette("Royal2", n = 5)) +
  theme_minimal() +
  facet_wrap(vars(BUILDINGID)) +
  theme(legend.position="bottom")

# Validation Table -> There are too high signals in  building TI
ggplot(data = long.table.validation) +
  aes(x = WAPvalue, fill = FLOOR) +
  geom_histogram(bins = 30, color="black") +
  labs(x='Value of WAP', y='Counts', title='WAP Distribution based on Location', subtitle = "Specified by Floor") +
  scale_fill_manual(name="Floor", values = wes_palette("Royal2", n = 5)) +
  theme_minimal() +
  facet_wrap(vars(BUILDINGID)) +
  theme(legend.position="bottom")


## Signals are recorded by location and floor in train dataset (visualize buildings shape and distribution)
pal <- wes_palette(10, name = "Royal1", type = "continuous")
ggplot(data = long.table.train) +
  aes(x = LONGITUDE, y = LATITUDE, color=FLOOR) +
  geom_point() +
  labs(x='Longitude', y='Latitude') +
  scale_color_manual(name ="Floor", values = pal) +
  theme_minimal() +
  theme(legend.position="bottom")

## Signals are recorded by location and floor in validation dataset (visualize buildings shape and distribution)
pal <- wes_palette(10, name = "Royal1", type = "continuous")
ggplot(data = long.table.validation) +
  aes(x = LONGITUDE, y = LATITUDE, color=FLOOR) +
  geom_point() +
  labs(x='Longitude', y='Latitude') +
  scale_color_manual(name ="Floor", values = pal) +
  theme_minimal() +
  theme(legend.position="bottom")

#### WAP values with too high signals ####
## Removing records with WAP values >-30dbm (long table)
long.table.train <- long.table.train %>% filter(WAPvalue <= -30)
long.table.validation <- long.table.validation %>% filter(WAPvalue <= -30)

waps.train <- grep("WAP", names(wide.table.train), value = TRUE) # 465 WAPs 
dim(wide.table.train) # 19300 x 474
waps.validation <- grep("WAP", names(wide.table.validation), value = TRUE) # 465 WAPs 
dim(wide.table.validation) # 1111 x 474


## Filter to exclude the rows that contain values above -30dbm in WAP columns
wide.table.train <- wide.table.train %>% filter_at(vars(contains("WAP")), all_vars(. < -30))
dim(wide.table.train) # 18792 x 474
###############################
wide.table.validation <- wide.table.validation %>% filter_at(vars(contains("WAP")), all_vars(. < -30))
dim(wide.table.validation) # 1111 x 474


#### Analysis of signals ####
# Training Table
dim(wide.table.train) # 18792 x 474
wide.table.train <- wide.table.train %>% filter_at(vars(contains("WAP")), any_vars(. > -90)) # filter WAPs with values >-90 to take them for modelling
dim(wide.table.train) # 18607 x 474
wide.table.train$WAP248 <- NULL
dim(wide.table.train) # 18607 x 473
wide.table.train$WAP427 <- NULL
dim(wide.table.train) # 18607 x 472
post.var.waps.train <- grep("WAP", names(wide.table.train), value = TRUE) # grep 463 WAPs remaining after removing WAP 248 and WAP 427

####################################################
# Validation Table by Train
dim(wide.table.validation) # 1111 x 474
wide.table.validation <- wide.table.validation %>% filter_at(vars(contains("WAP")), any_vars(. > -90)) # filter WAPs with values >-90 to take them for modelling
dim(wide.table.validation) # 1109 x 474
wide.table.validation$WAP248 <- NULL
dim(wide.table.validation) # 1109 x 473
wide.table.validation$WAP427 <- NULL
dim(wide.table.validation) # 1109 x 472
post.var.waps.validation <- grep("WAP", names(wide.table.validation), value = TRUE) # grep 463 WAPs remaining after removing WAP 248 and WAP 427


#### PCA ####
validation_PCA <- predict(compress, newdata = wide.table.validation)
# valid_pca <- predict(pca.train, newdata = wide.table.validation)

# Models (Random Forest is the best)
rfBuildingFit1 <-readRDS(file = "models/rfBuildingFit1.rds") # Building
xgbFloorFit1 <-readRDS(file = "models/xgbFloorFit1.rds") # Floor
rfLongitudeFit1 <-readRDS(file = "models/rfLongitudeFit1.rds") # Longitude
rfLatitudeFit1 <-readRDS(file = "models/rfLatitudeFit1.rds") # Latitude

predictedValidationBuilding <- predict(rfBuildingFit1, newdata=validation_PCA)
predictedValidationFloor <- predict(xgbFloorFit3, newdata=validation_PCA)
predictedValidationFloor <- predict(xgbFloorFit1, newdata=validation_PCA)
predictedValidationLongitude <- predict(rfLongitudeFit1, newdata=validation_PCA)
predictedValidationLatitude <- predict(rfLatitudeFit1, newdata=validation_PCA)

confusionMatrix(predictedValidationFloor, validation_PCA$FLOOR)

class(predictedValidationBuilding)
class(predictedValidationFloor)
class(predictedValidationLongitude)
class(predictedValidationLatitude)

predictedValidationBuilding <- as.numeric(predictedValidationBuilding)
predictedValidationFloor <- as.numeric(predictedValidationFloor)

validation_PCA_N <- validation_PCA
str(validation_PCA_N)
validation_PCA_N$BUILDINGID <-as.numeric(validation_PCA_N$BUILDINGID)
validation_PCA_N$FLOOR <-as.numeric(validation_PCA_N$FLOOR)

is.data.frame(validation_PCA_N)
final.table <- validation_PCA_N[,1:4]
nrow(final.table)
length(predictedValidationBuilding)

final.table$LONGITUDE_P <-predictedValidationLongitude
final.table$LATITUDE_P <-predictedValidationLatitude
final.table$FLOOR_P <-predictedValidationFloor
final.table$BUILDINGID_P <-predictedValidationBuilding

#### Coputing Distance and Error
head(final.table)
final.table$DISTANCE <- sqrt((final.table[,5]-final.table[,1])^2 + (final.table[,6]-final.table[,2])^2)
final.table$ERROR <- final.table$DISTANCE + 4*abs(final.table[,7]-final.table[,3]) +50*abs(final.table[,8]-final.table[,4])

quantile(final.table$ERROR)
max(final.table$ERROR)

#### Error Boxplot ####
pal <- wes_palette(4, name = "Royal1", type = "continuous")
ggplot(final.table, aes(y=ERROR)) + 
  labs(x="Error", y="Meters") +
  geom_boxplot() +
  theme(legend.position="bottom", legend.title=element_blank())


#### Error Boxplot ####
pal <- wes_palette(4, name = "Royal1", type = "continuous")
ggplot(final.table, aes(y=ERROR, color="ERROR")) + 
  labs(x="Error", y="Meters", title='Error Rate') +
  geom_boxplot(outlier.color = NA) +
  scale_y_continuous(limits = c(-5, 30), breaks = seq(-10,30,5))+
  scale_fill_manual(name ="Error", values = pal) +
  theme(legend.position="none", legend.title=element_blank())

#### Error Violinplot ####
pal <- wes_palette(1, name = "Royal1", type = "continuous")
ggplot(final.table, aes(x="ERROR", y=ERROR, fill="ERROR")) + 
  labs(x="Error", y="Meters", title='Error Rate') +
  geom_violin(trim=TRUE, draw_quantiles = c(0.25, 0.5, 0.75)) +
  scale_y_continuous(limits = c(-10, 60), breaks = seq(-10,140,10))+
  scale_fill_manual(name ="Error", values = pal) +
  theme(legend.position="none", legend.title=element_blank())



  
#### Error Violinplot ####
pal <- wes_palette(1, name = "Royal1", type = "continuous")
ggplot(final.table, aes(x=ERROR, y=ERROR, fill=ERROR)) + 
  labs(x="Error", y="Meters", title='Error Rate') +
  geom_violin(trim=FALSE, draw_quantiles = c(0.25, 0.5, 0.75), color="black") +
  scale_fill_manual(name ="Error", values = pal) +
  theme(legend.position="none", legend.title=element_blank())


## Signals are recorded by location and floor in validation dataset (predicted)
pal <- wes_palette(7, name = "Royal1", type = "continuous")
ggplot(data = final.table) +
  aes(x = LONGITUDE_P, y = LATITUDE_P, color=as.factor(FLOOR_P)) +
  geom_point() +
  labs(x='Longitude', y='Latitude', title='Prediction (Validation Dataset)')+
  scale_color_manual(name ="Floor", values = pal) +
  theme_minimal() +
  theme(legend.position="bottom")

#### DISTANCE ####
## error = distanceˡᵃᵗ,ˡᵒⁿ(estimated, real) + 4 ∗ abs(estimated_floor – real_floor) + 50 ∗abs(estimated_bld – real_bld)


#pca2 <- prcomp(training_WAPs, scale. = T)
#valid_pca <- predict(pca2, newdata = Data_Full[Data_Full$source=="DataValid",])
#valid_pca <- valid_pca[, 1:120]
#combine(DataTrain, DataValid)
#DataFull
#https://rpubs.com/Mentors_Ubiqum/Spike_Computational_cost


# https://rpubs.com/SaraMarin/AWS







