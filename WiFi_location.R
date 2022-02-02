## Wi-Fi Locationing

## dBm - Decibels in relation to a milliwatt (usually -30 to -100)
## https://www.metageek.com/training/resources/wifi-signal-strength-basics.html

#### Load Packages  #####

pacman:: p_load("rstudioapi", "readr","dplyr", "tidyr", "ggplot2", "plotly", 
                "data.table", "reshape2","ggridges", "party",
                "esquisse", "caret", "randomForest", "ochRe",
                "hablar", "wesanderson", "RColorBrewer", "ggsci", "gplots",
                "ggpubr")


#### Setting Working Directory ####
current_working_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(current_working_dir)
getwd()

#### Loading Data ####


## Training Data
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

#### Data Preprocessing ####

# Histogram WAP 12 distribution to understand 100 meaning
ggplot(data = wide.table.train) +
  aes(x = WAP012) +
  geom_histogram(bins = 10, aes(fill = "BrBg"), color="black") +
  labs(x='WAP 012', y='Counts', title="Distribution of WAP012") +
  theme_minimal() + theme(legend.position = "none")
ggplotly(p = ggplot2::last_plot())

#### NA"s ####
wide.table.train[wide.table.train == 100] <- -105
head(wide.table.train)

long.table.train[,11][long.table.train[, 11] == 100] <- -105
long.table.train <- filter(long.table.train, long.table.train$WAPvalue != -105)


#### Data Cleaning ####

## Wide table

wide.table.train <- wide.table.train %>% convert(num(LONGITUDE, LATITUDE),   
                                     fct(BUILDINGID, FLOOR, USERID, PHONEID,
                                         RELATIVEPOSITION, SPACEID),
                                     dtm(TIMESTAMP))

is.data.frame(wide.table.train)
# check -> sapply(wide_train, class)
NumClasses <- apply(wide.table.train[,c("LONGITUDE", "LATITUDE")], 2, class)
NumClasses <- apply(wide.table.train[,c(1:520)], 2, class)
NumClasses <- apply(wide.table.train[,c(1:520)], 2, class)
NumClasses

levels(wide.table.train$BUILDINGID) <- c("TI", "TD","TC")
levels(wide.table.train$RELATIVEPOSITION) <- c("Inside", "Outside")

## Long Table

long.table.train <- long.table.train %>% convert(num(LONGITUDE, LATITUDE),   
                                                 fct(BUILDINGID, FLOOR, USERID, PHONEID,
                                                     RELATIVEPOSITION, SPACEID),
                                                 dtm(TIMESTAMP))

levels(long.table.train$BUILDINGID) <- c("TI", "TD","TC")
levels(long.table.train$RELATIVEPOSITION) <- c("Inside", "Outside")

head(long.table.train)

#### Constant Values Check ####
## nearZeroVar diagnoses predictors that have one unique value (i.e. are zero variance predictors) or predictors that are have both of the following characteristics: they have very few unique values relative to the number of samples and the ratio of the frequency of the most common value to the frequency of the second most common value is large. checkConditionalX looks at the distribution of the columns of x conditioned on the levels of y and identifies columns of x that are sparse within groups of y.

# check if there are constant WAPs (means variance is 0) with caret nearZeroVar()

constant.value.train <- nearZeroVar(wide.table.train[1:520], saveMetrics = TRUE) 

sum(constant.value.train$zeroVar == TRUE)  # 55 WAPs have values with variance =0 (constants)

wide.table.train <- wide.table.train[-which(constant.value.train$zeroVar == TRUE)] # removing the constants as they might affect modeling


rm(constant.value.train)
ls()

post.var.waps.train <- grep("WAP", names(wide.table.train), value = TRUE) # grep 465 WAPs remaining after applying zeroVar
dim(wide.table.train)


# duplicates (2908)
anyDuplicated(wide.table.train)
duplicates.table <- as.data.frame(duplicated(wide.table.train))

ggplot(duplicates.table) +
  aes(x = `duplicated(wide.table.train)`) +
  geom_bar(aes(fill = "BrBG"), color="black") +
  labs(title = "Duplicate Values",
       x = "Duplicated Observations",
       y = "Observations") +
  theme_minimal() + theme(legend.position = "none")


# Treatment of Duplicate Values
wide.table.train <- unique(wide.table.train)
long.table.train <- unique(long.table.train)
dim(wide.table.train)
names(wide.table.train)

#### Exploratory Analysis #####

## Signal are recorded by Building and Floor in dataset (-> Building TC )
# Open question: Will it affect a validation dataset?
# Conclusion: There are too high signals in  building TC

ggplot(data = long.table.train) +
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
  labs(x='Longitude', y='Latitude', title='Visualization of Campus Buildings', subtitle = "Specified by Floor") +
  scale_color_manual(name ="Floor", values = pal) +
  theme_minimal() +
  theme(legend.position="bottom")




#### WAP values with too high signals ####
## Signals > -30 dBm are extremely rare to happen in normal conditions; we need to analyse if there is something wrong in these records, therefore we isolate signals > than -30

## Investigate signals which are greater than -30 dBm
greaterMinus30 <- long.table.train %>% filter(WAPvalue > -30)


## Boxplot: Visualize to high signals (by WAPs in each building)
# Conclusion: almost all too high signals come from TC building besides one WAP in building TD
pal <- wes_palette(10, name = "Royal1", type = "continuous")
ggplot(data = greaterMinus30) +  
  aes(x = WAPid, y = WAPvalue, fill = BUILDINGID) +  
  geom_boxplot() +  
  labs(x='WAP ID', y='WAP Value', title='Abnormally High Signals', subtitle = "Specified by Building") +
  scale_fill_manual(name ="Building", values = pal) +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(legend.position="bottom")



## Histogram: Visualize to high signals (by Building and Floor)
# Conclusion:  Too high RSSI occur in floor 3 and 4 of the building TC
pal <- wes_palette(10, name = "Royal1", type = "continuous")
ggplot(data = greaterMinus30) +        
  aes(x = WAPvalue, fill = FLOOR) +
  geom_histogram(bins = 30, color="black") +
  labs(x='Value of WAP', y='Counts', title='WAP Distribution based on Building', subtitle = "Specified by Floor") +
  scale_fill_manual(name="Floor", values = pal) +
  theme_minimal() +
  facet_wrap(vars(BUILDINGID)) +
  theme(legend.position="bottom")


## Histogram: Visualize, if abnormally good signals come form specific user/phone model
# Conclusion 1: Phone 19 is responsible for the vast majority of too high signals
# Conclusion 2: Phone 19 is used by User 6 (max responsible for >-30dbm RSSI)

# Phone ID check
ggplot(data = greaterMinus30) +      
  aes(x = PHONEID, fill = FLOOR) +
  geom_bar(color="black") +
  labs(x='Phone ID', y='Counts', title='Abnormally High Signal', subtitle = "Specified by Phone Model") +
  scale_fill_manual(name ="Floor", values = pal) +
  theme_minimal() + 
  theme(legend.position="bottom") +
  facet_wrap(vars(BUILDINGID))     

# User ID check
pal <- wes_palette(10, name = "Royal1", type = "continuous")
ggplot(data = greaterMinus30) +
  aes(x = USERID, fill = FLOOR) +
  geom_bar(color="black") +
  labs(x='User ID', y='Counts', title='Abnormally High Signal', subtitle = "Specified by User") +
  scale_fill_manual(name ="Floor", values = pal) +
  theme_minimal() + 
  theme(legend.position="bottom") +
  facet_wrap(vars(BUILDINGID)) 


#### Closer investigation of User 6 behavioural pattern ####

## Feature engineering
# Creating a new attribute BUILDINGID-FLOOR
long.table.train$BuildingFloor <-  paste(long.table.train$BUILDINGID, long.table.train$FLOOR, sep = "-")
head(long.table.train)

## Checking all WAPs recods of User 6 
User6 <- long.table.train %>% filter(USERID == 6)
table(User6$WAPvalue)
summary(User6$WAPvalue)

## Histogram: Visualize, if all signals of User 6 are biased
# Conclusion: Decision should be made whether this User should be removede (more than 80% of data are affected)

ggplot(data = User6) +
  aes(x = WAPvalue) +
  geom_histogram(bins = 30, aes(fill="BrBG"), color="black") +
  labs(x='Value of WAP', y='Counts', title='WAP Signal Distribution', subtitle = "User 6") +
  theme_minimal() + theme(legend.position = "none")

## Histogram: Visualize WAPs in TC, floor 3 and 4
# Conclusion: We cannot remove this user completely

# Exploration of data in TC, floor 3
pal <- wes_palette(10, name = "Royal1", type = "continuous")
TCfloor3 <- long.table.train %>% 
  filter(BuildingFloor == "TC-3") %>%
  ggplot() +    
  aes(x = WAPvalue, fill = USERID) +
  geom_histogram(bins = 30, color="black") +
  labs(x='Value of WAP', y='Counts', title='WAP Signal of User 6', subtitle = "TC  Floor 3") +
  scale_fill_manual(name ="User ID", values = pal) +
  theme_minimal() + theme(legend.position="bottom")
TCfloor3

# Exploration of data in TC, floor 4
pal <- wes_palette(10, name = "Royal1", type = "continuous")
TCfloor4 <- long.table.train %>% 
  filter(BuildingFloor == "TC-4") %>%
  ggplot() +    
  aes(x = WAPvalue, fill = USERID) +
  geom_histogram(bins = 30, color="black") +
  labs(x='Value of WAP', y='Counts', title='WAP Signal of User 6', subtitle = "TC  Floor 4") +
  scale_fill_manual(name ="User ID", values = pal) +
  theme_minimal() + theme(legend.position="bottom")
TCfloor4

# Conclusion: User 6 captured a big proportion of data in TC3- and TC-4

## Visualize contrubution of User 6 to total data amount
# Conclusion: although we could still predict TC3, TC4 would end up with really few data 
pal <- wes_palette(20, name = "Royal1", type = "continuous")
TCtotal <- long.table.train %>% 
  filter(BUILDINGID == "TC") %>%
  ggplot() +
  aes(x = FLOOR, fill = USERID, weight = WAPvalue) +
  geom_bar(color="black") +                  
  labs(x='Floor', y='Counts', title='Contrubution of Users to Total Data Amount', subtitle = "Specified by User ID") +
  scale_fill_manual(name= "User ID", values=pal) +
  theme_minimal() + 
  theme(legend.position="bottom")  
TCtotal

rm(User6)
rm(TCfloor3, TCfloor4, TCtotal)

## Signals by location for each floor (visualize buildings shape and distribution)
pal <- wes_palette(10, name = "Royal1", type = "continuous")
ggplot(data=long.table.train) +
  aes(x = LONGITUDE, y = LATITUDE, color=FLOOR) +
  geom_point() +
  labs(x='Longitude', y='Latitude', title='Visualization of Campus Buildings', subtitle = "Specified by Floor") +
  scale_color_manual(name="Floor", values = pal) +
  theme_minimal() +
  theme(legend.position="bottom") +
  facet_wrap(vars(as.factor(FLOOR)))


## Removing records with WAP values >-30dbm (long table)
long.table.train <- long.table.train %>% filter(WAPvalue <= -30)

waps.train <- grep("WAP", names(wide.table.train), value = TRUE) # 465 WAPs 
dim(wide.table.train) # 19300 x 474

## Filter to exclude the rows that contain values above -30dbm in WAP columns
wide.table.train <- wide.table.train %>% filter_at(vars(contains("WAP")), all_vars(. < -30))
dim(wide.table.train) # 18792 x 474
colnames(wide.table.train)
is.data.frame(wide.table.train)

#### Analysis of signals ####
# this will help us group the signals by intensity, and consequently help us estimate
# the location of the WAPs in relation to the users (useful for building prediction)
long.table.train$signalStrength <- ifelse(long.table.train$WAPvalue >=-67, "1. Amazing",
                            ifelse(long.table.train$WAPvalue >=-70, "2. Very Good",
                            ifelse(long.table.train$WAPvalue >=-80, "3. Okay",
                            ifelse(long.table.train$WAPvalue >=-90, "4. Not Good",
                                                        "5. Unusable"))))



## Signal intensity distribution by building 
# Conclusion: the majority of the signals in each building are bad, we should try to only keep the amazing signals
pal <- wes_palette(10, name = "Royal1", type = "continuous")
ggplot(data = long.table.train) +
  aes(x = WAPvalue, fill = signalStrength) +
  geom_histogram(bins = 30, color="black") +
  labs(x='Value of WAP', y='Counts', title='WAP Signal Strength', subtitle = "Arranged by quality") +
  scale_fill_manual(name ="Signal strength", values = pal) +
  theme_minimal() + theme(legend.position="bottom") +
  facet_wrap(vars(BUILDINGID)) 



## Coverage by amazing signals (do they cover all buildings?) 
amazing.signals.df <- filter(long.table.train, long.table.train$signalStrength =="1. Amazing")



## Comparison of all waps records...
ggplot(data = long.table.train) +       
  aes(x = LONGITUDE, y = LATITUDE) +
  geom_point(aes(color = "BrBg")) +
  labs(x='Longitude', y='Latitude', title='Visualization of All WAP Records') +
  theme_minimal() +
  theme(legend.position="none")


## Visualization of only Amazing signal WAPs  
# Conclusion: Amazing signals do not cover buildings completely!
ggplot(data = amazing.signals.df) +
  aes(x = LONGITUDE, y = LATITUDE) +
  labs(x='Longitude', y='Latitude', title='Signals with Amazing Quality') +
  geom_point(aes(color = "BrBg")) +
  theme_minimal() +
  theme(legend.position="none")

#rm(amazing.signals.df)


## What if we consider "Amazing & Very Good signals"?
good.signals.long <- filter(long.table.train, long.table.train$signalStrength =="1. Amazing" | long.table.train$signalStrength =="2. Very Good"
               | long.table.train$signalStrength =="2. Okay"
               | long.table.train$signalStrength =="2. Not Good") 

good.signals.wide <- wide.table.train %>% filter_at(vars(contains("WAP")), any_vars(. > -90)) # filter WAPs with values >-90 to take them for modelling
dim(good.signals.wide) # 18607 x 474

# Visualizing only Amazing and Vary good signal WAPs 
# Conclusion: although precision would increase, there are tiny areas (specially in building TD) that stay uncovered
ggplot(data = am.gd.signals.long) +
  aes(x = LONGITUDE, y = LATITUDE, fill = signalStrength) +
  geom_point(color = "#0c4c8a") +
  theme_minimal()       


## let's analyse this including FLOOR attribute to see the distribution of signals by quality in each building and floor

# signals per floor 3D plot in building TI  
pal <- wes_palette(10, name = "Royal1", type = "continuous")
buildingTI <- filter(long.table.train, long.table.train$BUILDINGID =="TI")
ggplot(data = buildingTI) +
  aes(x = LONGITUDE, y = LATITUDE, color = signalStrength) +
  labs(x='Longitude', y='Latitude', title='Visualization of Campus Buildings', subtitle = "Specified by Signal Strength") +
  geom_point() +
  scale_color_manual(name ="Signal Strength", values = pal) +
  theme_minimal() +
  theme(legend.position="bottom") +
  facet_wrap(vars(FLOOR))



  
# signals per floor 3D plot in building TD
pal <- wes_palette(10, name = "Royal1", type = "continuous")
buildingTD <- filter(long.table.train, long.table.train$BUILDINGID =="TD")
ggplot(data = buildingTD) +
  aes(x = LONGITUDE, y = LATITUDE, color = signalStrength) +
  labs(x='Longitude', y='Latitude', title='Visualization of Campus Buildings', subtitle = "Specified by Signal Strength") +
  geom_point() +
  scale_color_manual(name ="Signal Strength", values = pal) +
  theme_minimal() +
  theme(legend.position="bottom") +
  facet_wrap(vars(FLOOR))
   

# signals per floor 3D plot in building TC
pal <- wes_palette(10, name = "Royal1", type = "continuous")
buildingTC <- filter(long.table.train, long.table.train$BUILDINGID =="TC")
ggplot(data = buildingTC) +
  aes(x = LONGITUDE, y = LATITUDE, color = signalStrength) +
  labs(x='Longitude', y='Latitude', title='Visualization of Campus Buildings', subtitle = "Specified by Signal Strength") +
  geom_point() +
  scale_color_manual(name ="Signal Strength", values = pal) +
  theme_minimal() +
  theme(legend.position="bottom") +
  facet_wrap(vars(FLOOR))

############################## Overlaps ##################################
range(long.table.train$TIMESTAMP)

long.table.train %>%
  distinct(WAPid, BUILDINGID, FLOOR) %>%
  filter(BUILDINGID!="TC") %>%
  filter(FLOOR=="4")



overlaps.wide <- wide.table.train %>%
  select(WAP001:WAP465, BUILDINGID, FLOOR) %>%
  group_by(BUILDINGID, FLOOR) %>%
  arrange(FLOOR)


overlaps.TI <- wide.table.train %>%
  select(WAP001:WAP465, BUILDINGID, FLOOR) %>%
  filter(BUILDINGID== "TI")


overlaps.long <- long.table.train %>%
  distinct(WAPid, BUILDINGID) %>%
  group_by(WAPid,BUILDINGID) %>%
  arrange(WAPid, BUILDINGID) %>%
  summarise(HIT = n_distinct(BUILDINGID))


T0 <- long.table.train %>%
  distinct(WAPid, BUILDINGID) %>%
  group_by(WAPid, BUILDINGID) %>%
  summarise(HIT = n()) %>%
  summarise(TOTAL=sum(HIT)) %>%
  filter(TOTAL > 1)
dim(T0)

T1 <- long.table.train %>%
  distinct(WAPid, BUILDINGID) %>%
  group_by(WAPid,BUILDINGID) %>%
  arrange(WAPid, BUILDINGID) %>%
  summarise(HIT = n())

T2 <-tapply(T1$HIT, list(T1$WAPid, T1$BUILDINGID), sum)
T2 <- as.data.frame(T2)
T2[is.na(T2)] <- 0

T2$WAPid <- rownames(T2)
T2<-T2[, c("WAPid", "TI", "TD", "TC")]
row.names(T2) <- NULL

T2$TOTAL=T2$TI+T2$TD+T2$TC
head(T2)
dim(T2)

filter(T2, TOTAL==2)
filter(T2, TOTAL==3)

threeWAPid <-long.table.train %>%
  select(WAPid, WAPvalue, BUILDINGID) %>%
  filter(WAPid %in% c("WAP248", "WAP362","WAP413")) %>%
  group_by(BUILDINGID) %>%
  arrange(BUILDINGID) 

with(threeWAPid, tapply(WAPvalue, list(WAPid, BUILDINGID), sd))
table(threeWAPid$BUILDINGID)

pal3 <- wes_palette(3, name = "Royal1", type = "continuous")
WAP248 <-long.table.train %>%
  select(WAPid, WAPvalue, BUILDINGID) %>%
  filter(WAPid=="WAP248") %>%
  group_by(BUILDINGID) %>%
  arrange(BUILDINGID) %>%
  ggplot(aes(x = WAPvalue, y = BUILDINGID)) +
  labs(x='WAP value', y='Building', title='Distribution of Signal Intensity', subtitle = "WAP 248") +
  geom_density_ridges(aes(fill = BUILDINGID)) +
  scale_fill_manual(name="Building ID", values = pal3) +
  theme(legend.position = "bottom")



WAP362 <-long.table.train %>%
  select(WAPid, WAPvalue, BUILDINGID) %>%
  filter(WAPid=="WAP362") %>%
  group_by(BUILDINGID) %>%
  arrange(BUILDINGID) %>%
  ggplot(aes(x = WAPvalue, y = BUILDINGID)) +
  labs(x='WAP value', y='Building', title='Distribution of Signal Intensity', subtitle = "WAP 362") +
  geom_density_ridges(aes(fill = BUILDINGID)) +
  scale_fill_manual(name="Building ID", values = pal3) +
  theme(legend.position = "bottom")


WAP413 <-long.table.train %>%
  select(WAPid, WAPvalue, BUILDINGID) %>%
  filter(WAPid=="WAP413") %>%
  group_by(BUILDINGID) %>%
  arrange(BUILDINGID) %>%
  ggplot(aes(x = WAPvalue, y=BUILDINGID)) +
  labs(x='WAP value', y='Building', title='Distribution of Signal Intensity', subtitle = "WAP 413") +
  geom_density_ridges(aes(fill = BUILDINGID)) +
  scale_fill_manual(name="Building ID", values = pal3) +
  theme(legend.position = "bottom")


## More accurate graphs (as they take into consideration the sample size)
WAP248 <-long.table.train %>%
  select(WAPid, WAPvalue, BUILDINGID) %>%
  filter(WAPid=="WAP248") %>%
  group_by(BUILDINGID) %>%
  arrange(BUILDINGID) %>%
  ggplot(aes(x = WAPvalue, y = ..count..)) +
  labs(x='WAP value', y='Density', title='Distribution of Signal Intensity', subtitle = "WAP 248") +
  geom_density(alpha =0.5, aes(fill = BUILDINGID)) +
  scale_fill_manual(name="Building ID", values = pal3) +
  theme(legend.position = "bottom")


WAP362 <-long.table.train %>%
  select(WAPid, WAPvalue, BUILDINGID) %>%
  filter(WAPid=="WAP362") %>%
  group_by(BUILDINGID) %>%
  arrange(BUILDINGID) %>%
  ggplot(aes(x = WAPvalue, y = ..count..)) +
  labs(x='WAP value', y='Density', title='Distribution of Signal Intensity', subtitle = "WAP 362") +
  geom_density(alpha =0.5, aes(fill = BUILDINGID)) +
  scale_fill_manual(name="Building ID", values = pal3) +
  theme(legend.position = "bottom")

## too many rows to delete 
filter362 <-wide.table.train %>%
  select(WAP362, BUILDINGID) %>%
  group_by(BUILDINGID) %>%
  filter(BUILDINGID =="TI")
nrow(filter362) 

nrow(wide.table.train)  

WAP413 <-long.table.train %>%
  select(WAPid, WAPvalue, BUILDINGID) %>%
  filter(WAPid=="WAP413") %>%
  group_by(BUILDINGID) %>%
  arrange(BUILDINGID) %>%
  ggplot(aes(x = WAPvalue, y = ..count..)) +
  labs(x='WAP value', y='Density', title='Distribution of Signal Intensity', subtitle = "WAP 413") +
  geom_density(alpha =0.5, aes(fill = BUILDINGID)) +
  scale_fill_manual(name="Building ID", values = pal3) +
  theme(legend.position = "bottom")

## too many rows to delete 
filter413 <-wide.table.train %>%
  select(WAP413, BUILDINGID) %>%
  group_by(BUILDINGID) %>%
  filter(BUILDINGID=="TI")
nrow(filter413)  

post.var.waps.train <- grep("WAP", names(wide.table.train), value = TRUE) # grep 465 WAPs remaining after applying zeroVar

dim(wide.table.train)

wide.table.train <- wide.table.train %>% filter_at(vars(contains("WAP")), any_vars(. > -90)) # filter WAPs with values >-90 to take them for modelling
dim(wide.table.train) # 18607 x 474
wide.table.train$WAP248 <- NULL
dim(wide.table.train) # 18607 x 473
wide.table.train$WAP427 <- NULL
dim(wide.table.train) # 18607 x 472

post.var.waps.train <- grep("WAP", names(wide.table.train), value = TRUE) # grep 464 WAPs remaining after removing WAP 248 and WAP 427



######################### ATTRIBUTE SELECTION & ENGINEERING ##########################

# in order to reduce dimensions but keep important information given by the waps we will run a PCA to create a new data set containing components with the highest variance

# Preprocess to standarize the WAPs attributes
compress <- preProcess(wide.table.train[ ,post.var.waps.train], 
                       method = c("center", "scale", "pca"), 
                       thresh = 0.80) 
# PCA needed 141 components to capture 80 percent of the variance


# PCA Train
pca.train <- prcomp(wide.table.train[,post.var.waps.train], 
                    center= TRUE, scale.= TRUE, rank. = 141) 

## PCA Visualization 
## Matrix "x" has principal component score vectors in a 18607 X 141 dimension
dim(pca.train$x)

## Compute standard deviation of each principal component
PCs.sd <- pca.train$sdev

## Compute variance
PCs.var <- PCs.sd^2 


## Proportion of variance explained by each component
var.explained <- PCs.var/sum(PCs.var) 
plot(var.explained, 
     xlab = "Principal Component", 
     ylab = "Proportion of Variance Explained", 
     type = "b")                                         

## Cumulative proportion of variance explained plot  
plot(cumsum(var.explained), 
     xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b") 
# graph shows that taking 141 components results in variance close to ~ 80%


# Preparing the TRAINING set for PCA
training_PCA <- predict(compress, wide.table.train[,post.var.waps.train])
vars_not_waps_tr <- wide.table.train[ ,c("BUILDINGID","FLOOR","LONGITUDE", "LATITUDE")]
training_PCA <- cbind(training_PCA, vars_not_waps_tr)
is.data.frame(training_PCA)
colnames(training_PCA)


# Save an object to a file
saveRDS(training_PCA, file = "training_PCA.rds")
getwd()
ls()


# https://rpubs.com/SaraMarin/AWS