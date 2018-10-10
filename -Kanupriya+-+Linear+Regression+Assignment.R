#----------------------------- Load libraries ----------------------------
library(tidyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(car)
library(MASS)

# ---------------------------- Load the data  ----------------------------
car_data <- read.csv("CarPrice_Assignment.csv")

#  ---------------------------- Understand the data  ----------------------------
# Check structure of data
str(car_data)
head(car_data)
View(car_data)

#Check if there are any duplicate rows 
nrow(car_data)
nrow(unique(car_data[,2:26]))
#No Duplicate Rows Present

#Check how many NA values exist
length(which(is.na(car_data)))
#No NA values

#Remove car_ID since it's unique to each row and doesn't help with the analysis or modeling
length(unique(car_data$car_ID))
car_data <- car_data[2:26]

#---------------------------- Data Preparation -------------------------------

# Separate "CarName" into brand name and model name
car_data <- separate(car_data, CarName, into=c("CarName", "Model"), sep = " ")
#Drop "Model" as it's not required
car_data <- car_data[,-3]

#View all distinct car brands, we see lots of data issues
View(unique(car_data$CarName))
#first let's turn them all to  lower case
car_data$CarName <- tolower(car_data$CarName)
#Correct maxda to mazda
car_data[which(car_data$CarName=="maxda"), "CarName"] <- "mazda"
#Correct porcshce to porsche
car_data[which(car_data$CarName=="porcshce"), "CarName"] <- "porsche"
#Correct toyouta to toyota
car_data[which(car_data$CarName=="toyouta"), "CarName"] <- "toyota"
#Correct vokswagen and vw to volkswagen
car_data[which(car_data$CarName=="vokswagen" | car_data$CarName=="vw"), "CarName"] <- "volkswagen"
View(unique(car_data$CarName)) #Car Names corrected

#------------------------- Check for outliers ------------------------- 

#Check for outliers in wheelbase
quantile(car_data$wheelbase, seq(0,1,0.1))
ggplot(car_data, aes(x="", y=wheelbase)) + geom_boxplot()
boxplot.stats(car_data$wheelbase)$out
#Values higher than 115 are the outliers so let's set them to 115
car_data[which(car_data$wheelbase>115), "wheelbase"] <- 115

#Check for outliers in carlength
quantile(car_data$carlength, seq(0,1,0.1))
ggplot(car_data, aes(x="", y=carlength)) + geom_boxplot()
boxplot.stats(car_data$carlength)$out
#No outliers that are very far from other data

#Check for outliers in carwidth
quantile(car_data$carwidth, seq(0,1,0.1))
ggplot(car_data, aes(x="", y=carwidth)) + geom_boxplot()
boxplot.stats(car_data$carwidth)$out
#No outliers that are very far from other data

#Check for outliers in carheight
quantile(car_data$carheight, seq(0,1,0.1))
ggplot(car_data, aes(x="", y=carheight)) + geom_boxplot()
boxplot.stats(car_data$carheight)$out
#No outliers 

#Check for outliers in curbweight
quantile(car_data$curbweight, seq(0,1,0.1))
ggplot(car_data, aes(x="", y=curbweight)) + geom_boxplot()
boxplot.stats(car_data$curbweight)$out
#No outliers 

#Check for outliers in enginesize
quantile(car_data$enginesize, seq(0,1,0.1))
ggplot(car_data, aes(x="", y=enginesize)) + geom_boxplot()
boxplot.stats(car_data$enginesize)$out
#Values higher than 200 seem to be outliers so let's set them to 200
car_data[which(car_data$enginesize>200), "enginesize"] <- 200

#Check for outliers in boreratio
quantile(car_data$boreratio, seq(0,1,0.1))
ggplot(car_data, aes(x="", y=boreratio)) + geom_boxplot()
boxplot.stats(car_data$boreratio)$out
#No outliers 

#Check for outliers in stroke
quantile(car_data$stroke, seq(0,1,0.1))
ggplot(car_data, aes(x="", y=stroke)) + geom_boxplot()
boxplot.stats(car_data$stroke)$out
#Values higher than 3.54 seem to be outliers so let's set them to 3.54
car_data[which(car_data$stroke>3.54), "stroke"] <- 3.54
#Values less than 2.7 seem to be outliers so let's set them to 2.7
car_data[which(car_data$stroke<2.7), "stroke"] <- 2.7

#Check for outliers in compressionratio
quantile(car_data$compressionratio, seq(0,1,0.1))
ggplot(car_data, aes(x="", y=compressionratio)) + geom_boxplot()
boxplot.stats(car_data$compressionratio)$out
#Values higher than 10.94 seem to be outliers so let's set them to 10.94
car_data[which(car_data$compressionratio>10.94), "compressionratio"] <- 10.94

#Check for outliers in horsepower
quantile(car_data$horsepower, seq(0,1,0.1))
ggplot(car_data, aes(x="", y=horsepower)) + geom_boxplot()
boxplot.stats(car_data$horsepower)$out
#Values higher than 200 seem to be outliers so let's set them to 200
car_data[which(car_data$horsepower>200), "horsepower"] <- 200

#Check for outliers in peakrpm
quantile(car_data$peakrpm, seq(0,1,0.1))
ggplot(car_data, aes(x="", y=peakrpm)) + geom_boxplot()
boxplot.stats(car_data$peakrpm)$out
#Values higher than 6000 seem to be outliers so let's set them to 6000
car_data[which(car_data$peakrpm>6000), "peakrpm"] <- 6000

#Check for outliers in citympg
quantile(car_data$citympg, seq(0,1,0.1))
ggplot(car_data, aes(x="", y=citympg)) + geom_boxplot()
boxplot.stats(car_data$citympg)$out
#No outliers that are very far from other data

#Check for outliers in highwaympg
quantile(car_data$highwaympg, seq(0,1,0.1))
ggplot(car_data, aes(x="", y=highwaympg)) + geom_boxplot()
boxplot.stats(car_data$highwaympg)$out
#No outliers that are very far from other data

# ----------------------- Convert categorical data to numeric -----------------------

# "fueltype"
levels(car_data$fueltype)<-c(1,0)
car_data$fueltype<- as.numeric(levels(car_data$fueltype))[car_data$fueltype]

# "aspiration"
levels(car_data$aspiration)<-c(1,0)
car_data$aspiration<- as.numeric(levels(car_data$aspiration))[car_data$aspiration]

# "doornumber"
levels(car_data$doornumber)<-c(1,0)
car_data$doornumber<- as.numeric(levels(car_data$doornumber))[car_data$doornumber]

# "enginelocation"
levels(car_data$enginelocation)<-c(1,0)
car_data$enginelocation<- as.numeric(levels(car_data$enginelocation))[car_data$enginelocation]

#--------------------------- Dummy Variable Creation --------------------------- 

#CarName
summary(factor(car_data$CarName))
#Converting "CarName" into dummies . 
dummy_1 <- data.frame(model.matrix( ~CarName, data = car_data))
#check the dummy_1 data frame.
View(dummy_1)
#This column should be removed from the newly created dummy_1 dataframe containing the dummy values for the variable "carName". 
dummy_1 <- dummy_1[,-1]
# Combine the dummy variables to the main data set, after removing the original categorical "carName" column
car_data_1 <- cbind(car_data[,-2], dummy_1)

#Check structure of car_data_1
str(car_data_1)

# "carbody" 
summary(factor(car_data_1$carbody))

#Converting "carbody" into dummies . 
dummy_1 <- data.frame(model.matrix( ~carbody, data = car_data_1))
#check the dummy_1 data frame.
View(dummy_1)
#This column should be removed from the newly created dummy_1 dataframe containing the dummy values for the variable "carbody". 
dummy_1 <- dummy_1[,-1]
# Combine the dummy variables to the main data set, after removing the original categorical "carbody" column
car_data_2 <- cbind(car_data_1[,-5], dummy_1)

#Check structure of car_data_2
str(car_data_2)

# "drivewheel" 
summary(factor(car_data_2$drivewheel))

#Converting "drivewheel" into dummies . 
dummy_1 <- data.frame(model.matrix( ~drivewheel, data = car_data_2))
#check the dummy_1 data frame.
View(dummy_1)
#This column should be removed from the newly created dummy_1 dataframe containing the dummy values for the variable "drivewheel". 
dummy_1 <- dummy_1[,-1]
# Combine the dummy variables to the main data set, after removing the original categorical "drivewheel" column
car_data_3 <- cbind(car_data_2[,-5], dummy_1)

#Check structure of car_data_3
str(car_data_3)

# "enginetype" 
summary(factor(car_data_3$enginetype))

#Converting "enginetype" into dummies . 
dummy_1 <- data.frame(model.matrix( ~enginetype, data = car_data_3))
#check the dummy_1 data frame.
View(dummy_1)
#This column should be removed from the newly created dummy_1 dataframe containing the dummy values for the variable "enginetype". 
dummy_1 <- dummy_1[,-1]
# Combine the dummy variables to the main data set, after removing the original categorical "enginetype" column
car_data_4 <- cbind(car_data_3[,-11], dummy_1)

#Check structure of car_data_4
str(car_data_4)

# "cylindernumber" 
summary(factor(car_data_4$cylindernumber))

#Converting "cylindernumber" into dummies . 
dummy_1 <- data.frame(model.matrix( ~cylindernumber, data = car_data_4))
#check the dummy_1 data frame.
View(dummy_1)
#This column should be removed from the newly created dummy_1 dataframe containing the dummy values for the variable "cylindernumber". 
dummy_1 <- dummy_1[,-1]
# Combine the dummy variables to the main data set, after removing the original categorical "cylindernumber" column
car_data_5 <- cbind(car_data_4[,-11], dummy_1)

#Check structure of car_data_5
str(car_data_5)

# "fuelsystem" 
summary(factor(car_data_5$fuelsystem))

#Converting "fuelsystem" into dummies . 
dummy_1 <- data.frame(model.matrix( ~fuelsystem, data = car_data_5))
#check the dummy_1 data frame.
View(dummy_1)
#This column should be removed from the newly created dummy_1 dataframe containing the dummy values for the variable "fuelsystem". 
dummy_1 <- dummy_1[,-1]
# Combine the dummy variables to the main data set, after removing the original categorical "fuelsystem" column
car_data_6 <- cbind(car_data_5[,-12], dummy_1)

#Check structure of car_data_6
str(car_data_6)

#All categorical variables have been changed to numeric

#---------------------- Create Derived Metrics ----------------------

#Horsepower to weight ratio is considered an important ratio for cars
car_data_6$power_weight_ratio <- car_data_6$horsepower / car_data_6$curbweight

#---------------------- Divide into training and test data set ----------------------
#set the seed to 100 
set.seed(100)

# randomly generate row indices for train dataset
trainindices= sample(1:nrow(car_data_6), 0.7*nrow(car_data_6))
# generate the train data set
train = car_data_6[trainindices,]

#Similarly store the rest of the observations into an object "test".
test = car_data_6[-trainindices,]

#---------------------- Model Building ----------------------
# Build model 1 containing all variables
model_1 <-lm(price~.,data=train)
summary(model_1) #Adjusted R-squared:   0.9652

#Pass first model to stepAIC function
step <- stepAIC(model_1, direction="both")
step

# Let's execute the model given by stepAIC 
model_2 <- lm(formula = price ~ aspiration + enginelocation + wheelbase + 
                carwidth + curbweight + enginesize + horsepower + peakrpm + 
                CarNamebmw + CarNamebuick + CarNamechevrolet + CarNamedodge + 
                CarNamehonda + CarNameisuzu + CarNamejaguar + CarNamemazda + 
                CarNamemercury + CarNamemitsubishi + CarNamenissan + CarNamepeugeot + 
                CarNameplymouth + CarNamerenault + CarNamesaab + CarNamesubaru + 
                CarNametoyota + CarNamevolkswagen + CarNamevolvo + carbodyhardtop + 
                carbodyhatchback + carbodysedan + carbodywagon + drivewheelrwd + 
                enginetypeohc + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + fuelsystem2bbl + fuelsystemmpfi + fuelsystemspdi + 
                power_weight_ratio, data = train)
# Let us look at the summary of the model
summary(model_2) #Adjusted r-squared 0.97

#Variable "peakrpm" has a p-value > 0.20, so let's remove it
model_3 <- lm(formula = price ~ aspiration + enginelocation + wheelbase + 
                carwidth + curbweight + enginesize + horsepower + 
                CarNamebmw + CarNamebuick + CarNamechevrolet + CarNamedodge + 
                CarNamehonda + CarNameisuzu + CarNamejaguar + CarNamemazda + 
                CarNamemercury + CarNamemitsubishi + CarNamenissan + CarNamepeugeot + 
                CarNameplymouth + CarNamerenault + CarNamesaab + CarNamesubaru + 
                CarNametoyota + CarNamevolkswagen + CarNamevolvo + carbodyhardtop + 
                carbodyhatchback + carbodysedan + carbodywagon + drivewheelrwd + 
                enginetypeohc + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + fuelsystem2bbl + fuelsystemmpfi + fuelsystemspdi + 
                power_weight_ratio, data = train)
# Let us look at the summary of the model
summary(model_3) #Adjusted r-squared 0.9698

#Variable "curbweight" has a p-value > 0.15, so let's remove it
model_4 <- lm(formula = price ~ aspiration + enginelocation + wheelbase + 
                carwidth + enginesize + horsepower + 
                CarNamebmw + CarNamebuick + CarNamechevrolet + CarNamedodge + 
                CarNamehonda + CarNameisuzu + CarNamejaguar + CarNamemazda + 
                CarNamemercury + CarNamemitsubishi + CarNamenissan + CarNamepeugeot + 
                CarNameplymouth + CarNamerenault + CarNamesaab + CarNamesubaru + 
                CarNametoyota + CarNamevolkswagen + CarNamevolvo + carbodyhardtop + 
                carbodyhatchback + carbodysedan + carbodywagon + drivewheelrwd + 
                enginetypeohc + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + fuelsystem2bbl + fuelsystemmpfi + fuelsystemspdi + 
                power_weight_ratio, data = train)
# Let us look at the summary of the model
summary(model_4) #Adjusted r-squared 0.9695

#Variable "enginesize" has a p-value > 0.32, so let's remove it
model_5 <- lm(formula = price ~ aspiration + enginelocation + wheelbase + 
                carwidth + horsepower + 
                CarNamebmw + CarNamebuick + CarNamechevrolet + CarNamedodge + 
                CarNamehonda + CarNameisuzu + CarNamejaguar + CarNamemazda + 
                CarNamemercury + CarNamemitsubishi + CarNamenissan + CarNamepeugeot + 
                CarNameplymouth + CarNamerenault + CarNamesaab + CarNamesubaru + 
                CarNametoyota + CarNamevolkswagen + CarNamevolvo + carbodyhardtop + 
                carbodyhatchback + carbodysedan + carbodywagon + drivewheelrwd + 
                enginetypeohc + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + fuelsystem2bbl + fuelsystemmpfi + fuelsystemspdi + 
                power_weight_ratio, data = train)
# Let us look at the summary of the model
summary(model_5) #Adjusted r-squared 0.9695

#Variable "wheelbase" has a p-value > 0.11, so let's remove it
model_6 <- lm(formula = price ~ aspiration + enginelocation + 
                carwidth + horsepower + 
                CarNamebmw + CarNamebuick + CarNamechevrolet + CarNamedodge + 
                CarNamehonda + CarNameisuzu + CarNamejaguar + CarNamemazda + 
                CarNamemercury + CarNamemitsubishi + CarNamenissan + CarNamepeugeot + 
                CarNameplymouth + CarNamerenault + CarNamesaab + CarNamesubaru + 
                CarNametoyota + CarNamevolkswagen + CarNamevolvo + carbodyhardtop + 
                carbodyhatchback + carbodysedan + carbodywagon + drivewheelrwd + 
                enginetypeohc + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + fuelsystem2bbl + fuelsystemmpfi + fuelsystemspdi + 
                power_weight_ratio, data = train)
# Let us look at the summary of the model
summary(model_6) #Adjusted r-squared 0.9691

#Variable "CarNamehonda" has a p-value > 0.15, so let's remove it
model_7 <- lm(formula = price ~ aspiration + enginelocation + 
                carwidth + horsepower + 
                CarNamebmw + CarNamebuick + CarNamechevrolet + CarNamedodge + 
                CarNameisuzu + CarNamejaguar + CarNamemazda + 
                CarNamemercury + CarNamemitsubishi + CarNamenissan + CarNamepeugeot + 
                CarNameplymouth + CarNamerenault + CarNamesaab + CarNamesubaru + 
                CarNametoyota + CarNamevolkswagen + CarNamevolvo + carbodyhardtop + 
                carbodyhatchback + carbodysedan + carbodywagon + drivewheelrwd + 
                enginetypeohc + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + fuelsystem2bbl + fuelsystemmpfi + fuelsystemspdi + 
                power_weight_ratio, data = train)
# Let us look at the summary of the model
summary(model_7) #Adjusted r-squared 0.9688

#Variable "CarNameisuzu" has a p-value > 0.41, so let's remove it
model_8 <- lm(formula = price ~ aspiration + enginelocation + 
                carwidth + horsepower + 
                CarNamebmw + CarNamebuick + CarNamechevrolet + CarNamedodge + 
                CarNamejaguar + CarNamemazda + 
                CarNamemercury + CarNamemitsubishi + CarNamenissan + CarNamepeugeot + 
                CarNameplymouth + CarNamerenault + CarNamesaab + CarNamesubaru + 
                CarNametoyota + CarNamevolkswagen + CarNamevolvo + carbodyhardtop + 
                carbodyhatchback + carbodysedan + carbodywagon + drivewheelrwd + 
                enginetypeohc + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + fuelsystem2bbl + fuelsystemmpfi + fuelsystemspdi + 
                power_weight_ratio, data = train)
# Let us look at the summary of the model
summary(model_8) #Adjusted r-squared 0.9689

#Variable "CarNamemercury" has a p-value > 0.30, so let's remove it
model_9 <- lm(formula = price ~ aspiration + enginelocation + 
                carwidth + horsepower + 
                CarNamebmw + CarNamebuick + CarNamechevrolet + CarNamedodge + 
                CarNamejaguar + CarNamemazda + 
                CarNamemitsubishi + CarNamenissan + CarNamepeugeot + 
                CarNameplymouth + CarNamerenault + CarNamesaab + CarNamesubaru + 
                CarNametoyota + CarNamevolkswagen + CarNamevolvo + carbodyhardtop + 
                carbodyhatchback + carbodysedan + carbodywagon + drivewheelrwd + 
                enginetypeohc + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + fuelsystem2bbl + fuelsystemmpfi + fuelsystemspdi + 
                power_weight_ratio, data = train)
# Let us look at the summary of the model
summary(model_9) #Adjusted r-squared 0.9688

#Variable "CarNamesaab" has a p-value > 0.11, so let's remove it
model_10 <- lm(formula = price ~ aspiration + enginelocation + 
                carwidth + horsepower + 
                CarNamebmw + CarNamebuick + CarNamechevrolet + CarNamedodge + 
                CarNamejaguar + CarNamemazda + 
                CarNamemitsubishi + CarNamenissan + CarNamepeugeot + 
                CarNameplymouth + CarNamerenault + CarNamesubaru + 
                CarNametoyota + CarNamevolkswagen + CarNamevolvo + carbodyhardtop + 
                carbodyhatchback + carbodysedan + carbodywagon + drivewheelrwd + 
                enginetypeohc + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + fuelsystem2bbl + fuelsystemmpfi + fuelsystemspdi + 
                power_weight_ratio, data = train)
# Let us look at the summary of the model
summary(model_10) #Adjusted r-squared 0.9684

#Variable "CarNamevolvo" has a p-value > 0.35, so let's remove it
model_11 <- lm(formula = price ~ aspiration + enginelocation + 
                 carwidth + horsepower + 
                 CarNamebmw + CarNamebuick + CarNamechevrolet + CarNamedodge + 
                 CarNamejaguar + CarNamemazda + 
                 CarNamemitsubishi + CarNamenissan + CarNamepeugeot + 
                 CarNameplymouth + CarNamerenault + CarNamesubaru + 
                 CarNametoyota + CarNamevolkswagen + carbodyhardtop + 
                 carbodyhatchback + carbodysedan + carbodywagon + drivewheelrwd + 
                 enginetypeohc + cylindernumberfive + cylindernumberfour + 
                 cylindernumbersix + fuelsystem2bbl + fuelsystemmpfi + fuelsystemspdi + 
                 power_weight_ratio, data = train)
# Let us look at the summary of the model
summary(model_11) #Adjusted r-squared 0.9685

#Variable "carbodyhardtop" has a p-value > 0.17, so let's remove it
model_12 <- lm(formula = price ~ aspiration + enginelocation + 
                 carwidth + horsepower + 
                 CarNamebmw + CarNamebuick + CarNamechevrolet + CarNamedodge + 
                 CarNamejaguar + CarNamemazda + 
                 CarNamemitsubishi + CarNamenissan + CarNamepeugeot + 
                 CarNameplymouth + CarNamerenault + CarNamesubaru + 
                 CarNametoyota + CarNamevolkswagen + 
                 carbodyhatchback + carbodysedan + carbodywagon + drivewheelrwd + 
                 enginetypeohc + cylindernumberfive + cylindernumberfour + 
                 cylindernumbersix + fuelsystem2bbl + fuelsystemmpfi + fuelsystemspdi + 
                 power_weight_ratio, data = train)
# Let us look at the summary of the model
summary(model_12) #Adjusted r-squared 0.9682

#Now all variables have some significance so let's check their multicollinearity
vif(model_12)

#"aspiration" has a vif > 3 and p-value > 0.01 so let's remove it and observe
model_13 <- lm(formula = price ~ enginelocation + carwidth + horsepower + 
                 CarNamebmw + CarNamebuick + CarNamechevrolet + CarNamedodge + 
                 CarNamejaguar + CarNamemazda + 
                 CarNamemitsubishi + CarNamenissan + CarNamepeugeot + 
                 CarNameplymouth + CarNamerenault + CarNamesubaru + 
                 CarNametoyota + CarNamevolkswagen + 
                 carbodyhatchback + carbodysedan + carbodywagon + drivewheelrwd + 
                 enginetypeohc + cylindernumberfive + cylindernumberfour + 
                 cylindernumbersix + fuelsystem2bbl + fuelsystemmpfi + fuelsystemspdi + 
                 power_weight_ratio, data = train)
# Let us look at the summary of the model
summary(model_13) #Adjusted r-squared 0.9666

#Variable "CarNamechevrolet" has a p-value > 0.05, so let's remove it
model_14 <- lm(formula = price ~ enginelocation + carwidth + horsepower + 
                 CarNamebmw + CarNamebuick + CarNamedodge + 
                 CarNamejaguar + CarNamemazda + 
                 CarNamemitsubishi + CarNamenissan + CarNamepeugeot + 
                 CarNameplymouth + CarNamerenault + CarNamesubaru + 
                 CarNametoyota + CarNamevolkswagen + 
                 carbodyhatchback + carbodysedan + carbodywagon + drivewheelrwd + 
                 enginetypeohc + cylindernumberfive + cylindernumberfour + 
                 cylindernumbersix + fuelsystem2bbl + fuelsystemmpfi + fuelsystemspdi + 
                 power_weight_ratio, data = train)
# Let us look at the summary of the model
summary(model_14) #Adjusted r-squared 0.9659

#Variable "CarNamedodge" has a p-value > 0.06, so let's remove it
model_15 <- lm(formula = price ~ enginelocation + carwidth + horsepower + 
                 CarNamebmw + CarNamebuick + CarNamejaguar + CarNamemazda + 
                 CarNamemitsubishi + CarNamenissan + CarNamepeugeot + 
                 CarNameplymouth + CarNamerenault + CarNamesubaru + 
                 CarNametoyota + CarNamevolkswagen + 
                 carbodyhatchback + carbodysedan + carbodywagon + drivewheelrwd + 
                 enginetypeohc + cylindernumberfive + cylindernumberfour + 
                 cylindernumbersix + fuelsystem2bbl + fuelsystemmpfi + fuelsystemspdi + 
                 power_weight_ratio, data = train)
# Let us look at the summary of the model
summary(model_15) #Adjusted r-squared 0.9651

#Variable "carbodysedan" has a p-value > 0.07, so let's remove it
model_16 <- lm(formula = price ~ enginelocation + carwidth + horsepower + 
                 CarNamebmw + CarNamebuick + CarNamejaguar + CarNamemazda + 
                 CarNamemitsubishi + CarNamenissan + CarNamepeugeot + 
                 CarNameplymouth + CarNamerenault + CarNamesubaru + 
                 CarNametoyota + CarNamevolkswagen + 
                 carbodyhatchback + carbodywagon + drivewheelrwd + 
                 enginetypeohc + cylindernumberfive + cylindernumberfour + 
                 cylindernumbersix + fuelsystem2bbl + fuelsystemmpfi + fuelsystemspdi + 
                 power_weight_ratio, data = train)
# Let us look at the summary of the model
summary(model_16) #Adjusted r-squared 0.9644

#Variable "carbodyhatchback" has a p-value > 0.17, so let's remove it
model_17 <- lm(formula = price ~ enginelocation + carwidth + horsepower + 
                 CarNamebmw + CarNamebuick + CarNamejaguar + CarNamemazda + 
                 CarNamemitsubishi + CarNamenissan + CarNamepeugeot + 
                 CarNameplymouth + CarNamerenault + CarNamesubaru + 
                 CarNametoyota + CarNamevolkswagen + 
                 carbodywagon + drivewheelrwd + 
                 enginetypeohc + cylindernumberfive + cylindernumberfour + 
                 cylindernumbersix + fuelsystem2bbl + fuelsystemmpfi + fuelsystemspdi + 
                 power_weight_ratio, data = train)
# Let us look at the summary of the model
summary(model_17) #Adjusted r-squared 0.9642

#Variable "CarNamenissan" has a p-value > 0.07, so let's remove it
model_18 <- lm(formula = price ~ enginelocation + carwidth + horsepower + 
                 CarNamebmw + CarNamebuick + CarNamejaguar + CarNamemazda + 
                 CarNamemitsubishi + CarNamepeugeot + 
                 CarNameplymouth + CarNamerenault + CarNamesubaru + 
                 CarNametoyota + CarNamevolkswagen + 
                 carbodywagon + drivewheelrwd + 
                 enginetypeohc + cylindernumberfive + cylindernumberfour + 
                 cylindernumbersix + fuelsystem2bbl + fuelsystemmpfi + fuelsystemspdi + 
                 power_weight_ratio, data = train)
# Let us look at the summary of the model
summary(model_18) #Adjusted r-squared 0.9635

#Variable "fuelsystem2bbl" has a p-value > 0.21, so let's remove it
model_19 <- lm(formula = price ~ enginelocation + carwidth + horsepower + 
                 CarNamebmw + CarNamebuick + CarNamejaguar + CarNamemazda + 
                 CarNamemitsubishi + CarNamepeugeot + 
                 CarNameplymouth + CarNamerenault + CarNamesubaru + 
                 CarNametoyota + CarNamevolkswagen + 
                 carbodywagon + drivewheelrwd + 
                 enginetypeohc + cylindernumberfive + cylindernumberfour + 
                 cylindernumbersix + fuelsystemmpfi + fuelsystemspdi + 
                 power_weight_ratio, data = train)
# Let us look at the summary of the model
summary(model_19) #Adjusted r-squared 0.9633

#Variable "fuelsystemspdi" has a p-value > 0.10, so let's remove it
model_20 <- lm(formula = price ~ enginelocation + carwidth + horsepower + 
                 CarNamebmw + CarNamebuick + CarNamejaguar + CarNamemazda + 
                 CarNamemitsubishi + CarNamepeugeot + 
                 CarNameplymouth + CarNamerenault + CarNamesubaru + 
                 CarNametoyota + CarNamevolkswagen + 
                 carbodywagon + drivewheelrwd + 
                 enginetypeohc + cylindernumberfive + cylindernumberfour + 
                 cylindernumbersix + fuelsystemmpfi + 
                 power_weight_ratio, data = train)
# Let us look at the summary of the model
summary(model_20) #Adjusted r-squared 0.9628

#Variable "CarNameplymouth" has a p-value > 0.24, so let's remove it
model_21 <- lm(formula = price ~ enginelocation + carwidth + horsepower + 
                 CarNamebmw + CarNamebuick + CarNamejaguar + CarNamemazda + 
                 CarNamemitsubishi + CarNamepeugeot + CarNamerenault + CarNamesubaru + 
                 CarNametoyota + CarNamevolkswagen + carbodywagon + drivewheelrwd + 
                 enginetypeohc + cylindernumberfive + cylindernumberfour + 
                 cylindernumbersix + fuelsystemmpfi + 
                 power_weight_ratio, data = train)
# Let us look at the summary of the model
summary(model_21) #Adjusted r-squared 0.9627

#Now all variables have some significance so let's check their multicollinearity
vif(model_21)

#Variable "drivewheelrwd" has a vif>2 and p-value>0.02 so let's remove it and observe
model_22 <- lm(formula = price ~ enginelocation + carwidth + horsepower + 
                 CarNamebmw + CarNamebuick + CarNamejaguar + CarNamemazda + 
                 CarNamemitsubishi + CarNamepeugeot + CarNamerenault + CarNamesubaru + 
                 CarNametoyota + CarNamevolkswagen + carbodywagon + 
                 enginetypeohc + cylindernumberfive + cylindernumberfour + 
                 cylindernumbersix + fuelsystemmpfi + 
                 power_weight_ratio, data = train)
# Let us look at the summary of the model
summary(model_22) #Adjusted r-squared 0.9614

#Now all variables have some significance so let's check their multicollinearity
vif(model_22)

#Variable "fuelsystemmpfi" has a vif>2 and p-value>0.02 so let's remove it and observe
model_23 <- lm(formula = price ~ enginelocation + carwidth + horsepower + 
                 CarNamebmw + CarNamebuick + CarNamejaguar + CarNamemazda + 
                 CarNamemitsubishi + CarNamepeugeot + CarNamerenault + CarNamesubaru + 
                 CarNametoyota + CarNamevolkswagen + carbodywagon + 
                 enginetypeohc + cylindernumberfive + cylindernumberfour + 
                 cylindernumbersix + power_weight_ratio, data = train)
# Let us look at the summary of the model
summary(model_23) #Adjusted r-squared 0.9602

#Variable "CarNamevolkswagen" has a p-value > 0.09, so let's remove it
model_24 <- lm(formula = price ~ enginelocation + carwidth + horsepower + 
                 CarNamebmw + CarNamebuick + CarNamejaguar + CarNamemazda + 
                 CarNamemitsubishi + CarNamepeugeot + CarNamerenault + CarNamesubaru + 
                 CarNametoyota + carbodywagon + 
                 enginetypeohc + cylindernumberfive + cylindernumberfour + 
                 cylindernumbersix + power_weight_ratio, data = train)
# Let us look at the summary of the model
summary(model_24) #Adjusted r-squared 0.9596

#Now all variables have some significance so let's check their multicollinearity
vif(model_24)

#Variable "cylindernumberfour" has a vif>9 and p-value>0.04 so let's remove it and observe
model_25 <- lm(formula = price ~ enginelocation + carwidth + horsepower + 
                 CarNamebmw + CarNamebuick + CarNamejaguar + CarNamemazda + 
                 CarNamemitsubishi + CarNamepeugeot + CarNamerenault + CarNamesubaru + 
                 CarNametoyota + carbodywagon + enginetypeohc + cylindernumberfive + 
                 cylindernumbersix + power_weight_ratio, data = train)
# Let us look at the summary of the model
summary(model_25) #Adjusted r-squared 0.9586

#Variable "cylindernumberfive" has a p-value>0.43 so let's remove it and observe
model_26 <- lm(formula = price ~ enginelocation + carwidth + horsepower + 
                 CarNamebmw + CarNamebuick + CarNamejaguar + CarNamemazda + 
                 CarNamemitsubishi + CarNamepeugeot + CarNamerenault + CarNamesubaru + 
                 CarNametoyota + carbodywagon + enginetypeohc + 
                 cylindernumbersix + power_weight_ratio, data = train)
# Let us look at the summary of the model
summary(model_26) #Adjusted r-squared 0.9587

#Now all variables have some significance so let's check their multicollinearity
vif(model_26)

#Some variables that have a high vif also have a high significance
#So let's remove variables with least significance first and observe

#Let's remove "CarNamerenault" which has a p-value > 0.037
model_27 <- lm(formula = price ~ enginelocation + carwidth + horsepower + 
                 CarNamebmw + CarNamebuick + CarNamejaguar + CarNamemazda + 
                 CarNamemitsubishi + CarNamepeugeot + CarNamesubaru + 
                 CarNametoyota + carbodywagon + enginetypeohc + 
                 cylindernumbersix + power_weight_ratio, data = train)
# Let us look at the summary of the model
summary(model_27) #Adjusted r-squared 0.9576

#Let's remove "CarNamemazda" which has a p-value > 0.020
model_28 <- lm(formula = price ~ enginelocation + carwidth + horsepower + 
                 CarNamebmw + CarNamebuick + CarNamejaguar + 
                 CarNamemitsubishi + CarNamepeugeot + CarNamesubaru + 
                 CarNametoyota + carbodywagon + enginetypeohc + 
                 cylindernumbersix + power_weight_ratio, data = train)
# Let us look at the summary of the model
summary(model_28) #Adjusted r-squared 0.9561

#Let's remove "carbodywagon" which has a p-value > 0.028
model_29 <- lm(formula = price ~ enginelocation + carwidth + horsepower + 
                 CarNamebmw + CarNamebuick + CarNamejaguar + 
                 CarNamemitsubishi + CarNamepeugeot + CarNamesubaru + 
                 CarNametoyota + enginetypeohc + 
                 cylindernumbersix + power_weight_ratio, data = train)
# Let us look at the summary of the model
summary(model_29) #Adjusted r-squared 0.9547

#Let's remove "CarNamemitsubishi" which has a p-value > 0.036
model_30 <- lm(formula = price ~ enginelocation + carwidth + horsepower + 
                 CarNamebmw + CarNamebuick + CarNamejaguar + 
                 CarNamepeugeot + CarNamesubaru + 
                 CarNametoyota + enginetypeohc + 
                 cylindernumbersix + power_weight_ratio, data = train)
# Let us look at the summary of the model
summary(model_30) #Adjusted r-squared 0.9535

#Now all variables have significance so let's look at multicollinearity
vif(model_30)

#Variable "power_weight_ratio" has a very high vif (>16) ,so let's see if it's correlated to another variable.
#Since it's derived from horsepower and weight let's check correlation with horsepower
cor(car_data_6$horsepower, car_data_6$power_weight_ratio) #0.85

#They're highly correlated so let's try to remove "power_weight_ratio" and observe
model_31 <- lm(formula = price ~ enginelocation + carwidth + horsepower + 
                 CarNamebmw + CarNamebuick + CarNamejaguar + 
                 CarNamepeugeot + CarNamesubaru + 
                 CarNametoyota + enginetypeohc + 
                 cylindernumbersix, data = train)
# Let us look at the summary of the model
summary(model_31) #Adjusted r-squared 0.9403

#Adjusted r-squared has dropped by 0.013 points, but it's still pretty good so let's go ahead

#Let's remove "CarNametoyota" as its p-value > 0.41
model_32 <- lm(formula = price ~ enginelocation + carwidth + horsepower + 
                 CarNamebmw + CarNamebuick + CarNamejaguar + 
                 CarNamepeugeot + CarNamesubaru + enginetypeohc + 
                 cylindernumbersix, data = train)
# Let us look at the summary of the model
summary(model_32) #Adjusted r-squared 0.9405

#Now since all variables have significance let's look at multicollinearity
vif(model_32)

#"cylindernumbersix" has a vif > 2.7 and p-value > 0.02 so let's remove it and observe
model_33 <- lm(formula = price ~ enginelocation + carwidth + horsepower + 
                 CarNamebmw + CarNamebuick + CarNamejaguar + 
                 CarNamepeugeot + CarNamesubaru + enginetypeohc, data = train)
# Let us look at the summary of the model
summary(model_33) #Adjusted r-squared 0.9385

#"CarNamepeugeot" has a p-value > 0.07 so let's remove it and observe
model_34 <- lm(formula = price ~ enginelocation + carwidth + horsepower + 
                 CarNamebmw + CarNamebuick + CarNamejaguar + 
                 CarNamesubaru + enginetypeohc, data = train)
# Let us look at the summary of the model
summary(model_34) #Adjusted r-squared 0.9375

#Now since all variables have significance let's look at multicollinearity
vif(model_34)

#Since all variables with vif>2 are highly significant, let's remove the one with least significance
#Let's remove "CarNamesubaru" which has a p-value > 0.007
model_35 <- lm(formula = price ~ enginelocation + carwidth + horsepower + 
                 CarNamebmw + CarNamebuick + CarNamejaguar + 
                 enginetypeohc, data = train)
# Let us look at the summary of the model
summary(model_35) #Adjusted r-squared 0.9346

#Let's remove "enginetypeohc" which has a p-value > 0.01
model_36 <- lm(formula = price ~ enginelocation + carwidth + horsepower + 
                 CarNamebmw + CarNamebuick + CarNamejaguar, data = train)
# Let us look at the summary of the model
summary(model_36) #Adjusted r-squared 0.9322

#Now since all variables are significant, let's look at multicollinearity
vif(model_36)

#"carwidth" has the highest multicollinearity so let's remove it and observe
model_37 <- lm(formula = price ~ enginelocation + horsepower + 
                 CarNamebmw + CarNamebuick + CarNamejaguar, data = train)
# Let us look at the summary of the model
summary(model_37) #Adjusted r-squared 8854

#Adjusted r-square drops by > 0.04 points, so let's go back to previous model (model_36)
summary(model_36) #Adjusted r-squared 0.9322
vif(model_36)

#"horsepower" has the second highest multicollinearity so let's remove it and observe
model_38 <- lm(formula = price ~ enginelocation + carwidth + 
                 CarNamebmw + CarNamebuick + CarNamejaguar, data = train)
# Let us look at the summary of the model
summary(model_38) #Adjusted r-squared 0.8853

#Adjusted r-square drops by > 0.04 points, so let's go back to previous model (model_36)
summary(model_36) #Adjusted r-squared 0.9322
vif(model_36)

#------------------------------- Model Assessment ---------------------------
#Now all variables are significant and all vif less than or almost equal to 2, 
#so let's evaluate the test data-set on this model (model_36)
Predict_1 <- predict(model_36,test[,-19])
test$test_price <- Predict_1

# Now, we need to test the r square between actual and predicted sales. 
r <- cor(test$price,test$test_price) #0.933
rsquared <- cor(test$price,test$test_price)^2
rsquared #0.87

#This seems to be a pretty good model

#------------------------------- Error distribution ----------------------------

#Now let's try to see the error distribution
errors <- test$price - test$test_price
plot(c(1:length(errors)),errors)
myline.fit <- lm(errors ~ c(1:length(errors)))
abline(myline.fit)
#errors are randomly distributed

#------------------------------- Model Interpretation ----------------------------

#The following variables are present in our model
#Variable               Coefficients 
#enginelocation         -16829.684
#carwidth               1326.824
#horsepower             72.528
#CarNamebmw             9577.642   
#CarNamebuick           11937.953   
#CarNamejaguar          11838.956

#enginelocation: Only 3 rows have enginelocation as "rear" and they are all by Porsche. 
#Engine Location if rear has a higher average value for the car.

#carwidth: Higher car width means a higher price

#horsepower : Again, more horsepower leads to a higher price

#Other three are brand names. So basically brand names also play an important role in the pricing.
#In particular, BMW, Buick and Jaguar brand names are important driving factor for price when people buy a car.
