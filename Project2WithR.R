#install.packages("sampling")
library(sampling)
#install.packages("Boruta")
library(corrgram)
library(rpart) # Decision Tree
library(randomForest) # Random Forest
library(ggplot2)


# Set the working Directory
setwd("E:\\Data Analytics\\Project2")

# Verify the working Directory
getwd()

#Remove all the variables in current environment
rm(list=ls())

# load the Train csv files
df_train = read.csv("day.csv",header=TRUE,sep=",")

#Summary of data
dim(df_train)
summary(df_train)
str(df_train)

#Apply Pre-processing techniques

#Pre-processing Technique #1: Convert the variables to required data types
df_train$dteday = as.Date(df_train$dteday)
df_train$yr = as.factor(df_train$yr)
df_train$mnth = as.factor(df_train$mnth)
df_train$season = as.factor(df_train$season)
df_train$holiday = as.factor(df_train$holiday)
df_train$weekday = as.factor(df_train$weekday)
df_train$workingday = as.factor(df_train$workingday)
df_train$weathersit = as.factor(df_train$weathersit)

# Exploratory Data Analysis
# 1. Check Yearly data - Bike count
df_yearwise_data = aggregate(df_train$cnt, by = list(df_train$yr,df_train$mnth), sum) 

# Update with correct column names
colnames(df_yearwise_data) = c("yr","mnth","cnt")

# Update the identifier with real value for better understanding of data
levels(df_yearwise_data$yr) <- c("2011","2012")

# Plot the graph to understand the data
ggplot(data=df_yearwise_data, aes(x=mnth, y=cnt, fill=yr)) +
  ggtitle("Year and Month Wise Bike Count") + 
  geom_bar(stat="identity")

# Based on this, 
# we can clearly see, Bike Bookings are more from March Month to October Month(in every year)

# 2. Check Season Wise and week Day wise Data - Bike Count
df_seasonwise_data = aggregate(df_train$cnt, by = list(df_train$season,df_train$weekday), sum) 

# Update with correct column names
colnames(df_seasonwise_data) = c("season","weekday","cnt")

# Update the identifier with real value for better understanding of data
levels(df_seasonwise_data$season) <- c("springer","summer","fall","winter")

# Plot the graph to understand the data
ggplot(df_seasonwise_data[-2], aes(x="", y=cnt, fill=season))+
  ggtitle("Season Wise Count") + 
  geom_bar(stat="identity", width = 1)+
  coord_polar("y", start=0)

# Based on the Observations, 
# we can cleary the Bike bookings are more in fall, Summer and Winter seasons

# 3. Check Week Day and Season wise - Bike Count
ggplot(df_seasonwise_data, aes(x=season, y=cnt, color=season)) +
  ggtitle("Season and Week Day Wise Count") +
  geom_point()

# Based on this observations, 
# we dont see any impact on Week Days (all days bookings are distributed).

# 4. Check Temperature Wise Data - Bike Count
df_tempwise_data = aggregate(df_train$cnt, by = list(df_train$temp), sum) 

# Update with correct column names
colnames(df_tempwise_data) = c("temp","cnt")

ggplot(df_tempwise_data, aes(x=temp, y=cnt, color=temp)) +
  ggtitle("Temperature Wise Count") +
  geom_point()

# Based on the observations, The Bike Bookings are more when Temperature is between 0.4 to 0.7

# 4. Check Humidity Wise - Bike Count
df_hum_wise = aggregate(df_train$cnt, by = list(df_train$hum), sum)

colnames(df_hum_wise) = c("hum","cnt")

# Plot the Graph to check the Humidity Pattern
ggplot(df_hum_wise, aes(x=hum, y=cnt, color=hum)) +
  ggtitle("Humidity Wise Count") +
  geom_point()

# Based on the Observations, we see that Bike Bookings are more when Humidity is between 0.5 to 0.8


# 5. Check Windspeed wise data - Bike Count
df_windspeed_wise = aggregate(df_train$cnt, by = list(df_train$windspeed), sum)

colnames(df_windspeed_wise) = c("windspeed","cnt")

# Plot the Graph to check the pattern
ggplot(df_windspeed_wise, aes(x=windspeed, y=cnt, color=windspeed)) +
  ggtitle("WindSpeed Wise Count") +
  geom_point()

# Based on the observations, the Bike Bookings are more when the WindSpeed is between 0.1 to 0.3

# 6. Check Weather wise data - Bike Count
df_weatherwise_data = aggregate(df_train$cnt, by = list(df_train$weathersit), sum)

colnames(df_weatherwise_data) = c("weathersit","cnt")

# Replace the Identifier values with original values for better understanding
levels(df_weatherwise_data$weathersit) <- c("Clear, Few clouds, Partly cloudy, Partly cloudy",
                                            "Mist + Cloudy, Mist + Broken clouds, Mist + Few clouds, Mist",
                                            "Light Snow, Light Rain + Thunderstorm + Scattered clouds, Light Rain + Scatteredclouds",
                                            "Heavy Rain + Ice Pallets + Thunderstorm + Mist, Snow + Fog")

# Plot the interactive graph for better understanding
ggplot(data=df_weatherwise_data, aes(x=weathersit, y=cnt, fill=weathersit)) +
  ggtitle("Weather Wise Bike Count") + 
  geom_bar(stat="identity") 

# Based on the Observations, we see the When Weather is "Clear", "Mist+Cloudy", the bookings are more respectively



#Pre-processing Technique #2: Missing Value analysis

# Verify the Missing value in all variables
df_missing_val = data.frame(apply(df_train,2,function(x){sum(is.na(x))}))

names(df_missing_val)[1] = "MissingValueCount"
df_missing_val$variable <- rownames(df_missing_val)
rownames(df_missing_val)<-NULL

# No Missing values found
df_missing_val[df_missing_val$MissingValueCount >0,]
#<0 rows> (or 0-length row.names)


#Pre-processing Technique #2: Feature Selection
corrgram(df_train,order=TRUE,upper.panel = panel.pie,main = "Correlation Plot")

# Based on the Observations, we see that "atemp" is highly Correlated with "temp" variable, so we can drop "atemp" variable
# We see that "registered" is higly correlated with "cnt" variable, we can drop "registered" variable
# We can also drop "instant", "casual" variables which will not have relavant information to predict the "cnt" variable
df_train$atemp = NULL
df_train$instant = NULL

# Since the Total No. Of Bike Count is Registered + Casual
# We can drop these two variables as our target variable is "cnt"
df_train$registered = NULL
df_train$casual = NULL

# Compute the analysis of variance
res_aov <- aov(cnt ~ season + yr + mnth + holiday+weekday + workingday+weathersit, data = df_train)
# Summary of the analysis
summary(res_aov)

# Based on the ANOVA test, we see that all categorical variables are having p-value < 0.05, 
# so the categorical variables plays role in deciding the target "cnt" variable

# we can drop the date variable, since the values are already split to "yr","mnth","weekday"
df_train$dteday = NULL

# Plot the Box Plots for numerical variables to check the outliers
boxplot(df_train[,"temp"])$out
boxplot(df_train[,"hum"])$out
boxplot(df_train[,"windspeed"])$out

# Histogram with Outlier
hist(df_train$hum)
hist(df_train$windspeed)

# Replace the Outlier Values with NA
for(col in names(df_train)){
  if(class(df_train[,col])=="numeric"){
    print(col)
    outlierdata = boxplot(df_train[,col])$out
    if(length(outlierdata)>0){
      print(outlierdata)
      df_train[,col][df_train[,col] %in% outlierdata] = NA
    }
  }
}

# Get the List of Variables with NA
list_na <- colnames(df_train)[ apply(df_train, 2, anyNA) ]

# Get the Mean of each variable
average_missing <- apply(df_train[,colnames(df_train) %in% list_na],
                         2,
                         mean,
                         na.rm =  TRUE)

# Update the NA with Mean of each variable to replace the outlier data
for(col in names(df_train)){
  if(class(df_train[,col])=="numeric"){
    print(col)
    df_train[,col][is.na(df_train[,col])] = average_missing[col]
  }
}


# Histogram without Outlier
hist(df_train$hum)
hist(df_train$windspeed)

# Verify the structure of final data
str(df_train)


# Build X_train, y_train, X_test, y_test
df_sample = sort(sample(nrow(df_train), nrow(df_train)*.8))
X_train<-df_train[df_sample,-11]
X_test<-df_train[-df_sample,-11]

y_train <- df_train[df_sample,11]
y_test <- df_train[-df_sample,11]

# Model Creation: #1. Logistic Regression
model_lr <- lm(y_train~.,data = X_train)
summary(model_lr)
#Multiple R-squared: 0.85
#Adjusted R-squared: 0.8427 
#F-statistic: 116.7 on 27 and 556 DF,  
#p-value: < 2.2e-16

predict_lr <- predict(model_lr, X_test)
actuals_preds_lr <- data.frame(cbind(actuals=y_test, predicteds=predict_lr))

correlation_accuracy_lr <- cor(actuals_preds_lr) 
min_max_accuracy_lr <- mean(apply(actuals_preds_lr, 1, min) / apply(actuals_preds_lr, 1, max))  
mape_lr <- mean(abs((actuals_preds_lr$predicteds - actuals_preds_lr$actuals))/actuals_preds_lr$actuals)  
mse_lr <- mean(model_lr$residuals^2)
# correlation_accuracy = 90.2%
# min_max accuracy = 84.67%, 
# mean absolute percentage deviation = 18.52%, 
# Mean Squared Error(MSE) = 579230.3


plot(actuals_preds_lr$predicteds,actuals_preds_lr$actuals,
     xlab="predicted",ylab="actual")
abline(a=0,b=1)


# Model 2: Randomn Forest Regression Model
model_rf <- randomForest(y_train~., data=X_train, mtry=3,ntree=100,
                         importance=TRUE, na.action=na.omit)

predict_rf <- predict(model_rf,X_test)

actuals_preds_rf <- data.frame(cbind(actuals=y_test, predicteds=predict_rf))

correlation_accuracy_rf <- cor(actuals_preds_rf) 
min_max_accuracy_rf <- mean(apply(actuals_preds_rf, 1, min) / apply(actuals_preds_rf, 1, max))  
mape_rf <- mean(abs((actuals_preds_rf$predicteds - actuals_preds_rf$actuals))/actuals_preds_rf$actuals)  
# correlation_accuracy = 94.3%
# min_max accuracy = 89.21%
# mean absolute percentage deviation = 14.44%,


plot(actuals_preds_rf$predicteds,actuals_preds_rf$actuals,
     xlab="predicted",ylab="actual")
abline(a=0,b=1)

var_importance = data.frame(importance(model_rf,type=1))

df_varimp = cbind(rownames(var_importance),var_importance)
colnames(df_varimp) = c("variableName","importance")
# Plot the interactive graph for better understanding
ggplot(data=df_varimp, aes(x=variableName, y=importance,fill=variableName)) +
  ggtitle("Variable Importance in Random Forest Model") + 
  geom_bar(stat="identity") 


head(X_train,1)

str(X_train)
str(df_test)

# Test the Model with Sample Data
df_test = data.frame(
              season = 1,
              yr=0,
              mnth=1,
              holiday = 0,
              weekday = 3,
              workingday=1,
              weathersit = 1,
              temp=0.226957,
              hum=0.436957,
              windspeed = 0.186900
            )

# Convert the data type and assign appropriate levels to test data
df_test$yr = as.factor(df_test$yr)
levels(df_test$yr) <- levels(df_train$yr)

df_test$mnth = as.factor(df_test$mnth)
levels(df_test$mnth) <- levels(df_train$mnth)

df_test$workingday = as.factor(df_test$workingday)
levels(df_test$workingday) <- levels(df_train$workingday)

df_test$season = as.factor(df_test$season)
levels(df_test$season) <- levels(df_train$season)

df_test$holiday = as.factor(df_test$holiday)
levels(df_test$holiday) <- levels(df_train$holiday)

df_test$weekday = as.factor(df_test$weekday)
levels(df_test$weekday) <- levels(df_train$weekday)

df_test$weathersit = as.factor(df_test$weathersit)
levels(df_test$weathersit) <- levels(df_train$weathersit)

predict_rf <- predict(model_rf,df_test)

# Actual Value = 1600
# Predicted Value = 1324

# Save the Model
saveRDS(model_rf, "./rf_final_model.rds")
