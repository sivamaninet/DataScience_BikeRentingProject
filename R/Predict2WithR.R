library(randomForest) # Random Forest

# Set the working Directory
#setwd("E:\\Data Analytics\\Project2")

# Verify the working Directory
#getwd()

#Remove all the variables in current environment
#rm(list=ls())

# Get the Command Line Input Parameters
args = commandArgs(trailingOnly=TRUE)

print(args[1])
print(args[2])

# load the Test csv files
df_test = read.csv("test.csv",header=TRUE,sep=",")

modelname = "rf_final_model.rds"

paste("Loaded given csv files..", args[1],sep=":")

# variables for Model Test
requiredvariables = c("season","yr","mnth","holiday","weekday","workingday","weathersit","temp","hum","windspeed")
X_test = df_test[requiredvariables]

X_test$yr = as.factor(X_test$yr)
levels(X_test$yr) <- c(0,1)

X_test$mnth = as.factor(X_test$mnth)
levels(X_test$mnth) <- c(1,2,3,4,5,6,7,8,9,10,11,12)

X_test$workingday = as.factor(X_test$workingday)
levels(X_test$workingday) <- c(0,1)

X_test$season = as.factor(X_test$season)
levels(X_test$season) <- c(1,2,3,4)

X_test$holiday = as.factor(X_test$holiday)
levels(X_test$holiday) <- c(0,1)

X_test$weekday = as.factor(X_test$weekday)
levels(X_test$weekday) <- c(0,1,2,3,4,5,6)

X_test$weathersit = as.factor(X_test$weathersit)
levels(X_test$weathersit) <- c(1,2,3)

#Load the Saved Model
final_model<-readRDS(modelname)

paste("Loaded the model..", modelname,sep=":")

# Predict the value
final_predictions <- predict(final_model,newdata=X_test)

# Create new variable with Predicted Values
df_test["cntpred"] = as.integer(final_predictions)

write.csv(df_test,args[2])

print("Saved the Predicted values to..test_predicted.csv file")
