#!/usr/bin/env python
# coding: utf-8

# In[1]:


import os # to get directory related
import pandas as pd # To use dataframe
import joblib
import sys
import numpy as np
from sklearn.ensemble import RandomForestClassifier


# In[2]:


#os.chdir("E:\\Data Analytics\\Project2")

print("Running in below working directory..")
os.getcwd()


# In[3]:


inputfilename = sys.argv[1]
outputfilename = sys.argv[2]
modelname = "rf_final_model_py.sav" 


# In[32]:


# Load the Test csv file as Data Frame
df_test = pd.read_csv(inputfilename)
print("Loaded the Input File..")


# In[33]:


# Select only the variables required for Randomn Forest Model
feature_indexes = ["season","yr","mnth","holiday","weekday","workingday","weathersit","temp","hum","windspeed"]

X_test_rf = df_test[feature_indexes]
print(feature_indexes)


# In[34]:


#Load the Saved Model
loaded_model = joblib.load(modelname)

# Predict the Test File
y_pred = loaded_model.predict(X_test_rf)
print("Loaded the Randomn Forest Model and Predicted the values..")


# In[35]:


df_test["predbikecnt"] = y_pred
df_test['predbikecnt'] = df_test['predbikecnt'].astype(int)
print('Below are the predicted values..')
df_test.head(10)


# In[36]:


df_test.to_csv(outputfilename,index = None, header = True)
print('Saved the Predicted values to the given Output file name..')

