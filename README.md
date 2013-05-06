# gpredictr

gpredictr is forked from the googlepredictionapi library by Maciej Janiec
(http://code.google.com/p/r-google-prediction-api-v12/,
which is itself a fork from the googlepredictionapi library by Markko Ko
(https://code.google.com/p/google-prediction-api-r-client/)

This package is updated to work with the Google Prediction API version 1.5.

This is currently a WORK IN PROGRESS and has not been extensively tested. Some
features (such as directly uploading a training dataset via the high-level
PredictionApiTrain) are not currently available.

## Prerequisites
 1. A Google Cloud Storage account
 2. Access to the Google Prediction API
 3. That you upload your training dataset to your Google Cloud Storage account

## Usage
```{r}
### Load libraries -------------------------------------------------------------
library(rjson)
library(RCurl)
library(gpredictr)

### Initialize -------------------------------------------------------------------

### Turn off SSL check
# See http://code.google.com/p/r-google-analytics/issues/detail?id=1#c5 & http://www.omegahat.org/RCurl/FAQ.html
options(RCurlOptions = list(capath = system.file("CurlSSL", "cacert.pem", package = "RCurl"),
                            ssl.verifypeer = FALSE))

### Put your own email, password and API key below
my.email <- "example@example.com"
my.password <- "averystrongpassword"
my.api.key <- "googleapikey"

### High-level functions -------------------------------------------------------

### Set path to remote file on Google Cloud Storage
remote.file <- "gs://prediction-example/language_id.txt"

### High-level functions to train a model using the remote file, and predict
###   a label or value from new data.
### Returns a "PredictionApiModel" class, on which generic functions 'summary'
### and 'predict' can be used. Note that the only really important variable,
### though, is unique.identifier (See "Lower-level functions").
language.id.model <- PredictionApiTrain(unique.identifier = "languageIdentifier",
                                        remote.file = remote.file)
# Training request submitted.
# Training is running on languageIdentifier: time:0.85 seconds

### Request a summary of your model
summary(object = language.id.model)
### While the model is running:
# Unique Identifer:     languageIdentifier
# Remote File Location: gs://prediction-example/prediction-examplelanguage_id.txt
# Current Status:       RUNNING
### When the model is done training:
# Unique Identifer:     languageIdentifier
# Remote File Location: gs://prediction-example/prediction-examplelanguage_id.txt
# Current Status:       DONE
# Job Info --------------------------------------------
# Created:              2013-05-05T07:16:09.077Z
# Training Completed:   2013-05-05T07:17:26.618Z
# Number of Instances:  406
# Model Type:           classification
# Number of Labels:     3
# Accuracy:             0.98
# ----------------------------------------------------

### High-level function to check status (e.g., RUNNING, DONE) of a model
PredictionApiCheckTrainingStatus(unique.identifier = "languageIdentifier")
### While the model is running:
# $training.status
# [1] "RUNNING"
### When te model is done training:
# $training.status
# [1] "DONE"
# 
# $unique.identifier
# [1] "languageIdentifier"
# 
# $created
# [1] "2013-05-05T07:16:09.077Z"
# 
# $trainingComplete
# [1] "2013-05-05T07:17:26.618Z"
# 
# $numberInstances
# [1] "406"
# 
# $modelType
# [1] "classification"
# 
# $numberLabels
# [1] "3"
# 
# $accuracy
# [1] 0.98

### Predict a response to one test instance
predict(object = language.id.model, newdata = "le français")
# [1] "French"   "Spanish"  "English"  "0.392296" "0.311996" "0.295708"

### Returns a matrix listing all currently available models and their statuses
PredictionApiListModels()
#      id                   trainingStatus numberInstances numberLabels accuracy
# [1,] "languageIdentifier" "DONE"         "406"           "3"          "0.98"  
# [2,] "test-data"          "RUNNING"      NA              NA           NA

### Removes a previously trained model
PredictionApiRemoveModel(unique.identifier = "languageIdentifier")
### If the model is successfully deleted:
# $status
# [1] "SUCCESS"
### If the model has already been deleted, or is not found:
# $status
# [1] "ERROR"
# 
# $error.code
# [1] "404"
# 
# $error.message
# [1] "Not Found"
# 
# $error.information
# [1] "No Model found. Model must first be trained."

### Lower-level functions (not run) --------------------------------------------
train.runner <- function(unique.identifier = "languageIdentifier",
                         remote.file = remote.file,
                         verbose = FALSE,
                         check.until.done = FALSE,
                         check.interval = 5)

### Useful when you do not have a PredictionApiModel class object saved in your
### R workspace, but know the unique.identifier of your model
predict.runner(unique.identifier = "languageIdentifier", "le français")
```