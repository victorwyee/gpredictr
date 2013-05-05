# Copyright 2010 Google Inc. All Rights Reserved.
# Author: markko@google.com (Markko Ko)
# Modified for Google API v1.2 and extended by Maciej Janiec (mjaniec@gmail.com), 2011-05-30
# Modified by w.victoryee@gmail.com (Victor Yee), 2013-05-04

PredictionApiTrain <- function(unique.identifier,
                               remote.file,
                               verbose = FALSE,
                               check.until.done = FALSE,
                               check.interval = 5) {
  # Trains a model using data you provided through Google Prediction API
  
  # Check if remote file name is well-formed
  if (missing(remote.file) || !is.character(remote.file))
    stop("'remote.file' should be character")
  check.result <- train.parse.remote.filename(remote.file)
  if (!check.result$well.formed) {
    stop("remote.file format error: should be string with format: ",
         "\"bucket.name/object.name\" or \"gs://bucket.name/object.name\"\n")
  }
  
  # Passed to TrainRunner @ prediction_api.TrainRunner.R
  result <- TrainRunner(unique.identifier = unique.identifier,
                        remote.file       = remote.file,
                        verbose           = verbose,
                        check.until.done  = check.until.done,
                        check.interval    = check.interval)
  return(result)
}

summary.PredictionApiModel <- function(object, ...) {
  # Output a summary report for model of type: 'predictionapiModel'.
  # The report includes:
  #   1. Location of data in Google Storage
  #   2. Estimated accuracy from Google Prediction API
  # It's a generic function so that user can use summary(model) to call it
  #
  # Args:
  #   object: model with class type: PredictionApiModel
  # Returns:
  #   print a report including:
  #     1. Location of data
  #     2. Estimated accuracy

  report <- paste("Location of data: ", object$bucket.name, "/",
                  object$object.name, "\n",
                  "Estimated accuracy: ",
                  object$accuracy, "\n", sep='')
  cat(report)
}

predict.PredictionApiModel <- function(object,
                                       newdata,
                                       verbose = FALSE,
                                       ...) {
  # Predicts the label of newdata using model on Prediction API.
  # example: not runnable, you need to provide bucket.name and object.name
  #          from your Google Storage account
  #   # load data
  #   data(wdbcData)
  #   # train a model using this data first
  #   model <- PredictionApiTrain(remote.file="bucket.name/object.name")
  #   # use this model and take the first row without label
  #   # from data as newdata to predict
  #   predict(model,newdata=c(as.numeric(wdbcData[1, 2:ncol(wdbcData)])))
  #   # then it will return a label
  #
  # Args:
  #   object: the model object returned from PredictionApiTrainRunner(),
  #     it's class type must be "PredictionApiModel"
  #   newdata: a data list to be predicted,
  #     if the class type is character, I'll use "text"
  #   verbose: If TRUE, print out all detail for debugging. Default is FALSE.
  # Returns:
  #   if succeed, return predicted label
  #   else, return the error report including
  #     1. result: "error"
  #     2. error.code
  #     3. error.message
  #     4. error.information

  # check input
  if (missing(object))
    stop("object should not be null")
  if (!is(object, "PredictionApiModel"))
    stop("object should be type: PredictionApiModel")
  bucket.name <- object$bucket.name
  object.name <- object$object.name
  if (is.null(bucket.name) || !is.character(bucket.name))
    stop("'bucket.name' must be character")
  if (nchar(bucket.name) == 0)
    stop("'bucket.name' should not be empty")
  if (is.null(object.name) || !is.character(object.name))
    stop("'object.name' must be character")
  if (nchar(object.name) == 0)
    stop("'object.name' should not be empty")
  if (missing(newdata) || nchar(newdata) == 0)
    stop("'newdata' should not be empty")

  result <- PredictionApiPredictRunner(model    = object,
                                       verbose  = verbose,
                                       newdata  = newdata)
  return(result)
}

PredictionApiCheckTrainingStatus <- function(bucket.name,
                                             object.name,
                                             verbose = FALSE) {
  # Checks if the training of the given object is completed
  # example:
  #    # to check if the training of gs://bucket.name/object.name is finished
  #    PredictionApiCheckTrainingStatus("bucket.name", "object.name")
  # Args:
  #   bucket.name: bucket.name of data location in Google Storage
  #   object.name: object.name of data location in Google Storage
  #   verbose: If TRUE, print out all detail for debugging. Default is FALSE.
  # Returns:
  #   if error, return the error report including
  #     1. result: "error"
  #     2. error.code
  #     3. error.message
  #     4. error.information
  #   if NOT finished, the format will be:
  #     1. result: "notFinish"
  #   else
  #     1. result: "finished"
  #     2. cv.accuracy: accuracy
  #     3. bucket.name
  #     4. object.name

  # check input
  if (missing(bucket.name) || !is.character(bucket.name))
    stop("'bucket.name' must be character")
  if (nchar(bucket.name) <= 1)
    stop("'bucket.name' should not empty")
  if (missing(object.name) || !is.character(object.name))
    stop("'object.name' must be character")
  if (nchar(object.name) <= 1)
    stop("'object.name' should not empty")

  result <- PredictionApiCheckTrain(bucket.name = bucket.name,
                                    object.name = HexSlash(object.name),
                                    verbose = verbose)
  return(result)
}

PredictionApiListObjects <- function(bucket.name = "",
                                     object.name = "",
                                     verbose = FALSE) {
  # Lists all buckets if object.name is not provided
  # or list all objects if bucket.name is provided.
  # example:
  #    PredictionApiListObjects("bucket.name") # this will return
  #                                            # a list of objects you have
  #    PredictionApiListObjects() # this will return a list of buckets you have
  # Args:
  #   bucket.name: the bucket.name of data location in Google Storage
  #   object.name: the object.name of data location in Google Storage
  #   verbose: If TRUE, print out all detail for debugging. Default is FALSE.
  # Returns:
  #   if succeed: a list of information including
  #     1. result: "correct"
  #     2. value: message from Google Storage:
  #       a list of your bucket(not specify bucket.name),
  #       a list of your object(specify bucket.name but not object.name)
  #       object name you specified(both bucket.name and object.name)
  #   else: a list of error message including:
  #     1. result: "error"
  #     2. status: status code from Google Storage API
  #     3. code: error code from Google Storage API
  #     4. reason: reason message from Google Storage API

  result <- PredictionApiUtilLs(bucket.name = bucket.name, # @ prediction_api_storage_util.R
                                object.name = object.name,
                                verbose = verbose)
  return(result)
}

PredictionApiRemoveObjects <- function(bucket.name,
                                       object.name,
                                       verbose = FALSE) {
  # Removes the given object in Google Storage
  #
  # Args:
  #   bucket.name: the bucket.name of data location in Google Storage
  #   object.name: the object.name of data location in Google Storage
  #   verbose: If TRUE, print out all detail for debugging. Default is FALSE.
  # Returns:
  #   if succeed: a list of information including
  #     1. result: "correct"
  #     2. value: message from Google Storage
  #   else: a list of error message including:
  #     1. result: "error"
  #     2. status: status code from Google Storage API
  #     3. code: error code from Google Storage API
  #     4. reason: reason message from Google Storage API

  result <- PredictionApiUtilRm(bucket.name = bucket.name,
                                object.name = object.name,
                                verbose = verbose)
  return(result)
}
