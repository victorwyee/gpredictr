# Copyright 2010 Google Inc. All Rights Reserved.
# Author: markko@google.com (Markko Ko)

PredictionApiCheckTrain <- function(bucket.name = "",
                                    object.name = "",
                                    data.tosend = "",
                                    verbose = FALSE) {
  # Checks the training status of an object in Google Storage
  # if it's completed
  #
  # Args:
  #   bucket.name: bucket name of object you want to check
  #   object.name: object name you want to chack
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
  if (nchar(bucket.name) < 1)
    stop("'bucket.name' should not be empty")
  if (missing(object.name) || !is.character(object.name))
    stop("'object.name' must be character")
  if (nchar(object.name) < 1)
    stop("'object.name' should not be empty")

  # connect to prediction API and get result
  result.conn <- PredictionApiConnectHandler(connect.type = "check",
                                             bucket.name = bucket.name,
                                             object.name = object.name,
                                             data.tosend = "",
                                             verbose = verbose)
  if (!result.conn$succeed.connect)
    stop("Connection to API failed, stop.\n")

  # translate the json result to R object
  result <- PredictionApiJsonToData(result.conn$data, verbose)
  
  cat(unlist(result),"\n")

  # handle json format data from prediction API
  output <- PredictionApiResultHandler(result.data = result,
                                       mode = "check",
                                       verbose = verbose)
  return(output)
}
