# Copyright 2010 Google Inc. All Rights Reserved.
# Author: markko@google.com (Markko Ko)
# Modified by w.victoryee@gmail.com (Victor Yee), 2013-05-04

### Builds POST data to send in an 'insert' API call
# Used in TrainRunner()
BuildInsertPostData <- function(unique.identifier, bucket, object) {
  paste0('{"id" : "', unique.identifier, '",',
         ' "storageDataLocation" : "', bucket, '/', object, '"}')
}

### Adds "PredictionApiModel" class to a result
# Used in PredictionApiTrainRunner()
WrapModel <- function(result) {
  class(result) <- "PredictionApiModel"
  return(result)
}

# Used for APIv0.12 but no longer needed for APIv0.15
HexSlash <- function(object) {
  gsub("/", "%2F", object)
}

# Used for storage utilities in storage_util.R
PythonCommand <- function(command) {
  return(paste(myPython, " ", myGSUtilPath, command, sep = ""))
}
