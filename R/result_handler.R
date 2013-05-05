# Copyright 2010 Google Inc. All Rights Reserved.
# Author: markko@google.com (Markko Ko)
# Modified for Google API v1.2 and extended by Maciej Janiec (mjaniec@gmail.com), 2011-05-30
# Modified by w.victoryee@gmail.com (Victor Yee), 2013-05-04

ResultHandler <- function(result.data,
                          mode = "insert",
                          verbose = FALSE) {
  # Handles json format result from Google Prediction API
  # and outputs a R style result object with status information.
  # It accepts four connection modes: "insert", "predict", "get", and "list"
  
  # String for detecting training status in checking
  flag.check <- "RUNNING"
  
  # Check input
  mode.type <- c("insert", "predict", "get", "list")
  mode <- match.arg(mode, mode.type)
  
  if (missing(result.data) || length(result.data) == 0)
    stop("'result.data' should not be empty")
  
  # Handle prediction result
  if (mode == "predict") {
    # Handle prediction result
    # The format of json result is:
    # {
    #   "kind": "prediction#output",
    #   "id": "unique.identifier",
    # 	"selfLink": "https://www.googleapis.com/prediction/v1.5/URL_of_resource,
    #   "outputLabel": "The most likely class label [Categorical models only]",
    #   "outputMulti": [
    #      {
    #        "label": ${string},
    #        "score": ${double}
    #      }],
    #   "OutputValue": "The estimated regression value [Regression models only]."
    # }
    
    # Check if error
    if (is.null(result.data$outputLabel)) {
      return(list(result            = "ERROR",
                  error.code        = result.data$error$code,
                  error.message     = result.data$error$message,
                  error.information = result.data$error$errors))
    }
    
    result.final <- list(final = result.data$outputLabel,
                         multi = length(result.data$outputMulti))
    result.final$labels <- character(result.final$multi)
    result.final$scores <- double(result.final$multi)
    
    for (i in 1:result.final$multi) {
      result.final$labels[i] <- result.data$outputMulti[[i]]$label
      result.final$scores[i] <- result.data$outputMulti[[i]]$score
    }
    
    result.final$order  <- order(result.final$scores, decreasing = TRUE)
    result.final$labels <- result.final$labels[result.final$order]
    result.final$scores <- result.final$scores[result.final$order]
    
    result.final <- c(result.final$labels, result.final$scores)
    
    return(result.final)
  }
  
  if (mode == "insert") {
    # Handle training result
    # The format of json result is:
    # {
    #   "kind":"prediction#training",
    # 	"id":"unique.identifier",
    #   "storageDataLocation": "bucket.name/object.name",
    # 	"selfLink":"https://www.googleapis.com/prediction/v1.5/URL_of_resource,
    # }
    
    # Check error on result
    result.location <- result.data$storageDataLocation
    storage.data.location <- unlist(strsplit(result.location, "/", fixed = TRUE))
    
    if (is.null(result.location)) {
      # If error, output the error code and reason in json
      return(list(result          = "ERROR",
                  error.code      = result.data$error$code,
                  error.message   = result.data$error$message,
                  error_debuginfo = result.data$error$errors))
    }
    
    # Split and get info from json
    return(list(result      = "SUBMITTED",
                unique.identifier = result.data$id,
                bucket.name = storage.data.location[1],
                object.name = substr(storage.data.location,
                                     regexpr("/", storage.data.location)[1] + 1,
                                     nchar(storage.data.location))
    ))  
  }
  
  if (mode == "get") {
    # Check the status of result (notFinish or finished)
    # If it's finished, return location and accuracy.
    # The json format of result from Prediction API is:
    # {"data":{"kind": "prediction#training",
    #          "id": "${unique.identifier}",
    #          "selfLink": "https://www.googleapis.com/prediction/v1.5/URL_of_resource,
    #          "created": "${datetime}",
    #          "trainingComplete: "${datetime}",
    #          "modelInfo": {"numberInstances": "${numberInstances},
    #                        "modelType": "classification" or "regression",
    #                        "numberLabels": "${numberLabels}",
    #                        "classificationAccuracy": ${classificationAccuracy}},
    #  "trainingStatus": "DONE"}
    
    # Check error on result
    result_label <- result.data$id
    result.location <- result.data$storageDataLocation
    
    if (is.null(result_label)) {
      # If error, output the error code and reason
      return(list(training.status = "ERROR",
                  error.code = result.data$error$code,
                  error.message = result.data$error$message,
                  error.information = result.data$error$errors))
    }
    
    # Get information from result
    training.status  <- result.data$trainingStatus
    
    if (training.status == flag.check) {
      output <- list(training.status = "RUNNING")
    } else {
      # split and get accuracy and location in Google Storage
      result.location   <- unlist(strsplit(result_label, "/", fixed = TRUE))
      output <- list(training.status   = training.status,
                     unique.identifier = result.data$id,
                     created           = result.data$created,
                     trainingComplete  = result.data$trainingComplete,
                     numberInstances   = result.data$modelInfo$numberInstances,
                     modelType         = result.data$modelInfo$modelType,
                     numberLabels      = result.data$modelInfo$numberLabels,
                     accuracy          = result.data$modelInfo$classificationAccuracy)
    }
    return(output)
  }
  
  if (mode == "list") {
    # Check the status of result (notFinish or finished)
    # If it's finished, return location and accuracy.
    # The json format of result from Prediction API is:
    # {
    #   "kind": 
    #   "selfLink":
    #   "items": [
    #     { ... },
    #     { ... }
    # }
    
    result.conv      <- JsonToData(result.data$data)
    number.of.items  <- length(result.conv$items)
    output           <- matrix(nrow = number.of.items, ncol = 5)
    colnames(output) <- c("id", "trainingStatus",
                          "numberInstances", "numberLabels", "accuracy")
    
    for (i in 1:number.of.items) {
      output[i, 1] <- result.conv$items[[i]]$id
      output[i, 2] <- result.conv$items[[i]]$trainingStatus
      if(output[i, 2] == "DONE") {
        output[i, 3] <- result.conv$items[[i]]$modelInfo$numberInstances
        if(result.conv$items[[i]]$modelInfo$modelType == "classification"){
          output[i, 4] <- result.conv$items[[i]]$modelInfo$numberLabels
          output[i, 5] <- result.conv$items[[i]]$modelInfo$classificationAccuracy
        } else {
          output[i, 4] <- NA
          output[i, 5] <- result.conv$items[[i]]$modelInfo$meanSquaredError        
        }
      } 
    }
    
    return(output)
  }
  
  stop("mode = '", mode, "' was not one of the recognized options")
}