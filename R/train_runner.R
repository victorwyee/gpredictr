# Copyright 2010 Google Inc. All Rights Reserved.
# Author: markko@google.com (Markko Ko)
# Modified for Google API v1.2 and extended by Maciej Janiec (mjaniec@gmail.com), 2011-05-30
# Modified by w.victoryee@gmail.com (Victor Yee), 2013-05-04

TrainRunner <- function(unique.identifier,
                        remote.file,
                        verbose = FALSE,
                        check.until.done = FALSE,
                        check.interval = 5) {
  # Trains a model on data you provided through Google Prediction API.
  
  # Parse remote.file to bucket.name and object.name
  parse.result <- TrainParseRemoteFilename(remote.file)
  bucket.name  <- parse.result$bucket.name
  object.name  <- parse.result$object.name
  
  # Set POST data
  data.tosend <- BuildInsertPostData(unique.identifier, bucket.name, object.name)
  
  # connect to Prediction API @ prediction_api_connect_handler.R
  result.conn <- ConnectionHandler(connect.type = "insert",
                                   unique.identifier = unique.identifier,
                                   data.tosend = data.tosend,
                                   verbose = verbose)
  if (result.conn$succeed.connect) {
    cat("Training request submitted.\n")
  } else {
    stop("Connection to Prediction API failed, stop.")
  }
  
  # Translate the json format result into R object
  result.data <- JsonToData(result.conn$data, verbose)
  if (verbose) { cat("JSON=", unlist(result.data),"\n") }
  
  # Handles results from Google Prediction API
  output <- ResultHandler(result.data = result.data,
                          mode = "insert",
                          verbose = verbose)
  if (verbose) { cat("RESULT HANDLER=", unlist(output), "\n") }
  
  # Handle output
  if (output$result == "ERROR") {
    cat("Error on retrieving result from result.handler:",
        output$error.message, "\n",
        "check return value for more information", "\n")
    return(output)
  }
  
  # Check training status every check.interval seconds
  timer <- Sys.time()
  while (TRUE) {
    check.result <- CheckTrain(unique.identifier = unique.identifier,
                               verbose = verbose)
    if (check.result$training.status == "ERROR") {
      cat("Error: ", check.result$error.message,
          "\ncheck return value for more information\n")
      return(check.result)
      
    } else if (check.result$training.status == "RUNNING") {
      cat(paste("Training is running on ", unique.identifier,
                ": time:", sprintf("%.2f",
                                   difftime(Sys.time(), timer, units = "sec")),
                " seconds", "\n", sep = ''))
      
    } else if (check.result$training.status == "DONE") {
      cat(paste("Training is complete on ", unique.identifier,
                ": time:", sprintf("%.2f",
                                   difftime(Sys.time(), timer, units = "sec")),
                " seconds\n", sep = ''))
      return(WrapModel(check.result))
      
    } else {
      stop("In error field: result from resultHandler()\n")
    }
    
    if (check.until.done) { Sys.sleep(check.interval) }
    else   { break }
  }
  
  return(WrapModel(output))
}

TrainParseRemoteFilename <- function(remote.file) {
  # Checks the format of remote.file and parses the remote.file string
  # into bucket.name and object.name
  
  temp <- unlist(strsplit(remote.file, "/"))
  
  # check "gs://bucket.name/object.name" format
  if (length(grep("^gs://[^/]+/.+$", remote.file)) != 0) { # prev.:: gs://[^/]+/[^/]+$
    composite.object <- ""
    for (i in 4:length(temp)) {
      composite.object <- paste(composite.object, temp[i], sep = "/")
    }
    composite.object <- sub("/", "", composite.object)
    
    return(list(well.formed = TRUE,
                bucket.name = temp[3],
                object.name = composite.object))
  }
  
  # check "bucket.name/object.name" format
  if (length(grep("^[^/]+/.+$", remote.file)) != 0) { # prev.: [^/]+/[^/]+$
    composite.object <- ""
    for (i in 2:length(temp)) {
      composite.object <- paste(composite.object, temp[i], sep = "/")
    }
    composite.object <- sub("/", "", composite.object)
    
    return(list(well.formed = TRUE,
                bucket.name = temp[1],
                object.name = composite.object))
  }
  
  # format not match
  return(list(well.formed = FALSE,
              bucket.name = "",
              object.name = ""))
}