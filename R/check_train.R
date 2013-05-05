# Copyright 2010 Google Inc. All Rights Reserved.
# Author: markko@google.com (Markko Ko)
# Modified by w.victoryee@gmail.com (Victor Yee), 2013-05-04

CheckTrain <- function(unique.identifier,
                       verbose = FALSE) {
  # Checks the training status of an object in Google Storage if it's completed
  
  # Connect to prediction API and get result
  result.conn <- ConnectionHandler(connect.type = "get",
                                   unique.identifier = unique.identifier,
                                   data.tosend = "",
                                   verbose = verbose)
  if (!result.conn$succeed.connect)
    stop("Connection to API failed, stop.\n")
  
  # Translate the json result to R object
  result <- JsonToData(result.conn$data, verbose)
  
  if (verbose) { cat(unlist(result),"\n") }
  
  # Handle json format data from prediction API
  output <- ResultHandler(result.data = result,
                          mode = "get",
                          verbose = verbose)
  return(output)
}