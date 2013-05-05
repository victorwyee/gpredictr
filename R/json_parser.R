# Copyright 2010 Google Inc. All Rights Reserved.
# Author: markko@google.com (Markko Ko)

PredictionApiJsonToData <- function(data, verbose = FALSE) {
  # This function translates json format data string into R object
  #
  # Args:
  #   data: the json string you want to translate
  #   verbose: If TRUE, prints all detail for debugging. Default is FALSE.
  # Returns:
  #   R object

  if (verbose)
    cat(paste("from Json:", data), "\n")

  if (is.null(data))
    return(NULL)

  # parse to R object
  result <- fromJSON(data)

  if (verbose)
    cat(paste("to Data:", result), "\n")
    
  return(result)
}

PredictionApiDataToJson <- function(data, verbose = FALSE) {
  # This function translates R object into json format data string
  #
  # Args:
  #   data: R object you want to translate
  #   verbose: If TRUE, prints all detail for debugging. Default is FALSE.
  # Returns:
  #   json format string

  if (verbose)
    cat(paste("from Data:", data))

  if (is.null(data))
    return(NULL)

  # parse the data
  result <- toJSON(data)

  if (verbose)
    cat(paste("to Json:", result))
    
  return(result)
}

