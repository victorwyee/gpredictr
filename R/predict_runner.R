# Copyright 2010 Google Inc. All Rights Reserved.
# Author: markko@google.com (Markko Ko)
# Modified by w.victoryee@gmail.com (Victor Yee), 2013-05-04

PredictRunner <- function(unique.identifier,
                          newdata,
                          verbose = FALSE) {
  # Predicts the label of newdata (one instance) using model
  # on Google Prediction API
  
  # Detect newdata type: "numeric" or "text" ("text" may also means mixed data)
  if (!is.numeric(newdata)) {
    if (verbose) { cat("'newdata' detected as text\n") }
    predict.type <- "text"
  } else if (attr(regexpr("[\\d|\\.|\\-|\\+|e| |\\,]+", newdata, perl = TRUE), "match.length") == nchar(newdata)) {
    # Numeric data must conists of: digits . - + e space ,
    if (verbose) { cat("'newdata' detected as numeric\n") }
    predict.type <- "numeric"
  } else {
    stop("The class type of 'newdata' should be either character or numeric")
  }
  
  # Number of row should be no more than 1
  row <- nrow(newdata)
  if (!is.null(row) && row > 1) {
    stop("'newdata' should only have one row: ", newdata)
  }
  
  # Mixed types need surrounding with "
  if (predict.type == "text") {
    newdata <- paste('\"', newdata,'\"',sep="")
  }
  
  data.tosend <- paste('{\"input\" : { \"csvInstance\" : [ ', newdata,' ]}}',sep = "")
  
  if (verbose) { cat("data to send:", data.tosend, "\n") }
  
  # Connect to Prediction API and get result
  result.conn <- ConnectionHandler(connect.type      = "predict",
                                   unique.identifier = unique.identifier,
                                   data.tosend       = data.tosend,
                                   verbose           = verbose)
  
  # Translate the json result to R object
  result <- JsonToData(result.conn$data, verbose)
  
  # Handle json from prediction API
  output <- ResultHandler(result.data  = result,
                          mode         = "predict",
                          verbose      = verbose)
  
  # Handle result
  if (!is.atomic(output) && output$result == "error") {
    cat("Error: ", output$error.message,
        "\ncheck return value for more information\n")
  }
  
  return(output)
}