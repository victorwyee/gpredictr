# Copyright 2010 Google Inc. All Rights Reserved.
# Author: markko@google.com (Markko Ko)

PredictionApiPredictRunner <- function(model,
                                       newdata,
                                       verbose = FALSE) {
  # Predicts the label of newdata (one instance) using model
  # on Google Prediction API
  #
  # Args:
  #   model: the model object returned from PredictionApiTrainRunner(),
  #          and its class type must be "prediction.api.model"
  #   newdata: one instance of data for prediction, its type should be a list,
  #            remember to remove the response at first column
  #            if its class type is character, use "text" format
  #   verbose: If TRUE, print out all detail for debugging. Default is FALSE.
  # Returns:
  #   if succeed, return the predicted label
  #   else, return the error report including
  #     1. result: "error"
  #     2. error.code
  #     3. error.message
  #     4. error.information
  #
  # Accepted data formats:
  #
  # "'some string data', 'singleval', 123 "
  # "1234, 'Man\'s best friend' "
  # "1234, 3345" 														- Numerical data does not need additional quotes.

  bucket.name <- model$bucket.name
  object.name <- model$object.name

  # detect newdata type: "numeric" or "text" ("text" may also means mixed data)
  if (length(grep("'",newdata))>0) {
    if (verbose)
      cat("'newdata' detected as text\n")
    predict.type <- "text"
  } else if (attr(regexpr("[\\d|\\.|\\-|\\+|e| |\\,]+",newdata,perl=TRUE),"match.length")==nchar(newdata)) {
  	# numeric data must conists of: digits . - + e space ,
    if (verbose)
      cat("'newdata' detected as numeric\n")
    predict.type <- "numeric"
  } else {
    stop("The class type of 'newdata' should be either character or numeric")
  }
  
  # number of row should be no more than 1
  row <- nrow(newdata)
  if (!is.null(row) && row > 1) {
    stop("'newdata' should only have one row: ", newdata)
  }
  
  # mixed types need surrounding with "
  if (predict.type == "text") {
  	newdata <- paste('\"',newdata,'\"',sep="")
  }
  
  data.tosend <- paste('{\"input\" : { \"csvInstance\" : [ ',newdata,' ]}}',sep="")
  
  if (verbose)
    cat("data to send:",data.tosend,"\n")
  

  # connect to Prediction API and get result
  result.conn <- PredictionApiConnectHandler(connect.type = "predict",
                                             bucket.name = bucket.name,
                                             object.name = object.name,
                                             data.tosend = data.tosend,
                                             verbose = verbose)

  # translate the json result to R object
  result <- PredictionApiJsonToData(result.conn$data, verbose)

  # handle json from prediction API
  output <- PredictionApiResultHandler(result.data  = result,
                                       mode         = "predict",
                                       data.type		= predict.type,
                                       verbose      = verbose)

  # handle result
  if (!is.atomic(output) && output$result == "error") {
    cat("Error: ", output$error.message,
        "\ncheck return value for more information\n")
  }
  return(output)
}
