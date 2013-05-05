# Copyright 2010 Google Inc. All Rights Reserved.
# Author: markko@google.com (Markko Ko)
# ---
#
# modified for Google API v1.2 and extended by Maciej Janiec (mjaniec@gmail.com), 2011-05-30

PredictionApiResultHandler <- function(result.data = "",
                                       mode = "train",
                                       data.type = "numeric",
                                       verbose = FALSE) {
  # Handles json format result from Google Prediction API
  # and outputs a R style result object with status information.
  # It accepts three connection modes: "train", "predict", "check"
  #
  # Args:
  #   mode: "train", "predict", "check"
  #   result.data: the data you want to handle
  #     for train,
  #       {"data" : {"data" : "${mybucket}/${mydata}"} } }
  #     for predict,
  #       {"data" : {"kind":"prediction#output",
  #                  "outputLabel":"${label1}",
  #                  "outputMulti":[{"label":"${label1}",
  #                                  "score":${score1},
  #                                 {"label":"${label2}",
  #                                  "score":${score2}}]}}
  #     for check,
  #       {"data":{"data":"${mybucket}/${mydata}",
  #       "modelinfo":"Training has not completed."}}}
  #       or
  #       {"data":{"data":"${mybucket}/${mydata}",
  #       "modelinfo":"estimated accuracy: 0.74"}}}
  #   verbose: If TRUE, prints all detail for debugging. Default is FALSE.
  # Returns:
  #   if error, return the error report including
  #     1. result: "error"
  #     2. error.code
  #     3. error.message
  #     4. error.information
  #   if succeed,
  #     for predict:
  #       1. return predicted label
  #     for train: it includes
  #       1. result: "correct"
  #       2. accuracy: accuracy
  #       3. bucket.name
  #       4. object.name
  #     for check: it includes
  #       if NOT finished, the format will be:
  #         1. result: "notFinish"
  #       else
  #         1. result: "finished"
  #         2. accuracy: accuracy
  #         3. bucket.name
  #         4. object.name

  # string for detecting training status in checking
  flag.check <- "RUNNING"

  # check input
  mode.type = c("train", "predict", "check")
  mode <- match.arg(mode, mode.type)
  
  if (missing(result.data) || length(result.data) == 0)
    stop("'result.data' should not be empty")

  # handle prediction result
  if (mode == "predict") {
  	if (verbose) cat(as.character(result.data),"\n")
  
    # handle json from prediction API
    # get label as result
    result.final <- result.data$outputLabel
    
    # check if error
    if (is.null(result.final)) {
      return(list(result = "error",
                  error.code = result.data$error$code,
                  error.message = result.data$error$message,
                  error.information = result.data$error$errors))
    }
    
    if (data.type=="text") {
    
    	result.final$multi <- length(result.data$outputMulti)

			result.final$labels <- character(result.final$multi)
			result.final$scores <- double(result.final$multi)

			for (i in 1:result.final$multi) {

				result.final$labels[i] <- result.data$outputMulti[[i]]$label
				result.final$scores[i] <- result.data$outputMulti[[i]]$score
	
			}
			
			result.final$order <- order(result.final$scores,decreasing=TRUE)
			
			result.final$labels <- result.final$labels[result.final$order]
			result.final$scores <- result.final$scores[result.final$order]
    
    }
    
    result.final <- c(result.final$labels,result.final$scores)
    
    return(result.final)
  }
  
  # handle training result
  # the format of json result is:
  # {
  # 	"kind":"prediction#training",
  # 	"id":"training_bucket/training_data",
  # 	"selfLink":"https://www.googleapis.com/prediction/v1.2/URL_of_resource,
	# }
	# prev: {"data" : { "data" : "${mybucket}/${mydata}"} } }
  if (mode == "train") {
    # check error on result
    result.label <- result.data$id
    if (is.null(result.label)) {
      # if error, output the error code and reason in json
      # get error code
      return(list(result = "error",
                  error.code = result.data$error$code,
                  error.message = result.data$error$message,
                  error.debuginfo = result.data$error$errors))
    }
    # split and get info from json
    data.name <- unlist(strsplit(result.label, "/", fixed = TRUE))
    return(list(result = "correct",
                accuracy = "0.0",
                bucket.name = data.name[1],
                object.name = substr( result.label, (regexpr("/",result.label)[1]+1), nchar(result.label) )
           			))  
  }

  # handle checking result
  if (mode == "check") {
    # check error on result
    # get label
    result.label <- result.data$id

    if (is.null(result.label)) {
      # if error, output the error code and reason
      # get error code
      return(list(result = "ERROR",
                  error.code = result.data$error$code,
                  error.message = result.data$error$message,
                  error.information = result.data$error$errors))
    }
    # Check the status of result (notFinish or finished)
    # If it's finished, return location and accuracy.
    # the json format of result from Prediction API is:
    # {"data":{"data":"${mybucket}/${mydata}",
    # "modelinfo":"Training has not completed."}}}
    # or
    # {"data":{"data":"${mybucket}/${mydata}",
    # "modelinfo":"estimated accuracy: 0.74"}}}

    # get information from result
    modelinfo <- result.data$trainingStatus
    data.label <- result.data$id
    if (modelinfo == flag.check) {
      output <- list(result = "RUNNING")
    } else {
      # split and get accuracy and location in Google Storage
      accuracy <- result.data$modelInfo$classificationAccuracy
      model_type <- result.data$modelInfo$modelType
      data.name <- unlist(strsplit(data.label, "/", fixed = TRUE))
      output <- list(result = "DONE",
                     accuracy = accuracy,
                     model_type = model_type,
                     bucket.name = data.name[1],
                     object.name = substr( result.label, (regexpr("/",result.label)[1]+1), nchar(result.label) )
                     )
    }
    return(output)
  }

  stop("mode='", mode, "' was not one of the recognized options")
}
