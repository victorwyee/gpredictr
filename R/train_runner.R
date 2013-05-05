# Copyright 2010 Google Inc. All Rights Reserved.
# Author: markko@google.com (Markko Ko)

# set the interval time (sec) to check if upload is finished
#
# ---
#
# modified for Google API v1.2 and extended by Maciej Janiec (mjaniec@gmail.com), 2011-05-30

kWaitTime <- 5

# adds "PredictionApiModel" class to a result
# used in PredictionApiTrainRunner
# may also be used with PredictionApiCheckTrain or PredictionApiCheckTrainingStatus
WrapModel <- function(result) {

	class(result) <- "PredictionApiModel"
	
	return(result)

}

PredictionApiTrainRunner <- function(data.type,
                                     data,
                                     remote.file,
                                     local.file,
                                     verbose = FALSE,
                                     retry = TRUE,
                                     tillDone = FALSE) {
  # Trains a model on data you provided through Google Prediction API.
  # It accepts three types of data:
  # 1. file:
  #     User inputs a csv file name, this function performs uploading
  #     and training to a model
  #     required arguments: data="file.csv", remote.file
  #     optional arguments: verbose
  # 2. remote:
  #     User inputs an object location in Google Storage, this function
  #     performs training to a model directly
  #     required arguments: data="bucket.name/object.name"
  #     optional arguments: local.file, verbose
  #
  # Args:
  #   data.type: the type of input data
  #     1. file
  #     2. remote
  #   data: Accept three types: file, remote
  #     e.g. "wdbcData.csv", "gs://bucket.name/wdbcData"
  #   remote.file: the location of object to be trained in Google Storage
  #     e.g. "bucket.name/wdbcData", or "gs://bucket.name/wdbcData"
  #   local.file: the location of local csv file that will be upload
  #               to Google Storage.
  #   verbose: If TRUE, print out all detail for debugging. Default is FALSE.
  #		tillDone - repeat checking for training results till training is completed
  # Returns:
  #   model object with class type: "PredictionApiModel"

  # set the default json string for training to Prediction API
  # data.tosend <- "{data:{}}"

  # parse remote.file to bucket.name and object.name
  parse.result <- PredictionApiTrainParseRemoteFile(remote.file)
  bucket.name <- parse.result$bucket.name
  object.name <- parse.result$object.name
  
  # POST set
  data.tosend <- GetPredictionSet(bucket.name,object.name)

	cat("Data type:",data.type,"\n")

  # user input dataframe or file:
  # upload the .csv file to Google Storage
  if (data.type == 'file') {
    upload.result <- PredictionApiUtilUpload(file.name = local.file,
                                             bucket.name = bucket.name,
                                             object.name = object.name,
                                             verbose = verbose,
                                             run.in.background = TRUE)
    if (upload.result$result != "found" && upload.result$result != "correct") {
      cat("Error in upload: see the return value for more information\n")
      return(upload.result)
    }
  }

  # connect to Prediction API @ prediction_api_connect_handler.R
  result.conn <- PredictionApiConnectHandler(connect.type = "train",
                                             bucket.name = bucket.name,
                                             object.name = object.name,
                                             data.tosend = data.tosend,
                                             verbose = verbose,
                                             retry = retry)
  if (!result.conn$succeed.connect)
    stop("Connection to Prediction API failed, stop.")

	if (verbose)
		cat("Training request completed.\n")

  # this function translates the json format result into R object @ prediction_api_json_parser.R
  result.data <- PredictionApiJsonToData(result.conn$data, verbose)

	cat("JSON=",unlist(result.data),"\n")

  # this function handles results from Google Prediction API
  output <- PredictionApiResultHandler(result.data = result.data,
                                       mode = "train",
                                       verbose = verbose)
                                       
  cat("RESULT HANDLER=",unlist(output),"\n")                                     
                                       
  # handle output
  if (output$result == "error") {
    cat("Error on retrieving result from result.handler:",
        output$error.message, "\n",
        "check return value for more information", "\n")
    return(output)
  }
  cat("The request for training has sent, ",
      "now trying to check if training is completed\n", sep = '')

  # check training status every wait.time seconds
  timer <- Sys.time();
  while (TRUE) {
    check.result <- PredictionApiCheckTrain(bucket.name = bucket.name,
                                            object.name = HexSlash(object.name),
                                            data.tosend = "",
                                            verbose = verbose)
    if (check.result$result == "ERROR") {
      cat("Error: ", check.result$error.message,
          "\ncheck return value for more information\n")
      return(check.result)
      
    } else if (check.result$result == "RUNNING") {
      # print status for each run
      cat(paste("Training on ", bucket.name, "/", object.name,
                ": time:",
                sprintf("%.2f", difftime(Sys.time(), timer, units="sec")),
                " seconds", "\n", sep =''))
                
    } else if (check.result$result == "DONE") {
      cat(paste("Training is complete, time: ",
                sprintf("%.2f", difftime(Sys.time(), timer, units="sec")),
                " seconds\n", sep=''))
      # set result object from resultHandler as 'PredictionApiModel'     
      return(WrapModel(check.result))
    
    } else {
      stop("In error field: result from resultHandler()\n")
    }
    
    if (tillDone) { Sys.sleep(kWaitTime) }
    				else 	{ break }
    
  }
  return(output)
}

PredictionApiTrainParseRemoteFile <- function(remote.file) {
  # Checks the format of remote.file and parses the remote.file string
  # into bucket.name and object.name
  #
  # Args:
  #   remote.file: the location of data in Google Storage.
  #     the format checking should accept both:
  #       1. gs://bucket.name/object.name
  #       2. bucket.name/object.name{/...}
  # Returns:
  #   a list including:
  #     1. well.formated: boolean, TRUE means the format is correct, FALSE else
  #     2. bucket.name: string, the bucket.name in the remote.file
  #     3. object.name: string, the object.name in the remote.file

  temp <- unlist(strsplit(remote.file, "/"))
  
  # check "gs://bucket.name/object.name" format
  if (length(grep("^gs://[^/]+/.+$", remote.file)) != 0) { # prev.:: gs://[^/]+/[^/]+$
    composite_object <- ""
	  for (i in 4:length(temp)) {
			composite_object <- paste(composite_object,temp[i],sep="/")
	  }
  	composite_object <- sub("/","",composite_object)
  
    return(list(well.formed = TRUE,
                bucket.name = temp[3],
                object.name = composite_object))
  }
  
  # check "bucket.name/object.name" format
  if (length(grep("^[^/]+/.+$", remote.file)) != 0) { # prev.: [^/]+/[^/]+$
  	composite_object <- ""
	  for (i in 2:length(temp)) {
			composite_object <- paste(composite_object,temp[i],sep="/")
	  }
  	composite_object <- sub("/","",composite_object)
  
    return(list(well.formed = TRUE,
                bucket.name = temp[1],
                object.name = composite_object))
  }
  # format not match
  return(list(well.formed = FALSE, bucket.name = "", object.name = ""))
}
