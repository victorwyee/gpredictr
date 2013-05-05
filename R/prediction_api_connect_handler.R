# Copyright 2010 Google Inc. All Rights Reserved.
# Author: markko@google.com (Markko Ko)

PredictionApiConnectHandler <- function(connect.type,
                                        bucket.name,
                                        object.name,
                                        data.tosend,
                                        verbose = FALSE,
                                        retry = FALSE) {
  # Handles the connection to Google Prediction API and return the result
  # with status information. This function supports four connection types:
  # "train", "predict", "check", "auth"
  # Args:
  #   connect.type: "train", "predict", "check", "auth"
  #   bucket.name: the name of bucket in Google Storage to be processed
  #   object.name: the name of object in Google Storage to be processed
  #   data.tosend: the data you want to send to Prediction API
  #   verbose: If TRUE, prints all detail for debugging. Default is FALSE.
  # Returns:
  #   the json format result string from Prediction API

  # get auth
  if (connect.type != "auth")
    auth <- PredictionApiUtilGetAuth(verbose = verbose) # @ prediction_api_get_auth_token.R

  # when connection fails, the number of times to retry
  retry.number.of.times <- ifelse(retry==TRUE,3,1)
  # when connection fails, how long to wait to retry (second)
  retry.sleep.time.in.seconds <- 0.5
  # check input
  connect.type.all = c("train", "predict", "check", "auth")
  if (!is.element(connect.type, connect.type.all))
    stop("Connect.type must be either train, predict, auth or check")
  if (missing(data.tosend))
    stop("Must have 'data.tosend'")

  if (connect.type == "train") {
    # prepare url for connection
    url <- paste("https://www.googleapis.com/prediction/v1.2/training?key=",myAPIkey,sep="")

    # set the header
    header.field <-
      c('Authorization' = paste("GoogleLogin auth=", auth, sep=''),
        'User-Agent' = paste('R client library for the Google Prediction API',
          'v', kVersion, sep=''),
          'Content-Type' = "application/json")

  } else if (connect.type == "predict") {
    url <- paste("https://www.googleapis.com/prediction/v1.2/training/",
                 bucket.name, "%2F", HexSlash(object.name), "/predict?key=",myAPIkey, sep='')

    # set the header
    header.field <-
      c('Authorization' = paste("GoogleLogin auth=", auth, sep=''),
        'User-Agent' = paste('R client library for the Google Prediction API',
          'v', kVersion, sep=''),
        'Content-Type' = "application/json")

  } else if (connect.type == "check") {
    # prepare url
    url <- paste("https://www.googleapis.com/prediction/v1.2/training/",
                 bucket.name, "%2F", HexSlash(object.name),"?key=",myAPIkey, sep='')
    header.field <-
      c('Authorization' = paste("GoogleLogin auth=", auth, sep=''),
        'User-Agent' = paste('R client library for the Google Prediction API',
          'v', kVersion, sep=''))

  } else if (connect.type == "auth") {
    # prepare url
    url <- "https://www.google.com/accounts/ClientLogin"
    header.field <-
      c('Content-Type' = "application/x-www-form-urlencoded",
        'User-Agent' = paste('R client library for the Google Prediction API',
          'v', kVersion, sep=''))

  }

  # send query to API
  # set options
  curl <- getCurlHandle()
  curlSetOpt(.opts = list(httpheader = header.field, verbose = verbose),
             curl = curl)

  # set callback object
  t <- basicTextGatherer()
  d <- debugGatherer()
  h <- basicHeaderGatherer()

  # connect to API, if fails, try retry.number.of.times times
  
  if (verbose) {
  	cat("Request to be send:\n")
  	cat("TYPE=",connect.type,"\n")
  	cat("URL=",url,"\n")
  	cat("POST=",data.tosend,"\n")
  	cat("HEAD=",header.field,"\n")  
  }	
  
  retry <- 0
  while (retry < retry.number.of.times) {
  		if (verbose)
  			cat("Connection attempt no.:",(retry+1),"\n")
  
    # Perform query
    if (connect.type == "check") {
      curlSetOpt(.opts = list(httpget = 1), curl = curl)
      body = curlPerform(url = url,
                         curl = curl,
                         writefunction = t$update,
                         headerfunction = h$update)
    } else {
      body = curlPerform(url = url,
                         curl = curl,
                         postfields = data.tosend,
                         writefunction = t$update,
                         headerfunction = h$update)
    }
    
    if (verbose) {
      cat("Returned values:\n")
    	cat("T=",t$value())
      cat("H=",h$value())
      cat("D=",d$value())
      }

    # prepare output
    output <- list(succeed.connect = TRUE,
                   data = t$value(),
                   status = h$value()[['status']],
                   status.message = h$value()[['statusMessage']])
    # check http status
    httpstatus <- as.numeric(output$status)
    if (httpstatus != 200)
      output$succeed.connect <- FALSE
    if (output$succeed.connect)
      break

    Sys.sleep(retry.sleep.time.in.seconds)
    retry <- retry + 1
    warning("Connection error: status:", output$status,
            ", status message:", output$status.message,"\n")           
  }
     
  return(output)
}
