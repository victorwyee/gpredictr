# Copyright 2010 Google Inc. All Rights Reserved.
# Author: markko@google.com (Markko Ko)
# Modified by w.victoryee@gmail.com (Victor Yee), 2013-05-04

ConnectionHandler <- function(connect.type,
                              unique.identifier,
                              data.tosend,
                              verbose = FALSE,
                              max.retries = 1) {
  
  # Get auth
  if (connect.type != "auth")
    auth <- GetAuthToken(verbose = verbose)
  
  # Check input
  connect.type.all = c("insert", "get", "predict", "list", "auth")
  if (!is.element(connect.type, connect.type.all))
    stop("Connect.type must be either insert, get, list, predict, or auth")
  if (is.element(connect.type, c("insert", "predict")) && missing(data.tosend))
    stop("Must have 'data.tosend'")
  
  # Set common query parameters
  prediction.api.url <- "https://www.googleapis.com/prediction/v1.5/trainedmodels"
  user.agent.string  <- "R client library for the Google Prediction APIv0.15"
  header.field.get   <- c('Authorization' = paste0("GoogleLogin auth=", auth),
                          'User-Agent'    = user.agent.string)
  header.field.json  <- c('Authorization' = paste0("GoogleLogin auth=", auth),
                          'User-Agent'    = user.agent.string,
                          'Content-Type' = "application/json")
  
  # Construct API query
  if (connect.type == "insert") {
    url          <- paste0(prediction.api.url, "?key=", my.api.key)
    header.field <- header.field.json
  } else if (connect.type == "get") {
    url          <- paste0(prediction.api.url, "/", unique.identifier,"?key=", my.api.key)
    header.field <- header.field.get
  } else if (connect.type == "predict") {
    url          <- paste0(prediction.api.url, "/", unique.identifier,"/predict?key=", my.api.key)
    header.field <- header.field.json
  } else if (connect.type == "list") {
    url          <- paste0(prediction.api.url, "/list?key=", my.api.key)
    user.agent   <- user.agent.string
    header.field <- header.field.get
  } else if (connect.type == "auth") {
    url          <- "https://www.google.com/accounts/ClientLogin"
    header.field <- c('Content-Type'  = "application/x-www-form-urlencoded",
                      'User-Agent'    = user.agent.string)
  }
  
  # Set request to API
  curl <- getCurlHandle()
  curlSetOpt(.opts = list(httpheader = header.field,
                          verbose = verbose),
             curl = curl)
  
  # Set callback object
  t <- basicTextGatherer()
  d <- debugGatherer()
  h <- basicHeaderGatherer()
  
  # Connect to API
  if (verbose) {
    cat("Request to be send:\n")
    cat("TYPE=", connect.type, "\n")
    cat("URL=" , url         , "\n")
    cat("POST=", data.tosend , "\n")
    cat("HEAD=", header.field, "\n")    
  }
  
  retry <- 0
  while (retry < max.retries) {
    if (verbose)
      cat("Connection attempt no.:", (retry + 1), "\n")
    
    # Perform query
    if (is.element(connect.type, c("get", "list"))) {
      curlSetOpt(.opts = list(httpget = 1), curl = curl)
      body <- curlPerform(url            = url,
                          curl           = curl,
                          writefunction  = t$update,
                          headerfunction = h$update)
    } else {
      body <- curlPerform(url            = url,
                          curl           = curl,
                          postfields     = data.tosend,
                          writefunction  = t$update,
                          headerfunction = h$update)
    }
    
    if (verbose) {
      cat("Returned values:\n")
      cat("T=", t$value())
      cat("H=", h$value())
      cat("D=", d$value())  
    }
    
    # Prepare output
    output <- list(succeed.connect = TRUE,
                   data            = t$value(),
                   status          = h$value()[['status']],
                   status.message  = h$value()[['statusMessage']])
    
    # Check http status
    httpstatus <- as.numeric(output$status)
    if (httpstatus != 200) { output$succeed.connect <- FALSE }
    if (output$succeed.connect) { break }
    
    retry.sleep.time.in.seconds <- 0.5
    Sys.sleep(retry.sleep.time.in.seconds)
    retry <- retry + 1
    warning("Connection error: status:", output$status,
            ", status message:", output$status.message,"\n")  
  }
  
  return(output)
}