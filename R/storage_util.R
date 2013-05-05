# Copyright 2010 Google Inc. All Rights Reserved.
# Author: markko@google.com (Markko Ko)
#
# ---
#
# modified for Google API v1.2 and extended by Maciej Janiec (mjaniec@gmail.com), 2011-05-30

myPython 			<- ""
myGSUtilPath 	<- ""

PythonCommand <- function(command) {

	return(paste(myPython," ",myGSUtilPath,command,sep=""))

}

PredictionApiUtilGsutil <- function(command, verbose = FALSE,
                                    run.in.background = FALSE) {
  # A thin interface for R to access gsutil using system call
  # Note:
  # TODO(markko): Because for the R Windows version, the implementation of this
  #               function is using gsutil which requires Cygwin. So if
  #               we can use RCurl to make http request instead of using gsutil
  #               , then this package can also work in Windows without Cygwin.
  # Args:
  #   command: the command that will be executed
  #   verbose: If TRUE, prints all detail for debugging. Default is FALSE.
  #   run.in.background: If TRUE, then put the procress into background
  #                     else, the procress will be in the foreground
  # Returns:
  #   if succeed: a list of information includes
  #     1. result: "correct"
  #     2. value: message from Google Storage
  #   else: a list of error message including:
  #     1. result: "error"
  #     2. status: status code from Google Storage API
  #     3. code: error code from Google Storage API
  #     4. reason: reason message from Google Storage API

  check.error.pattern.string <- "^Failure"
  auth.file <- "~/.boto"
  if (!file.exists(auth.file))
    stop(".boto file missing")
    # PredictionApiUtilgetGsutilAuth(auth.file)

  # check command
  if (missing(command) || nchar(command) == 0)
    stop("Please specify command\n")

  # run command
  # command <- paste(command, " 2>&1", sep='') ???
  
  command <- PythonCommand(command)
  
  if (verbose)
    cat("Command: ", command, "\n")
  result <- (if (run.in.background) system(command = command, wait = FALSE)
             else system(command = command, intern = TRUE))

  if (verbose)
    cat("Result: ", result, "\n")
  # handle error result:
  # the format of error message:
  # "Failure: status=404, code=NoSuchBucket, reason=Not Found."
  if (length(grep(check.error.pattern.string, result)) != 0 ) {
    error <- unlist(strsplit(substr(result, nchar(check.error.pattern.string)+1,
                                    nchar(result)-1),
                             ",", fixed = TRUE))
    error.code <- strsplit(error, "=")
    error.result <- list(result = "error",
                         status = error.code[[1]][2],
                         code = error.code[[2]][2],
                         reason = error.code[[3]][2])
    return(error.result)
  }
  output <- list(result = "correct",
                 value = result)
  if (verbose)
    cat(paste("Output from gsutil: ", output, "\n"), sep='')
  return(output)
}

# CURRENTLY UNUSED; .BOTO MUST BE CREATED MANUALLY WITH GSUTIL CONFIG
PredictionApiUtilgetGsutilAuth <- function(auth.file) {
  # Prompts user to input key and secret for gsutil and use gsutil to
  # establish the authentication
  # Args:
  #   auth.file: the default location of gsutil authentication file
  # Returns:
  key <- ""
  secret <- ""
  if (nchar(key) == 0 || nchar(secret) == 0)
    stop("Google Storage Access Key or Secret missing.")

  # write into temp file
  file.temp <- tempfile()
  write(paste(key, "\n", secret, sep=''), file = file.temp)
  command <- PythonCommand(paste("gsutil < ", file.temp, sep=''))
  result <- system(command, intern = TRUE)
  if (!file.remove(file.temp))
    cat("Remove temp file error: ", file.temp, "\n", sep='')
}

PredictionApiUtilLs <- function(bucket.name = "",
                                object.name = "",
                                verbose     = FALSE) {
  # Lists all buckets you have if object.name and bucket.name
  # are not provided or list all objects you have if bucket.name is provided.
  # Args:
  #   bucket.name: the bucket name of data location in Google Storage
  #   object.name: the object name of data location in Google Storage
  #   verbose: If TRUE, print out all detail for debugging. Default is FALSE.
  # Returns:
  #   a list of your bucket(if you do not specify bucket.name),
  #   a list of your object(if you specify bucket.name but not object.name)
  #   object you specified(if you specify both bucket.name and object.name)
  command <- ""
  if (nchar(bucket.name) != 0) {
    if (nchar(object.name) != 0) {
      command <- paste("gsutil ls gs://", bucket.name, "/",
                        object.name, sep = '')
    } else {
      command <- paste("gsutil ls gs://", bucket.name, sep = '')
    }
  } else {
    command <- paste("gsutil ls", sep = '')
  }
  ## cat(paste("command: ", command))
  result <- PredictionApiUtilGsutil(command = command, verbose = verbose)
  return(result)
}

PredictionApiUtilRm <- function(bucket.name,
                                object.name,
                                verbose = FALSE) {
  # Removes the given object in Google Storage
  # Args:
  #   bucket.name: the bucket name of data location in Google Storage
  #   object.name: the object name of data location in Google Storage
  #   verbose: If TRUE, print out all detail for debugging. Default is FALSE.
  # Returns:
  #   the result from gsutil

  command <- paste("gsutil rm gs://", bucket.name, "/",
                   object.name, sep='')
  if (verbose)
    cat("Command:", command, "\n")
  result <- PredictionApiUtilGsutil(command, verbose = verbose)
  return(result)
}

PredictionApiUtilUpload <- function(file.name,
                                    bucket.name,
                                    object.name,
                                    verbose           = FALSE,
                                    run.in.background = FALSE) {
  # Uploads file.name to gs://bucket.name/object.name
  # if you set run.in.background as TRUE,
  # this function checks the upload procress for every wait.time seconds
  #
  # Args:
  #   file.name: the file you want to upload
  #   bucket.name: the bucket name you want to operate on
  #   object.name: the object name you want to operate on
  #   verbose: If TRUE, print out all details for debugging. Default is FALSE.
  #   run.in.background: TRUE means the upload process will be in background
  #                      and user don't need to wait until it finished
  # Returns:
  #   if run.in.background is TRUE, the return value is the same as
  #   PredictionApiDoesObjectExist()
  #    error:    {result="error", value=error.code}
  #    found:    {result="found", value=bucket.name/object.name}
  #    notFound: {result="notFound", value=message}
  #   if run.in.background is FALSE, the return value is the same as
  #   PredictionApiUtilLs()

  # check input
  if (!file.exists(file.name))
    stop("No file to upload: ", file.name)

  # make a bucket if not existing
  checkBucket <- PredictionApiUtilLs(bucket.name)
  if (checkBucket$result == "error" &&
      checkBucket$code == "NoSuchBucket") {
    result <- PredictionApiUtilMakeBucket(bucket.name, verbose)
    if (result$result != "correct") {
      cat("Error in creating bucket: ", bucket.name, ", see return value\n")
      return(result)
    }
  }
  check <- PredictionApiDoesObjectExist(bucket.name = bucket.name,
                                        object.name = object.name,
                                        verbose     = verbose)
  
  if (verbose) cat("Object status:",check$result,"\n")                                        
                                        
  if (check$result == "error") {
    warning("Error, see return value for more information\n")
    return(check)
  } else if (check$result == "found") {
    cat("Already have this object in Google Storage: ",
        bucket.name, "/", object.name, "\noverwrite?(y/others)", sep='')
    ans <- scan(file="stdin", what="character", nlines=1, quiet=TRUE)
    if (ans != "y") {
      cat("No overwrite, cancel!\n")
      return(check)
    }
    result <- PredictionApiUtilRm(bucket.name, object.name, verbose)
    if (result$result == "error") {
      cat("Error while removing ", bucket.name, "/",
          object.name, " see return value\n")
      return(result)
    }
  }

  command <- paste("gsutil cp ", file.name, " gs://",
                   bucket.name, "/", object.name, sep = '')
  # run command
  result <- PredictionApiUtilGsutil(command,
                                    verbose = verbose,
                                    run.in.background = run.in.background)

  # iteratively prompt user upload time
  if (run.in.background) {
    timer <- Sys.time()
    check.passed <- FALSE
    while (!check.passed) {
      check <- PredictionApiDoesObjectExist(bucket.name = bucket.name,
                                            object.name = object.name,
                                            verbose     = verbose)                                       
                                           
      if (check$result == "error") {
        warning("Error, see return value for more information\n")
        return(check)
      } else if (check$result == "found") {
        check.passed = TRUE
      } else {
        cat ("Upload time...",
             difftime(Sys.time(), timer, units="sec"),
             "seconds\n")
        Sys.sleep(kWaitTime)
      }
    }
    cat("Upload finished\n")
    return(check)
  } else {
  # handle result
    if (result$result == "correct") {
      temp <- PredictionApiUtilLs(bucket.name = bucket.name,
                                  object.name = object.name,
                                  verbose     = verbose)
      result$value <- temp$value
    }
    return(result)
  }
}

PredictionApiDoesObjectExist <- function(bucket.name,
                                         object.name,
                                         verbose = FALSE) {
  # Checks if the given object exists in Google Storage
  # Args:
  #   bucket.name: the bucket name of data location in Google Storage
  #   object.name: the object name of data location in Google Storage
  #   verbose: If TRUE, print out all detail for debugging. Default is FALSE.
  # Returns:
  #   error:    {result="error", value=error.code}
  #   found:    {result="found", value=bucket.name/object.name}
  #   notFound: {result="notFound", value=message}
  
  check.no.object.string <- "matches no objects"
  check.invalid.uri <- "URI is invalid"
  
  check <- PredictionApiUtilLs(bucket.name    = bucket.name,
                               object.name    = object.name,
                               verbose        = verbose)
  if (check$result == "error")
    return(check)

  if (verbose)
    cat("CheckObject: ", check$value, "\n")

  check$result <- (if ( (length(grep(check.no.object.string, check$value)) == 0) &&
  											(length(grep(check.invalid.uri, check$value)) == 0) )
                   "found" else "notFound")
  return(check)
}

PredictionApiUtilMakeBucket <- function(bucket.name, verbose) {
  # Makes a bucket in Googel Storage
  # Args:
  #   bucket.name: the bucket name you want to create
  #   verbose: If TRUE, print out all detail for debugging. Default is FALSE.
  # Returns:
  #   return NULL if succeed or error object the same as
  #   PredictionApiUtilGsutil()

  command = paste("gsutil mb gs://", bucket.name, sep='')
  if (verbose)
    cat(command)
  result <- PredictionApiUtilGsutil(command, verbose)
  return(result)
}
