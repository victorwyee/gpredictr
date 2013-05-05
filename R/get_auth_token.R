# Copyright 2010 Google Inc. All Rights Reserved.
# Author: markko@google.com (Markko Ko)

PredictionApiUtilGetAuth <- function(verbose = FALSE) {
  # Returns authentication string based on:
  #   1. ~/.auth-token file
  #   2. retrieving authentication from ClientLogin and save to
  #      ~/.auth-token by prompting user to input email and passwd
  #
  # Args:
  #   verbose: If TRUE, print out all detail for debugging. Default is FALSE.
  # Returns:
  #   auth: string of auth-token

  # get auth from file
  file.auth <- "~/.auth-token"
  if (file.exists(file.auth)) {
    auth <- scan(file = file.auth, what = "character", quiet = TRUE)
    if (length(auth) != 0)
      return(auth)
  }
  
  # get auth by making a request to Google ClientLogin
  cat("Requesting Authentication from Google ClientLogin for user: ",
      myEmail, "\n", sep='')
  result.auth <- PredictionApiUtilRequestAuth(email     = myEmail,
                                              password  = myPassword,
                                              verbose   = verbose)
  temp <- unlist(strsplit(result.auth, "Auth=", fixed = TRUE))[2]
  auth <- unlist(strsplit(temp, "\n"))[1]
  # in Windows, the file.auth will store in
  #   "C:\Users\username\Documents\.auth-token"
  # in Linux, the file.auth will reside in
  #   "~/.auth-token"
  write(auth, file = file.auth)
  Sys.chmod(file.auth, mode = "0600")
  cat("Auth-token file was stored in ",
      file.auth, "\n", sep='')
  return(auth)
}

PredictionApiUtilRequestAuth <- function(email,
                                         password,
                                         verbose = FALSE) {
  # Makes a request to ClientLogin to get auth-token
  #
  # Args:
  #   email: user's email account for accessing Google Prediction API
  #   passwd: user's password for this email account
  # Returns:
  #   auth: string of auth-token

  # prepare data to send to Google ClientLogin
  data.tosend <-
    sprintf(paste("accountType=HOSTED_OR_GOOGLE&",
                  "Email=%s&",
                  "source=R%%20client%%20library&",
                  "service=predictionapi&",
                  "Passwd=%s", sep=''),
            email,
            PredictionApiUtilUrlencode(password))
  if (verbose)
    cat("Data: ", data.tosend, "\n", sep='')
  # connect to ClientLogin and get result.auth
  result.auth <- PredictionApiConnectHandler(connect.type = "auth",
                                             bucket.name  = "",
                                             object.name  = "",
                                             data.tosend  = data.tosend,
                                             verbose      = verbose)
  if (!result.auth$succeed.connect)
    stop("Connection to API failed, stop.\n")

  return(result.auth$data)
}

PredictionApiUtilUrlencode <- function(url) {
  # Since the URLencode() in utils package won't encode some special characters
  # that are required to be encoded for inputing into Prediction API,
  # this modified function can encode those "special characters" and
  # "reserved characters" correctly.
  # the characters that will be encoded include reserved characters
  # and some special characters in RFC 1738
  # "charaters": ('$', '&', '+', ',', '/', ':', ';', '=', '?', '@'
  #               '!', ''', '(', ')', '*', '#', '[', ']')
  # see also:
  #   RFC 3986: http://tools.ietf.org/html/rfc3986#page-12
  #   RFC 1738: http://www.rfc-editor.org/rfc/rfc1738.txt
  # Args:
  #   url: the string to be encoded
  # Returns:
  #   encoded string
  OK <- paste("[^-ABCDEFGHIJKLMNOPQRSTUVWXYZ",
              "abcdefghijklmnopqrstuvwxyz0123456789_.",
              "]", sep = "")
  x <- strsplit(url, "")[[1L]]
  z <- grep(OK, x)
  if (length(z)) {
    y <- sapply(x[z],
                function(x) {
                  paste("%", as.character(charToRaw(x)),
                        sep = "", collapse = "")
                })
    x[z] <- y
  }
  return(paste(x, collapse = ""))
}
