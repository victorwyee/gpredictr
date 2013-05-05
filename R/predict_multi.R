# Copyright 2010 Google Inc. All Rights Reserved.
# Author: markko@google.com (Markko Ko)

PredictMulti <- function(model,
                         testdata,
                         retry.number.of.times = 50,
                         retry.wait.time.in.second = 0.2,
                         request.interval.time.in.second = 1) {
  # Use the testdata of type: data.frame to perform a batch prediction on model
  # Args:
  #   model: model object generated from gpm() with type "prediction.api.model"
  #   testdata: test dataset, each row is one instance,
  #             each column is one feature,
  #             the format of each instance should be the same as
  #             newdata in predict()
  #   retry.number.of.times:
  #           number of times to retry when prediction failed
  #   retry.wait.time.in.second:
  #           how long to wait when retry (in second)
  #   request.interval.time.in.second:
  #           how long to wait when request succeed (in second)
  # Returns:
  #     a list of predicted label for each instance

  # check input
  if (missing(model)) {
    stop("Argument 'model' is required")
  }
  stopifnot(!missing(testdata), is.data.frame(testdata))

  if (!is(model, 'prediction.api.model')) {
    stop("The type of model should be 'prediction.api.model'")
  }
  pred <- c(length = nrow(testdata))
  count <- 1
  number.of.instance <- nrow(testdata)
  type <- (if (is.character(testdata[1, 1])) "text" else "numeric")

  # run through all instances
  while (count <= number.of.instance) {
    retry <- 0
    while (retry < retry.number.of.times) {
      if (type == "numeric") {
        newdata <- c(as.numeric(testdata[count, ]))
      } else {
        newdata <- c(testdata[count, ])
      }
      # perform prediction using Prediction API
      result <- predict(model, newdata = newdata)
      # retry
      if (!is.atomic(result) && result$result == "error") {
        retry <- retry + 1
        cat("retry in instance: ", count,
            ", code: ", result$error.code,
            ", message: ", result$error.message, "\n", sep='')
        Sys.sleep(retry.wait.time.in.second)
      } else {
        pred[count] <- result
        cat(count, ": ", result, "\n", sep='')
        retry <- 0
        break
      }
    }
    Sys.sleep(request.interval.time.in.second)
    count <- count + 1
  }
  return(pred)
}
