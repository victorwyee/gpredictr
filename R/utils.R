# Copyright 2010 Google Inc. All Rights Reserved.
# Author: markko@google.com (Markko Ko)
# Modified by w.victoryee@gmail.com (Victor Yee), 2013-05-04

### Builds POST data to send in an 'insert' API call
# Used in TrainRunner()
GetPredictionSet <- function(unique.identifier, bucket, object) {
  paste0('{"id" : "', unique.identifier, '",',
         ' "storageDataLocation" : "', bucket, '/', object, '"}')
}

# Used for APIv0.12 but no longer needed for APIv0.15
HexSlash <- function(object) {
  gsub("/", "%2F", object)
}