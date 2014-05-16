complete <- function(directory, id = 1:332) {
  files = list.files(directory)
  
  # set everything to NA to make it easy to filter later on.
  results = data.frame(id=rep.int(NA, max(id)), nobs=rep.int(NA, max(id)))
  
  # initialize the rows we care about.
  for (r in id) {
    results[r,] = c(r, 0)
  }
  
  for (f in files) {
    path = paste(directory, f, sep="/")
    data <- read.csv(path)
    
    # filter the ids I care about.
    data <- subset(data, ID %in% id)
    
    # increment count for each id.
    for (i in id) {
      # filter for this id
      z <- subset(data, ID == i)
      
      # pull out the complete cases
      good <- complete.cases(z)
      z <- z[good,]
      
      lengthAfter <- nrow(z)
      if (lengthAfter > 0) {
        results[i,] <- c(i, results[i,2] + lengthAfter)
        # need to figure out how to update the data frame.
      }
    }
  }
  
  # filter out the NA
  # this approach doesn't work because it doesn't preserve the order that can
  # be specified in id.
  #good <- complete.cases(results)
  #results <- results[good,]
  #results <- data.frame(id=results$id, nobs=results$nobs)
  
  # create a new frame and copy values into it.
  newResults <- data.frame(id=id, nobs=rep.int(0, length(id)))
  count <- 1
  for (rid in newResults$id) {
    newResults[count, 2] <- results[rid, 2]
    count <- count + 1
  }
  
  newResults

}

# source("http://d396qusza40orc.cloudfront.net/rprog%2Fscripts%2Fsubmitscript1.R")