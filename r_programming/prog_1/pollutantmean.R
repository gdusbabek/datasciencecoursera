# setwd("~/codes/github/datasciencecoursera/r_programming/prog_1")
# source('~/codes/github/datasciencecoursera/r_programming/prog_1/pollutantmean.R')

# direcotry is a char vector
# pollutant is a char vector of "sulfate" or "nitrate"
# id is an int vector indicating which monitors
# return mean of the pollutant across all monitors in the id vector
pollutantmean <- function(directory, pollutant, id = 1:332) {
  # read in all the files
  files = list.files(directory)
  max <- 1000000
  count <- 0
  
  sum <- 0
  sumCount <- 0
  
  results <- vector()
  
  for (f in list.files(directory)) {
    path = paste(directory, f, sep="/")
    # read the whole file.
    data <- read.csv(path)
    
    # pull in the column for the right ids.
    data <- subset(data, select=c("ID", pollutant), ID %in% id)
    
    # remove all the NA
    good <- complete.cases(data)
    data <- data[good,]
    
    sum <- sum + sum(data[[pollutant]])
    for (x in data[[pollutant]]) {
      results[length(results)+1] = x
      #print(toString(c(f, x)))
    }
    sumCount <- sumCount + length(data[[pollutant]])
    
    count <- count + 1
    if (count >= max) {
      break
    }
  }
  
  #print(sum / sumCount)
  #print(mean(results))
  mean(results)
}