# corr.R

corr <- function(directory, threshold = 0) {
  # find all locations where the number of complete cases is > threshold
  
  files <- list.files(directory)
  results = data.frame(Date=c(), sulfate=c(), nitrate=c(), ID=c())
  
  # merge them all together
  for (f in files) {
    path = paste(directory, f, sep="/")
    data <- read.csv(path)
    
    # remove incompletes
    good <- complete.cases(data)
    data <- data[good,]
    
    # merge them into everthing.
    results <- rbind(results, data)
  }
  
  # figure out which locations we have.
  locations <- unique(results$ID)
  
  # put results that reach the threshold into this structure.
  newResults = data.frame(Date=c(), sulfate=c(), nitrate=c(), ID=c())
  
  cors = c()
  for (id in locations) {
    data <- subset(results, ID==id)
    if (nrow(data) > threshold) {
      # we care about this.
      newResults <- rbind(newResults, data)
      correlation = cor(data$sulfate, data$nitrate)
      cors <- append(cors, correlation)
    }
  }
  
  cors
  #cor(newResults$sulfate, newResults$nitrate)
}