corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all variables)
  ## required to compute the correlation between nitrate and
  ## sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  ## NOTE: Do not round the result
  
  # Complete cases data field
  completeCases <- complete(directory)

    # Boolean list
  exceedsThreshold <- completeCases[,"nobs"] > threshold
  
  results <- numeric(sum(exceedsThreshold))
  count <- 0
  
  for (i in completeCases$id[exceedsThreshold]) {
    # Do something
    # probably with file$sulfate[!is.na(file$sulfate) & !is.na(file$nitrate)]
    
    # COR over i
    thisFile <- read.csv(file.path(directory,sprintf("%03d.csv", i)))
    count <- count + 1
    results[count] <- cor(thisFile$sulfate[!is.na(thisFile$sulfate) & !is.na(thisFile$nitrate)],
        thisFile$nitrate[!is.na(thisFile$sulfate) & !is.na(thisFile$nitrate)])
  }
  
  results
}