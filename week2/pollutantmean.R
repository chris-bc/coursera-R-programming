pollutantmean <- function(directory, pollutant, id=1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate
  ## the mean; either "sulfate" or "nitrate"
  
  ## 'id' is an integer vector indicating the monitor id values
  ## to be used
  
  ## Return the mean of the pollutant across all monitors
  ## listed in the 'id' vector (ignoring NA values)
  ## NOTE: Do not round the result
  
  sum<-0
  count<-0
  means<-numeric(max(id))
  
  for (i in id) {
    filename <- file.path(directory,sprintf("%03d.csv",i))
    data <- read.csv(filename)
    sum<-sum+sum(data[,pollutant][!is.na(data[,pollutant])])
    count<-count+sum(!is.na(data[,pollutant]))
  }

  sum/count
}