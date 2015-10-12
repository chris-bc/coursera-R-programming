best <- function(state, outcome) {
  ## Read outcome data
  outcomeData <- read.csv("outcome-of-care-measures.csv",stringsAsFactors = FALSE, na.strings='Not Available')
  ## Convert the mortality columns to numerics to simplify later processing
  outcomeData[,11]<-as.numeric(outcomeData[,11])
  outcomeData[,17]<-as.numeric(outcomeData[,17])
  outcomeData[,23]<-as.numeric(outcomeData[,23])
  
  ## Check parameters
  if (sum(outcomeData$State == state) == 0) {
    # State doesn't exist in the dataset
    stop("invalid state")
  }
  
  validOutcomes <- list("heart failure"=17, "heart attack"=11, "pneumonia"=23)
  outcomeCol <- unlist(validOutcomes[outcome], use.names = FALSE)
  if (is.null(outcomeCol)) {
    stop("invalid outcome")
  }
  
  ## Inputs are valid, find the hospital matching inputs
  sort(outcomeData$Hospital.Name[
    !is.na(
      outcomeData[,outcomeCol]) & outcomeData$State==state & 
          outcomeData[,outcomeCol] == min(outcomeData[,outcomeCol][outcomeData$State==state],na.rm=TRUE)]
  )[1]
}


## Working notes

##WI, heart failure returns 2
