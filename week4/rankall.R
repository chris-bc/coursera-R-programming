rankall <- function(outcome, num = "best") {
  ## Read outcome data
  outcomeData <- read.csv("outcome-of-care-measures.csv",stringsAsFactors = FALSE, na.strings='Not Available')
  ## Convert the mortality columns to numerics to simplify later processing
  outcomeData[,11]<-as.numeric(outcomeData[,11])
  outcomeData[,17]<-as.numeric(outcomeData[,17])
  outcomeData[,23]<-as.numeric(outcomeData[,23])
  
  ## Check parameters
  validOutcomes <- list("heart failure"=17, "heart attack"=11, "pneumonia"=23)
  outcomeCol <- unlist(validOutcomes[outcome], use.names = FALSE)
  if (is.null(outcomeCol)) {
    stop("invalid outcome")
  }
  
  if (!is.numeric(num) & num != "best" & num != "worst") {
    stop("invalid ranking")
  }
  if (num == "best") {
    num <- 1
  }
  
  ## Inputs are valid. Get a list of states
  allStates <- sort(unique(outcomeData$State))
  
  # Get our output ready
  retVal <- list()
  
  ##Subset outcomeData
  #First grab the selected column
  medSet <- outcomeData[, c(outcomeCol, 2, 7)]
  
  worst <- (num == "worst")
  
  # Then loop through all states
  for (thisState in allStates) {
    smallSet <- medSet[medSet$State==thisState,]
    ## Order data for selected state
    smallSet <- smallSet[do.call(order, c(smallSet,na.last=NA)),]
  
    ## Convert character positionals to numeric
    if (worst) {
      num <- nrow(smallSet)
    }
  
    ## Find the result
    retVal[thisState] <- smallSet[num, "Hospital.Name"]
  }
  
  ## Assemble the output
  data.frame(hospital=unlist(retVal), state=allStates)
}
