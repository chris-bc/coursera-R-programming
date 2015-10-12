rankhospital <- function(state, outcome, num = "best") {
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
  
  if (!is.numeric(num) & num != "best" & num != "worst") {
    stop("invalid ranking")
  }
  
  ## Inputs are valid, Subset outcomeData
  smallSet <- outcomeData[outcomeData$State==state, c(outcomeCol, 2, 7)]
  ## Order data for selected state
  smallSet <- smallSet[do.call(order, c(smallSet,na.last=NA)),]
  
  ## Convert character positionals to numeric
  if (num == "best") {
    num <- 1
  } else if (num == "worst") {
    num <- nrow(smallSet)
  }
  
  ## Find the result
  smallSet[num, "Hospital.Name"]
  
}
