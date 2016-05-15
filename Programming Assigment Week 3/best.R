best <- function(state, outcome) {
  ## Read outcome data
  df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
  ## Check that state and outcome are valid
  listStates = unique(df$State)
  stateInList = sum(listStates == state)
  
  correctOutcome = ((outcome=="heart attack") || (outcome=="pneumonia") || (outcome=="heart failure"))
  
  if (stateInList == 0) {
    stop("invalid state")
  } else if ( ! correctOutcome) {
    stop("invalid outcome")
  }   
  
  
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  numColName = 2
  #Mortality rate for heart attack
  if (outcome == "heart attack"){ 
    numColOutcome = 11
  }else if (outcome == "pneumonia") {
    numColOutcome = 23
  }else if (outcome == "heart failure") {
    numColOutcome = 17
  }
  df[, numColOutcome] <- as.numeric(df[, numColOutcome])
  dfSplitted <- split(df,df$State)
  stateSelected = dfSplitted[[state]]
  min = min(stateSelected[,numColOutcome],na.rm=TRUE)
  allHospitals = stateSelected[stateSelected[,numColOutcome]==min,numColName] 
  Hospital <- sort(allHospitals[!is.na(allHospitals)])
  Hospital
}