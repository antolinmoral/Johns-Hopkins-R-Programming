rankhospital <- function(state, outcome, num = "best") {

  ## Read outcome data
  df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  listStates = unique(df$State)
  stateInList = sum(listStates == state)
  
  correctOutcome = ((outcome=="heart attack") || (outcome=="pneumonia") || (outcome=="heart failure"))
  
  correctNum = ((num=="best") || (num=="worst") || (num >= 1))
  
  if (stateInList == 0) {
    stop("invalid state")
  } else if ( ! correctOutcome) {
    stop("invalid outcome")
  } else if ( ! correctNum ){
    stop("invalid num ranking")
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
  
  outcomeState = stateSelected[,numColOutcome]
  flagNA  = is.na(outcomeState)
  outcomeStateNoNA = outcomeState[! flagNA]
  
  nameState = stateSelected[,numColName]
  nameStateNoNA = nameState[! flagNA ]
    
  hospitalOrdered = order(outcomeStateNoNA,nameStateNoNA)
  
  if (num == "best") {
    Hospital =  nameStateNoNA[hospitalOrdered[1]]  
  } else if (num == "worst"){
    Hospital =  nameStateNoNA[hospitalOrdered[length(hospitalOrdered)]]  
  } else if (num <= length(hospitalOrdered) && num > 0){
    Hospital =  nameStateNoNA[hospitalOrdered[num]]    
  } else {
    Hospital = NA
  }
  #allHospitals = stateSelected[stateSelected[,numColOutcome]==min,numColName] 
  #Hospital <- sort(allHospitals[!is.na(allHospitals)])
  Hospital
}