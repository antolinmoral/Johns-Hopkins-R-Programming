rankall <- function(outcome, num = "best") {

  ## Read outcome data
  df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that num and outcome are valid  
  correctOutcome = ((outcome=="heart attack") || (outcome=="pneumonia") || (outcome=="heart failure"))
  correctNum = ((num=="best") || (num=="worst") || (num >= 1))
  if ( ! correctOutcome) {
    stop("invalid outcome")
  } else if ( ! correctNum ){
    stop("invalid num ranking")
  }    
  
  ## For each state, find the hospital of the given rank
  listStatesUnordered = unique(df$State)
  listStates = listStatesUnordered[order(listStatesUnordered)]
  
  hospitals = vector("character",length = length(listStates))
  
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
  ii = 1
  
  for(state in listStates) {
  
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
    hospitals[ii] = Hospital  
    ii = ii + 1
  }
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  dfRankingHospitals <- data.frame(hospital = hospitals, state = listStates, row=listStates, row.names="row")
  dfRankingHospitals
}