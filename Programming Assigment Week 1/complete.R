complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  numberCompleteCases = NULL
  
  for(i in seq_along(id)) {
    name = paste("./",directory,"/",sprintf("%03d", id[i]),".csv",sep="")
    frame = read.csv(name)
    
    numCases  = sum(as.numeric(complete.cases(frame)))
    
    numberCompleteCases = c(numberCompleteCases,numCases)
  }
  data.frame(id,nobs=numberCompleteCases)
}