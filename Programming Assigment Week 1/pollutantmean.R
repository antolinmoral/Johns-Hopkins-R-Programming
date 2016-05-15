pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  valuesPollutantAll = NULL
  
  for(i in seq_along(id)) {
    name = paste("./",directory,"/",sprintf("%03d", id[i]),".csv",sep="")
    frame = read.csv(name)
    flag = is.na(frame[pollutant])
    valuesPollutant = frame[pollutant]
    valuesPollutantNoNA = valuesPollutant[!flag]
    valuesPollutantAll = c(valuesPollutantAll, valuesPollutantNoNA)
  }
  mean(valuesPollutantAll)
  
}

