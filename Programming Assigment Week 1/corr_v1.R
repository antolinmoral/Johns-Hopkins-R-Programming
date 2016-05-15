corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  
#  fichCompletos = complete(directory)
#  fichSupThr    = fichCompletos[fichCompletos$nobs > threshold,]
#  print(nrow(fichSupThr))
  
  if (nrow(fichSupThr) == 0){
    correlation = vector('numeric')    
  } else{
    vectorSulfate = NULL
    vectorNitrate = NULL
    
    for(i in seq_along(fichSupThr$id)) {
      name = paste("./",directory,"/",sprintf("%03d", fichSupThr$id[i]),".csv",sep="")
      file = read.csv(name)
      
      good = complete.cases(file)
      valuesNitrate = file$nitrate[good]
      valuesSulfate = file$sulfate[good]
      
      vectorNitrate = c(vectorNitrate, valuesNitrate)
      vectorSulfate = c(vectorSulfate, valuesSulfate)
    }
    correlation = cor(vectorNitrate,vectorSulfate)  
  }
  correlation
}