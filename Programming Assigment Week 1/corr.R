corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  
  files <- list.files(directory, full.names=TRUE)
  corred = vector(mode="numeric", length=0)
  for (i in 1:332) 
  { 
    inuse <- read.csv(files[i])
    if (sum(complete.cases(inuse)) >= threshold){
      cornow = cor(inuse$sulfate,inuse$nitrate,use="pairwise.complete.obs")
      corred = c(corred,cornow)
    }else{
      next      
    }
  }
  corred
}



