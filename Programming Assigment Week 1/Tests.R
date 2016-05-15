#exercise 3
source("corr.R")
source("complete.R")
cr <- corr("specdata", 150)
head(cr)



directory = "specdata"
threshold = 0

fichCompletos = complete(directory)
fichSupThr    = fichCompletos[fichCompletos$nobs > threshold,]

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
cr = cor(vectorNitrate,vectorSulfate)





source("corr.R")
source("complete.R")
cr <- corr("specdata", 150)
head(cr)


#exercise 2
directory = "specdata"
id = c(2, 4, 8, 10, 12)

source("complete.R")
complete("specdata", 1)
complete("specdata", c(2, 4, 8, 10, 12))
complete("specdata", 30:25)
complete("specdata", 3)


numberCompleteCases = NULL

for(i in seq_along(id)) {
  name = paste("./",directory,"/",sprintf("%03d", id[i]),".csv",sep="")
  frame = read.csv(name)
  
  numCases  = sum(as.numeric(complete.cases(frame)))
  
  numberCompleteCases = c(numberCompleteCases,numCases)
}
result = data.frame(id,nobs=numberCompleteCases)



#exercise 1


pollutant = "nitrate"
id = 70:72

source("pollutantmean.R")
pollutantmean("specdata", "sulfate", 1:10)
pollutantmean("specdata", "nitrate", 70:72)
pollutantmean("specdata", "nitrate", 23)

valuesPollutantAll = NULL

for(i in seq_along(id)) {
  name = paste("./",directory,"/",sprintf("%03d", id[i]),".csv",sep="")
  print(name)
  frame = read.csv(name)
  flag = is.na(frame[pollutant])
  valuesPollutant = frame[pollutant]
  valuesPollutantNoNA = valuesPollutant[!flag]
  valuesPollutantAll = c(valuesPollutantAll, valuesPollutantNoNA)
}
mean(valuesPollutantAll)
