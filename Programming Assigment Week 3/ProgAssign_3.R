# Part A

outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
head(outcome)
ncol(outcome)
nrow(outcome)
names(outcome)

outcome[, 11] <- as.numeric(outcome[, 11])
## You may get a warning about NAs being introduced; that is okay
hist(outcome[, 11])


source("best.R")
best("TX", "heart attack")
# [1] "CYPRESS FAIRBANKS MEDICAL CENTER"
best("TX", "heart failure")
# [1] "FORT DUNCAN MEDICAL CENTER"
best("MD", "heart attack")
# [1] "JOHNS HOPKINS HOSPITAL, THE"
best("MD", "pneumonia")
#[1] "GREATER BALTIMORE MEDICAL CENTER"
best("BB", "heart attack")
#Error in best("BB", "heart attack") : invalid state
best("NY", "hert attack")
#Error in best("NY", "hert attack") : invalid outcome

source("http://d396qusza40orc.cloudfront.net/rprog%2Fscripts%2Fsubmitscript3.R")
submit()



# PArt B
source("rankHospital.R")
rankhospital("TX", "heart failure", 4)
# [1] "DETAR HOSPITAL NAVARRO"
rankhospital("MD", "heart attack", "worst")
#[1] "HARFORD MEMORIAL HOSPITAL"
rankhospital("MN", "heart attack", 5000)
#[1] NA
rankhospital("TX", "heart attack","best")

source("http://d396qusza40orc.cloudfront.net/rprog%2Fscripts%2Fsubmitscript3.R")
submit()



#Part C
source("rankall.R")
dftest = rankall("heart attack", 20)

head(rankall("heart attack", 20), 10)
tail(rankall("pneumonia", "worst"), 3)
tail(rankall("heart failure"), 10)

source("http://d396qusza40orc.cloudfront.net/rprog%2Fscripts%2Fsubmitscript3.R")
submit()
