## following test procedure provided by Scott Francis
source("makeCacheMatrix.R")
source("cacheSolve.R")

## create sourcable object
m <- makeCacheMatrix()
## initialize with easy-to-read matrix
m$set( matrix(c(1,0,5,2,1,6,3,4,0), nrow=3, ncol=3, byrow=TRUE))
## checking on set matrix:
m$get()
> ## test inverse cacher
cacheSolve(m)
## testing again, should be quicker because cached
cacheSolve(m)
m$get() %*% cacheSolve(m)
## letting R test the identity matrix
all.equal( diag(3), m$get() %*% cacheSolve(m))
## save inverse off in m1
m1 <- cacheSolve(m)
## set m to some new values
m$set( matrix(c(0,2,2,0),2,2))
# checking if fetching inverse works without pre-caching:
all.equal( diag(2), m$get() %*% cacheSolve(m))
## notice no message and we got the identity matrix