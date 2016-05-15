makeCacheMatrix <- function( x = numeric() ) {
  minv <- NULL
  set <- function(y) {
    x <<- y
    minv <<- NULL
  }
  get <- function() x
  setMInv <- function(minverse) minv <<- minverse
  getMInv <- function() minv
  list(set = set, get = get,
       setMInv = setMInv,
       getMInv = getMInv)
}