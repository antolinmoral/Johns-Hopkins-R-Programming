cacheSolve <- function(x, ...) {
  minv <- x$getMInv()
  if(!is.null(minv)) {
    message("getting cached data")
    return(minv)
  }
  data <- x$get()
  minv <- solve(data, ...)
  x$setMInv(minv)
  minv
}