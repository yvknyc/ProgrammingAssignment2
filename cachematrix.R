## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  invrs <- NULL
  set <-function(y) {
    x <<- y
    invrs <<- NULL
  }
  get <- function() x
  setinvrs<-function(inv) invrs<<-inv
  getinvrs<-function() invrs
  list(set = set, get = get, setinvrs = setinvrs, getinvrs = getinvrs)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinvrs()
  
  if (!is.null(i)){
    message("getting cached inverse")
    return(i)
  }
  ## here we have to calculate the inverse
  m <- x$get()
  i <- solve(m, ...)
  x$setinvrs(i)
  i
}
