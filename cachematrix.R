## This R function is able to cache potentially time-consuming computations.

## functions makeCacheMatrix :
## This function will create a Matrix and stores its Inverse.
## The function sets and gets the Matri as well as its Inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  seti <- function(inverse) i <<- inverse
  geti <- function() i
  list(set = set, get = get,
       seti = seti,
       geti = geti)
}


## function cacheSolve
## This function will determine whether a matrix has already been
## inverted or not. If not then it determines the inverse of the matrix otherwise returns the cached 
## inverse from makeCacheMatrix function

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  i <- x$geti()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$seti(i)
  i
}