## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    m <<- matrix
  }
  get <- function() x
  seti <- solve(x) 
  i <<- inverse
  geti <- function() i
  list(set = set, get = get,
       seti = seti,
       geti = geti)
  
}


## Write a short comment describing this function


## Return a matrix that is the inverse of 'x'
cachei <- function(x, ...) {
  m <- x$geti()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$seti(m)
  m
}