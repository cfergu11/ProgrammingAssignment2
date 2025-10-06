## This script contains a pair of functions that 1) cache the inverse of a 
## matrix input by the user, and 2) returns that cached inverse if it has
## already been calculated by the makeCacheMatrix function

## This function creates a special "matrix" object that can cache its inverse.
## It must be saved as a new object to be used with cacheSolve

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" returned by 
## `makeCacheMatrix` above. If the inverse has already been calculated (and the 
## matrix has not changed), then `cacheSolve` should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

