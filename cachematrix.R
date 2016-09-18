################################################################
## Program cachematrix.R                                      ##
## Programming Assignment 2 / R programming                   ##
## by Francis Chagnon (francis.chagnon@videotron.ca)          ##
################################################################
## This program calculates the inverse of a matrix using      ##
## functions to cache potentially time-consuming computations ##
## It is assumed that the matrix supplied to the functions    ##
## are always invertible                                      ##
################################################################
## Testing was done using the following invertible matrix     ##
## myMatrix <- rbind(c(2, -1), c(1,3))                        ##
################################################################

## Function makeCacheMatrix
## This function creates a matrix object 
## that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)  
}


## Function cacheSolve
## This function computes the inverse of the matrix returned by 
## the makeCacheMatrix function. If the inverse has already been 
## calculated (and the matrix has not changed), then the cachesolve
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m 
}
