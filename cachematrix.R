## Assignment: Caching the Inverse of a Matrix.
## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly.
## A pair of functions are written to cache the inverse of a matrix.

## makeCacheMatrix is a function that creates a special "matrix" object that can cache its inverse.
## This function is a list containing a function to
##      1. set the matrix
##      2. get the matrix
##      3. set the inverse
##      4. get the inverse
## For this assignment, the matrix supplied is assumably always invertible.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  
  ##set the matrix
  setMatrix <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  ##get the matrix
  getMatrix <- function() x
  
  ##set the inverse
  setInv <- function(invMatrix) {
    inverse <<- invMatrix
  }
  
  ##get the inverse
  getInv <- function() inverse
  
  ##this list servers as the input to cacheSolve function
  list(setMatrix=setMatrix, getMatrix=getMatrix, setInv=setInv, getInv=getInv)
}


## cacheSolve is a function that computes the inverse of the special "matrix" returned by makeCacheMatrix function above.
## If the inverse has already been calculated (and the matrix has not changed), then the cacheSolve function should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getInv()
  
  ## if the inverse has already been calculated
  if(!is.null(inverse)) {
    ## retrieve the inverse from the cache
    message("getting cached data")
    return(inverse)
  }
  
  ## if the inverse has not been calculated before
  data <- x$getMatrix()
  
  ## computes the inverse
  inverse <- solve(data, ...)
  
  ## set the inverse in the cache through the setInv function
  x$setInv(inverse)
  
  return(inverse)
}
