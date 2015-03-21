## Matrix inversion is usually a costly computation and there may be some benefit 
## to caching the inverse of a matrix rather than compute it repeatedly.
## These functions cache the inverse of a given matrix

## makeCacheMatrix creates a list that contains a function to set a 
## a matrix, get a matrix, set an inverse, and get an inverse
## this list is the input to cacheSolve()

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(m) {
    x <<- m
    inv <<- NULL
  }
  get <- function() x
  setMatrix <- function(inverse) inv <<- inverse
  getMatrix <- function() inv
  list(set=set, get=get, setMatrix=setMatrix, getMatrix=getMatrix)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and 
## the matrix has not changed), then the cachesolve should retrieve the inverse 
## from the cache.

## solves a matrix, or uses the cache if possible
cacheSolve <- function(x, ...) {
    inv <- x$getMatrix()
    if(!is.null(inv)) {
      message("cached")
      return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setMatrix(inv)
    inv
}