
## Victor Tran
## R programming Week 3 
## Assignment 02 Caching the Inverse of a Matrix

# Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly 
# (there are also alternatives to matrix inversion that we will not discuss here).
# The assignment is to write a pair of functions that cache the inverse of a matrix.


## makeCacheMatrix is a function that creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve is a function that computes the inverse of the special "matrix" created by makeCacheMatrix.
# If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setinverse(m)
  m
  ## Return a matrix 'm' that is the inverse of 'x'
}

