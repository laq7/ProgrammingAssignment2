#Week 3: Programming Assignment 

## Matrix inversion can be a costly computation and there may be a 
## benefit to caching the inverse of a matrix instead of computing repeatedly.

## makeCacheMatric function creates a matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
      ## x is a square invertible matrix
      ## returns a list containing functions to 
      ## set the matrix
      ## get the matrix
      ## set the inverse
      ## get the inverse
 
      inv <- NULL
      set <- function(y) {
            # <<- is used to assign a value to an object in a different environment
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setInverse <- function(inverse) inv <<- inverse
      getInverse <- function() inv
      list(set = set,
           get = get,
           setInverse = setInverse,
           getInverse = getInverse)
}

## cacheSolve function computes the inverse of the matrix created by makeCacheMatrix.
## If the inverse has already been calculated and hasn't been changed,
## it should be retrieved from the cache

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of x
      inv <- x$getInverse()
      if (!is.null(inv)) {
            message("getting cached data")
            return(inv)
      }
      matx <- x$get()
      inv <- solve(matx, ...)
      x$setInverse(inv)
      inv
}