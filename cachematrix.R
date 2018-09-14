## Matrix inversion can be a costly computation and there may be a 
## benefit to caching the inverse of a matrix instead of computing repeatedly.
## The two functions below can be used to create an object that 
## stores a matrix and caches its inverse.

## makeCacheMatric function creates a matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
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
## it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
      inv <- x$getInverse()
      if (!is.null(inv)) {
            message("getting cached data")
            return(inv)
      }
      mat <- x$get()
      inv <- solve(mat, ...)
      x$setInverse(inv)
      inv
}