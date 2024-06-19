# Author: Gabriel Crone
# Date: June 19, 2024

# The pair of functions below creates a cached version of a matrix
# (`makeCahceMatrix`), and finds the inverse of the cached matrix
# (`cacheSolve`). The purpose is to make large matrices far more efficient
# for R to solve. 

# Note that `makeCacheMatrix` assumes that the inputted matrix is square and
# invertable. Otherwise, the function will error out.

# This first function creates a matrix that can be cached in memory 
# for more efficient computations.
makeCacheMatrix <- function(x = matrix()) {
     m <- NULL
     set <- function(y) {
          x <<- y
          m <<- NULL
     }
     get <- function() x
     setInverse <- function(solve) m <<- solve
     getInverse <- function() m
     list(set = set, get = get,
          setInverse = setInverse,
          getInverse = getInverse)
}

# This second function takes the cached matrix and solves it.
# If the inverse was already calculated, then this function
# retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
     m <- x$getInverse()
     if(!is.null(m)) {
          message("getting cached data")
          return(m)
     }
     data <- x$get()
     m <- solve(data, ...)
     x$setInverse(m)
     m
}
