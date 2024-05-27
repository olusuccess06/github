## This function creates a special "matrix" object that can cache its inverse. 
## The object is essentially a list containing functions to:
## - Set the value of the matrix.
## - Get the value of the matrix.
## - Set the value of the inverse.
## - Get the value of the inverse.

## The makeCacheMatrix initializes a matrix x and a cache for its inverse inv.
## The function also provides methods to set and get the matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y  
    inv <<- NULL 
}
  get <- function() x 
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## The cacheSove checks if the inverse is already cached using x$getInverse().
## If cached, returns the cached inverse. If not cached, computes the inverse
## using the solve function, caches it using x$setInverse(), and then
## returns it.

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
