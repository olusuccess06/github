## Put comments here that give an overall description of what your
## functions do

##Below are a pair of functions that are used to create a special object 
## that stores a matrix and caches its inverse.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL         ## initializing inverse as NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x            ## function to get matrix x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## to get the cache data 
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()      ## check if inverse is NULL
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)            ## returns nverse value
  }
  mat <- x$get()
  inv <- solve(mat, ...)        ##solves inverse value
  x$setInverse(inv)
  inv
}
