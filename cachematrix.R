
## This function creates a special "matrix" object that can cache its median.
## It includes functions to set and get the matrix, and to set and get the cached median.


## This function computes the median of the special "matrix" returned by makeCacheMatrix.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL  # Reset the inverse when the matrix is changed
  }
  
  get <- function() x
  
  setInverse <- function(inverse) inv <<- inverse
  
  getInverse <- function() inv
  
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}




cacheSolve <- function(x, ...) {
  # Try to get the cached inverse
  inv <- x$getInverse()
  
  # If the inverse is already cached, return it
  if (!is.null(inv)) {
    message("Getting cached inverse")
    return(inv)
  }
  
  # If not cached, compute the inverse
  mat <- x$get()         # Get the matrix
  inv <- solve(mat, ...) # Compute the inverse using solve()
  
  # Cache the inverse for future use


