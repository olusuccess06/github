makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL  # Reset the cache if the matrix is changed
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}
cacheSolve <- function(x, ...) {
  # Check if the inverse is already cached
  inv <- x$getInverse()
  
  if(!is.null(inv)) {
    # If cached, return the cached inverse
    message("getting cached data")
    return(inv)
  }
  
  # Otherwise, compute the inverse
  mat <- x$get()  # Get the matrix
  inv <- solve(mat, ...)  # Compute the inverse
  
  # Cache the computed inverse
  x$setInverse(inv)
  
  # Return the computed inverse
  inv
}