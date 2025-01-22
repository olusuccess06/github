# The following "makeCacheMatrix" function is to create a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL # Initialize the inverse as NULL
  
  # Set the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL # Reset the inverse when the matrix is updated
  }
  
  # Get the matrix
  get <- function() x
  
  # Set the inverse
  setInverse <- function(inverse) inv <<- inverse
  
  # Get the inverse
  getInverse <- function() inv
  
  # Return a list
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

# The following "cacheSolve" function is to compute the inverse of the "matrix" returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
  inv <- x$getInverse() # Check if the inverse is already cached
  
  if (!is.null(inv)) {
    message("Getting cached inverse!")
    return(inv) # Return the cached inverse
  }
  
  # If inverse is not cached, calculate it
  mat <- x$get()
  inv <- solve(mat, ...) # Compute the inverse
  x$setInverse(inv) # Cache the inverse
  inv
}
