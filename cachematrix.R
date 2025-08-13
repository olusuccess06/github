## makeCacheMatrix and cacheSolve functions are useful to cache the inverse of a matrix.

## makeCacheMatrix This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
# inv will hold the cached inverse of the matrix
  inv <- NULL 
        # Function to set a new matrix
  set <- function(y) {
    x <<- y       # Assign the new matrix to 'x' in the parent environment
    inv <<- NULL  # Clear the cached inverse since the matrix has changed
  }
        # Function to get the matrix
  get <- function() x
  
  # Function to set the cached inverse
  setInverse <- function(inverse) inv <<- inverse
  
  # Function to get the cached inverse
  getInverse <- function() inv
  
  # Return a list containing all the above functions
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve() checks if the inverse is already stored before calculating it again.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
 inv <- x$getInverse()
# If the inverse is already cached, return it
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
# Otherwise, retrieve the matrix
  mat <- x$get()
  
  # Compute the inverse of the matrix
  inv <- solve(mat, ...)  
  
  # Store the inverse in the cache
  x$setInverse(inv)
  
  # Return the computed inverse
  inv
}
