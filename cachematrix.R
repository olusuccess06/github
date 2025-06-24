## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
 inv <- NULL  # Initialize the inverse cache
  
  set <- function(y) {
    x <<- y         # Assign new matrix
    inv <<- NULL    # Reset cache
  }
  
  get <- function() x                 # Return the matrix
  setInverse <- function(inverse) inv <<- inverse  # Cache the inverse
  getInverse <- function() inv        # Return cached inverse
  
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated and the matrix has not changed,
## then the cached inverse is returned.
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()  # Retrieve cached inverse
  
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  mat <- x$get()         # Get the actual matrix
  inv <- solve(mat, ...) # Compute the inverse
  x$setInverse(inv)      # Cache the inverse
  inv                    # Return the result
}
