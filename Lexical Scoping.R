makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL  # Initialize the inverse as NULL
  
  set <- function(y) {
    x <<- y    # Assign new matrix value
    inv <<- NULL  # Reset inverse cache
  }
  
  get <- function() x  # Retrieve the matrix
  
  setInverse <- function(inverse) inv <<- inverse  # Store the inverse
  getInverse <- function() inv  # Retrieve the inverse
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  
  if (!is.null(inv)) {
    message("Getting cached inverse")
    return(inv)
  }
  
  data <- x$get()
  if (nrow(data) != ncol(data)) {
    stop("The matrix must be square to compute its inverse.")
  }
  
  inv <- solve(data, ...)  # Compute inverse
  x$setInverse(inv)  # Cache inverse
  
  inv  # Return inverse
  