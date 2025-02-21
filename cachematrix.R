# This function creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # Initialize inverse as NULL
  
  # Function to set the value of the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL  # Reset inverse when the matrix changes
  }
  
  # Function to get the value of the matrix
  get <- function() x
  
  # Function to set the inverse of the matrix
  setinverse <- function(inverse) inv <<- inverse
  
  # Function to get the inverse of the matrix
  getinverse <- function() inv
  
  # Return a list of functions
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

# This function computes the inverse of the special matrix returned by makeCacheMatrix
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()  # Check if inverse is already cached
  
  # If inverse is cached, return it
  if (!is.null(inv)) {
    message("Getting cached inverse")
    return(inv)
  }
  
  # Otherwise, compute the inverse
  data <- x$get()
  inv <- solve(data, ...)  # Compute the inverse using solve()
  x$setinverse(inv)  # Cache the computed inverse
  inv  # Return the inverse
}

# Example usage:
# matrix_data <- matrix(c(1, 2, 3, 4), 2, 2)
# special_matrix <- makeCacheMatrix(matrix_data)
# cacheSolve(special_matrix)  # Computes and caches inverse
# cacheSolve(special_matrix)  # Retrieves cached inverse
