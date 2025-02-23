# Function to create a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # Variable to store the cached inverse
  
  # Function to set the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL  # Reset the cached inverse when the matrix changes
  }
  
  # Function to get the matrix
  get <- function() x
  
  # Function to set the inverse
  setInverse <- function(inverse) inv <<- inverse
  
  # Function to get the cached inverse
  getInverse <- function() inv
  
  # Return a list of functions to interact with the cache
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

# Function to compute the inverse of the matrix, caching the result
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()  # Check if the inverse is already cached
  
  # If cached inverse exists, return it
  if (!is.null(inv)) {
    message("Getting cached inverse")
    return(inv)
  }
  
  # Otherwise, compute the inverse
  mat <- x$get()
  inv <- solve(mat, ...)  # Compute inverse using solve()
  
  x$setInverse(inv)  # Cache the computed inverse
  inv
}

# Example Usage
m <- matrix(c(2, 1, 3, 4), 2, 2)  # Create a 2x2 invertible matrix
cachedMatrix <- makeCacheMatrix(m)  # Create a cache matrix object
cacheSolve(cachedMatrix)  # Compute and cache the inverse
cacheSolve(cachedMatrix)  # Retrieve the cached inverse
