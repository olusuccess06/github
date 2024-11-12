# This program implements a caching system for matrix inversion to reduce computation costs. 
# It defines two functions: makeCacheMatrix, which creates a special "matrix" object that can cache its inverse, 
# and cacheSolve, which computes the inverse of the matrix or retrieves it from the cache if it has been previously calculated.
# It uses the solve function to calculate the inverse of an invertible square matrix.




## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # Initialize the inverse as NULL
  set <- function(y) {
    x <<- y  # Set the matrix
    inv <<- NULL  # Reset the inverse when the matrix is changed
  }
  get <- function() x  # Get the matrix
  setInverse <- function(inverse) inv <<- inverse  # Set the inverse
  getInverse <- function() inv  # Get the inverse
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}





# This function computes the inverse of the special "matrix" returned by makeCacheMatrix.
# If the inverse has already been calculated (and the matrix has not changed),
# it retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()  # Check if the inverse is already cached
  if (!is.null(inv)) {  # If the inverse is cached, return it
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()  # Get the matrix
  inv <- solve(mat, ...)  # Calculate the inverse
  x$setInverse(inv)  # Cache the inverse
  inv  # Return the inverse
}






# Create a sample invertible matrix
test_matrix <- matrix(c(2, 4, 3, 8), 2, 2)
test_matrix

# Create a special "matrix" object that can cache its inverse
cachedMatrix <- makeCacheMatrix(test_matrix)

# First call to cacheSolve (should compute and cache the inverse)
cat("First calculation of inverse:\n")
inv_first <- cacheSolve(cachedMatrix)
print(inv_first)

# Second call to cacheSolve (should fetch the cached inverse)
cat("Fetching the cached inverse:\n")
inv_cached <- cacheSolve(cachedMatrix)
print(inv_cached)

# Verify that the cached inverse is the same as the one calculated initially
if (identical(inv_first, inv_cached)) {
  cat("The caching mechanism is working correctly!\n")
} else {
  cat("The caching mechanism is not working as expected.\n")
}


