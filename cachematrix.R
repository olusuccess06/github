# Define the makeCacheMatrix function
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  
  setInverse <- function(inverse) inv <<- inverse
  
  getInverse <- function() inv
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

# Define the cacheSolve function with error handling
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  data <- x$get()
  
  # Check if the matrix is invertible by examining the determinant
  if (det(data) == 0) {
    stop("The matrix is singular and cannot be inverted.")
  }
  
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}

# Test the caching mechanism with an invertible matrix
# Create an invertible matrix
my_matrix <- matrix(c(1, 2, 3, 4), 2, 2)

# Ensure the matrix is invertible by modifying it slightly
my_matrix[1, 1] <- 1.0001

# Create a special "matrix" object
cached_matrix <- makeCacheMatrix(my_matrix)

# Compute and cache the inverse
inverse1 <- cacheSolve(cached_matrix)
print("Inverse of the matrix:")
print(inverse1)

# Retrieve the cached inverse without recomputing
inverse2 <- cacheSolve(cached_matrix)
print("Retrieved cached inverse of the matrix:")
print(inverse2)

# Change the matrix to test resetting the cache with another invertible matrix
cached_matrix$set(matrix(c(2, 3, 3, 4), 2, 2))

# Compute and cache the new inverse
inverse3 <- cacheSolve(cached_matrix)
print("Inverse of the new matrix:")
print(inverse3)
