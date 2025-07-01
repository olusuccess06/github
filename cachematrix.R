# Function to create a special "matrix" object that can cache its inverse
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

# Function to compute the inverse of the special "matrix" 
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}
# Create a matrix
A <- matrix(c(1,2,3,4), nrow = 2, ncol = 2)

# Create a special "matrix" object
cached_A <- makeCacheMatrix(A)

# Calculate the inverse (this will compute and cache the inverse)
cacheSolve(cached_A)

# Call again (this will retrieve the cached inverse)
cacheSolve(cached_A)
