## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# makeCacheMatrix creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL  # Initialize the cached inverse as NULL
    
    # Set the matrix value and reset the cached inverse to NULL
    set <- function(y) {
        x <<- y      # Store the matrix in the parent environment
        inv <<- NULL  # Reset cached inverse to NULL whenever the matrix changes
    }
    
    # Get the matrix value
    get <- function() {
        x
    }
    
    # Set the cached inverse of the matrix
    setInverse <- function(inverse) {
        inv <<- inverse
    }
    
    # Get the cached inverse of the matrix
    getInverse <- function() {
        inv
    }
    
    # Return a list of functions that can be used to interact with the cache
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

# cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix
# If the inverse has already been calculated (and the matrix has not changed), 
# then cacheSolve retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
    inv <- x$getInverse()  # Try to get the cached inverse
    
    # If the inverse is already cached, return it with a message
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    # Otherwise, calculate the inverse of the matrix
    mat <- x$get()  # Get the matrix
    inv <- solve(mat, ...)  # Compute the inverse using the solve function
    
    # Cache the computed inverse
    x$setInverse(inv)
    
    return(inv)
}

