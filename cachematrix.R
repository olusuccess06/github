makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL  # Initialize the inverse as NULL
    set <- function(y) {
        x <<- y  # Assign the new matrix to x
        inv <<- NULL  # Reset inverse cache when matrix changes
    }
    get <- function() x  # Return the matrix
    setInverse <- function(inverse) inv <<- inverse  # Cache the inverse
    getInverse <- function() inv  # Return the cached inverse (if available)
    
    # Return a list of functions
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}
