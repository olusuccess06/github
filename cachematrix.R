## These functions collectively implement a caching system for the inverse of a matrix.
## The `makeCacheMatrix` function creates a special matrix object that can store its inverse.
## The `cacheSolve` function computes the inverse of the matrix or retrieves it from cache if already computed.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

    m <- NULL     # Variable to store the cached inverse
    set <- function(y) {
      x <<- y     # Assign a new matrix to x
      m <<- NULL  # Reset the cached inverse
    }
    get <- function() x     # Retrieve the matrix
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)     # Return a list of functions
  
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed),
## it retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()     # Check if the inverse is already cached
  if(!is.null(m)) {
    message("getting cached data")     # Notify that cached data is being used
    return(m)    # Return the cached inverse
  }
  data <- x$get()        # Retrieve the matrix
  m <- solve(data, ...)  # Compute the inverse
  x$setinverse(m)        # Cache the inverse
  m                      # Return the inverse
}
