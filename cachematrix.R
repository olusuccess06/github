## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  # Function to set the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # Function to get the matrix
  get <- function() x
  
  # Function to set the inverse
  setinverse <- function(inverse) inv <<- inverse
  
  # Function to get the inverse
  getinverse <- function() inv
  
  # Return list of functions
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  # Try to get cached inverse
  inv <- x$getinverse()
  
  # If inverse is cached, return it with message
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # If not cached, calculate inverse
  data <- x$get()
  inv <- solve(data, ...)
  
  # Cache the inverse
  x$setinverse(inv)
  
  # Return the inverse
  inv
}