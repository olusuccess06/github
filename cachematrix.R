# This script does 2 things: 
# 1. it stores data into an object and initializes a "storage space" for a computation performed on said data
# 2. it performs computation on the vector, returns an answer, and stores the answer in the storage space

################################################################################
# Assignment 2
################################################################################

makeCacheMatrix <- function(x = matrix()) { # The function where data is stored and "stotage" or cache is initialized
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

cacheSolve <- function(x, ...) { # A function that solves the inverse of a matrix 'x', the inverse is stored in makeCasheMatrix for fast retrival
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}

