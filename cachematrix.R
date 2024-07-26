## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# This function wiil cache the inverse matrix, setinverse() will cache the
# inverse and getinverse() will return the cached matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function () x
  setinverse <- function(inv) inv_matrix <<- inv
  getinverse <- function() inv_matrix
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function\
# The function will get the inverse matrix from x, checks if the cache already
# exist, if not it will get the matrix and solve using solve() function

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if (!is.null(m)) {
    message("Getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
