## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCachematrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
          inv <- x$getinverse()
  
  # Return cached inverse if available
  if(!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
        ## Return a matrix that is the inverse of 'x'
}

        source("cachematrix.R")

mat <- matrix(c(1, 2, 3, 4), 2, 2)
cachedMat <- makeCacheMatrix(mat)
cacheSolve(cachedMat)  # First time: computes inverse
cacheSolve(cachedMat)
