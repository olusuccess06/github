
## makeCacheMatrix is a function that creates a matrix, calculates its
##inverse, get this matrix's inverse value and stores it as a list.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) m <<- inverse
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
 

}


## cacheSolve is a function that calculates the inverse of a Matrix. If
## the matrix has not changed from the one created with makeCacheMatrix,
##this function just takes the value originated with that function (does
##not make a new calculation).

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
  
}
