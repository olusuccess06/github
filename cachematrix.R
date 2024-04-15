## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      # set the value of the matrix
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      # get the value of the matrix
      get <- function() x
      # set the value of the inverse
      setinverse <- function(inverse) inv <<- inverse
      # get the value of the inverse
      getinverse <- function() inv
      # return:
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}
## Write a short comment describing this function

# cacheSolve first checks if the inverse can be found in the cache.
# If it can be found the function just returns that value and the message "getting
# cached data". Otherwise the inverse is calculated, saved using setinverse and
# returned.
cacheSolve <- function(x, ...) {
      inv <- x$getinverse() # function() inv
      if(!is.null(inv)) {
            message("getting cached data")
            return(inv) # return ends the function
      }
      data <- x$get() # function() x
      inv <- solve(data, ...) # calculates the inverse and stores it as inv
      x$setinverse(inv) # function(inverse) inv <<- inverse
      inv
}
# test my code
mat <- makeCacheMatrix()
mat$set(matrix(rnorm(9), nrow = 3))
cacheSolve(mat)
