'''This function creates a special "matrix" object that can cache its inverse. It returns a list of functions to:
        Set the matrix
        Get the matrix
        Set the inverse
        Get the inverse'''

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL

        set <- function(y) {
         x <<- y
         inv <<- NULL  
  }

        get <- function() {
         x
  }

        setInverse <- function(inverse) {
         inv <<- inverse
  }

        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

'''This function computes the inverse of the special "matrix" returned by makeCacheMatrix. If the inverse is 
already cached, it retrieves the inverse from the cache instead of recomputing it.'''

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()

        if (!is.null(inv)) {
          message("getting cached data")
        return(inv)
  }

        mat <- x$get()

        inv <- solve(mat, ...)

        x$setInverse(inv)

        inv
}
