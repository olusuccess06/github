## Cache the inverse of a matrix rather than compute it repeatedly

## Create a "matrix" object that can cache its inversse

makeCacheMatrix <- function(x = matrix()) {
i <- NULL
set <- function(y) {
      x <<- y
      i <<- NULL
}
get <- function() x
setinverse <- function(inverse) i <<- inverse
getinverse <- function() i
list(set = set, get = get, setinverse = setinverse, getinverse =getinverse )
}

## Compute the inverse of a square "matrix" object 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      i <- x$getinverse()
      if (!is.null(i)) {
            message("getting cached data")
            return(i)
      }
      matrix <- x$get()
      i <- solve(matrix, ...)
      x$setinverse(i)
      i
}
