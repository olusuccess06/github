## A pair of functions that cache the inverse of a matrix

## Create a matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
 y <- NULL
 set <- function(matrix){
   x <<- matrix
   y <<- NULL
 }
 get <- function(){
   x
 }
 setInverse <- function(inverse){
   y <<- inverse
 }
 getInverse <- function(){
   y
 }
 list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Compute the inverse of "makeCacheMatrix". If the inverse has already been
## calcualted, "cachesolve" retrieve the inverse from the cache.

cacheSolve <- function(z, ...) {
        ## Return a matrix that is the inverse of 'x'
  x <- z$getInverse()
  if( !is.null(x)){
    message("getting cached data")
    return(x)
  }
  data <- z$get()
  x <- solve(data) %*% data
  z$setInverse(x)
  x
}
