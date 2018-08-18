## Below are two functions
## The first, makeCacheMatrix, creates a special "matrix" 
## object that can cache its inverse
## The second, cacheSolve, computes the inverse of the special "matrix"
## returned by makeCacheMatrix 


## makeCacheMatrix
## The following function creates a special "matrix"
## with four functions that 
## set the value of the matrix
## get the value of the matrix
## set the inverse of the matrix
## get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
      x <<- y
      m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) 
  m <<- inverse
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## cacheSolve
## The following function  
## outputs the inverse of the special "matrix" created by makeCacheMatrix. 
## First, the function checks to see if the inverse can be found in the cache
## If it cannot, the function caluculates the inverse and set it in cache

cacheSolve <- function(x, ...) {
  m <- x$getInverse() 
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()  
  m <- solve(data) 
  x$setInverse(m)  
  m 
}
