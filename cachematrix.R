
## R Programming - Assignment #2

## The makeCacheMatrix function stores a matrix x along with
## its inverse.  The CacheSolve function takes the cached
## matrix structure and returns the inverse of the original matrix.
## This function uses the getter and setter from makeCacheMatrix
## to calculate and store the inverse of x.


# Function makeCacheMatrix

# This function takes an invertible matrix as input and
# creates getters and setters for the object, as well
# as being able to cache the inverse for future retrieval.
# This function # is used in conjunction with cacheSolve, 
# to return the inverse of the initial matrix. 

makeCacheMatrix <- function(x = matrix()) {
  ## Create a matrix that can cache the inverse
  inv <<- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(y) inv <<- (y)
  getinv <- function() inv
  list(set = set, get = get, 
       setinv = setinv, 
       getinv = getinv)
  } # return a list of functions associated with x

# Function CacheSolve

# This function takes a cached matrix x as input 
# and returns the inverse of the function stored with x. If 
# the inverse is NULL, it will create the inverse and then
# return it.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
} # return the inverse matrix to x