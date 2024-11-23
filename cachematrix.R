## Put comments here that give an overall description of what your
## functions do
## The makeCacheMatrix function creates a special "matrix" object that can cache its inverse.
# It contains four functions: 
# - set: assigns a new matrix and resets the cached inverse.
# - get: retrieves the current matrix.
# - setInverse: caches the inverse of the matrix.
# - getInverse: retrieves the cached inverse if available.
# This allows efficient re-use of the inverse calculation, avoiding redundant computations.

# The cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix.
# It first checks if the inverse has already been calculated and cached. 
# - If the cached inverse exists, it retrieves and returns it with a message.
# - If not, it computes the inverse using the solve() function, caches the result, and then returns it.
# This function optimizes performance by avoiding redundant inverse calculations.
## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    j <- NULL
    set <- function(y){
        x <<- y
        j <<- NULL
    }
    get <- function()x
    setInverse <- function(inverse) j <<- inverse
    getInverse <- function() j 
    list(set = set, get = get, 
         setInverse = setInverse, 
         getInverse = getInverse)
}


## Write a short comment describing this function
##See ABOVE
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    j <- x$getInverse()
    if(!is.null(j)){
        message("getting cached data")
        return(j)
    }
    mat <- x$get()
    j <- solve(mat,...)
    x$setInverse(j)
    j
}
