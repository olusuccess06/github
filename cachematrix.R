## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    ## Clear the cache
    inv <- NULL 
    ## The "set" function sets the value of the matrix and invalidates the cached inverse
    set <- function(y) {
        x <<- y 
        inv <<- NULL 
    } 
    ## The "get" function returns the value of the matrix
    get <- function() x 
    ## Cache the inverse of the matrix
    setInverse <- function(inverse) inv <<- inverse 
    ## Return the cached inverse if it exists
    getInverse <- function() inv 
    list(set = set, get = get, 
         setInverse = setInverse, 
         getInverse = getInverse) 
}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" created by makeCacheMatrix. If the inverse has already been calculated, it retrieves the cached inverse
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
            inv <- x$getInverse() 
    ## If there is already a cached result, return the cache content and bypass the calculation
    if(!is.null(inv)) { 
        message("getting cached data") 
        return(inv) 
    } 
    ## If cache is empty, calculate the inverse of the matrix
    data <- x$get() 
    inv <- solve(data, ...) 
    x$setInverse(inv) 
    inv
}
