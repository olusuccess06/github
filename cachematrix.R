## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## The makeCacheMatrix function caches the inverse of a matrix and it includes a list of functions such as:
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        ## set the value of the matrix
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        ## get the value of the matrix
        get <- function() x
        ## set the value of the inverse
        setinverse <- function(inverse) i <<- inverse
        ## get the value of the inverse
        getinverse <- function() i
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function
## The cacheSolve function computes the inverse of the matrix returned by the 
## makeCacheMatrix function above
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if (!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
