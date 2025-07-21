## Put comments here that give an overall description of what your
## functions do

## These functions that cache the inverse of a matrix 

## Write a short comment describing this function

## This function creates a matrix object that can cache 
## its inverse.

## 1. Set the value of the Matrix
## 2. Get the value of the Matrix
## 3. Set the value of the Inverse
## 4. Get the value of the Inverse

makeCacheMatrix <- function(x = matrix()) {

    m = NULL
    set = function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function()x
    setinv <- function(inverse) m  <<- inverse
    getinv <- function()m
    list(set=set, 
         get=get, 
         setinv=setinv, 
         getinv=getinv)
}

## Write a short comment describing this function

## This function computes the inverse of the matrix created above.

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
