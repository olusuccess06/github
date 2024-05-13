## Put comments here that give an overall description of what your
## functions do
## This file contains functions that implement a cache for the matrix inversion operation.
## The cacheMatrix function takes a matrix as input and returns a list object that contains
## two functions: set and get. The set function stores the inverse of the input matrix in the
## cache, and the get function retrieves the inverse from the cache if it exists, otherwise it
## calculates the inverse and stores it in the cache.


## This function creates a cache for a matrix and provides methods to set and get the matrix, as well as calculate its inverse.
## Write a short comment describing this function


makeCacheMatrix <- function(x = matrix()) {
    # Initialize the cache variable to store the inverse matrix
    inv <- NULL
    
    # Function to set the matrix value and clear the cache
    set <- function(y) {
        x <<- y
        inv <<- NULL  # Clear the cache when the matrix is set
    }
    
    # Function to get the matrix value
    get <- function() x
    
    # Function to get the cached inverse if available; otherwise compute it
    cacheInverse <- function() {
        if (!is.null(inv)) {
            message("getting cached inverse")
            return(inv)
        }
        inv <- solve(x)
        inv <<- inv  # Cache the inverse
        inv
    }
    
    # Return a list of functions
    list(set = set,
         get = get,
         cacheInverse = cacheInverse)
}



## This function creates a special "matrix" object that can cache its inverse.
## Write a short comment describing this function



cacheSolve <- function(x, ...) {
    # Get the cached inverse if available; otherwise compute it
    inv <- x$cacheInverse()
    
    # Return the inverse matrix
    inv
}






