## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## The function creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(matrix = matrix()) {
    inverse_matrix <- NULL
    
    set_matrix <- function(m) {
        matrix <<- m
        inverse_matrix <<- NULL
    }
    get_matrix <- function() {
        matrix
    }
    set_inverse <- function(inv) {
        inverse_matrix <<- inv
        }
    get_inverse <- function() {
        inverse_matrix
    }
    list(set_matrix = set_matrix, get_matrix = get_matrix,
         set_inverse = set_inverse,
         get_inverse = get_inverse)
}

## Write a short comment describing this function
##The function computes the inverse of the special matrix
cacheSolve <- function(cached_matrix, ...) {
    inverse_matrix <- cached_matrix$get_inverse()
    if(!is.null(inverse_matrix)) {
        message("getting cached data")
        return(inverse_matrix)
    }
    data <- cached_matrix$get_matrix()
    inverse_matrix <- solve(data, ...)
    cached_matrix$set_inverse(inverse_matrix)
    inverse_matrix
}
