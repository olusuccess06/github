# My solution to Programming Assignment 2

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL  # Store the inverse
        
        set <- function(y) {
                x <<- y        # Set new matrix
                inv <<- NULL   # Clear cached inverse
        }
        
        get <- function() x
        
        setinverse <- function(inverse) inv <<- inverse
        
        getinverse <- function() inv
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if (!is.null(inv)) {
                message("getting cached inverse")
                return(inv)
        }
        
        mat <- x$get()
        inv <- solve(mat, ...)  # Compute inverse
        x$setinverse(inv)
        inv
}

# Submitted by Aliya-yuan
