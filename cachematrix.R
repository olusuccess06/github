## makeCacheMatrix: This function initializes a list of functions to set and get the matrix and its inverse


makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL  
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    get <- function() x
    
    setInverse <- function(inverse) inv <<- inverse

    getInverse <- function() inv

    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## cacheSolve: This function first checks if the inverse of the matrix is already cached. If it is, it retrieves and returns the cached inverse
cacheSolve <- function(x, ...) {
            inv <- x$getInverse()  
    
    if (!is.null(inv)) { 
        message("getting cached data")
        return(inv)
    }
    
    mat <- x$get() 
    inv <- solve(mat, ...)  
    x$setInverse(inv)  
    inv 
}
