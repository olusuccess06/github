## This pair of functions allows to cache the inverse of a matrix. 
##If the inverse of a matrix is already cache the program doesn't need to compute it 
##repeatedly. 

## This function generates a matrix object, and a list that contains a function 
##to set and get the value of the matrix, and also the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(z) {
                x <<- z
                i <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) i <<- solve(x)
        getsolve <- function() i
        list(set = set, 
             get = get, 
             setsolve = setsolve,
             getsolve = getsolve)
        
}


## The following function calculates the inverse of the matrix generated with the 
## makeCacheMatrix function. First verifies if the inverse has already been calculated.
## If it's true, it gets the inverse from the cache and skips the computation. 
## Or else it calculates the inverse of the data and sets the return value in the cache 
## by the setinverse function. 


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getsolve()
        if(!is.null(i)) {
                message("Getting cached data")
                return(i)
        }
        dt <- x$get()
        i <- solve(dt, ...)
        x$setsolve(i)
        i
}

