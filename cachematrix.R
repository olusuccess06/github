##We will be creating 2 functions, such that we can calculate the inverse of a matrix if needed or we can pull it from the cache if it is yet to exist in order to reduce redundant calculations

##The makeCacheMatrix function will: make an object containing a matrix that allows you to (1) set the value of the matrix; (2) retrieve the value of the matrix if it already exisits; (3) set the value of the inverse of the matrix, using (1) or the cached value; and (4) retrieve the inverse of the matrix that is stored


## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function () i
    list( set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## Write a short comment describing this function

## The cacheSolve function will calculate the inverse of the matrix as computed by makeCacheMatrix if it doesn't yet exist in the cache. else, it calculates the inverse matrix and sets the value of the inverse matrix in the makeCacheMatrix function (3)


cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("retrieving inverse of matrix")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
        ## Return a matrix that is the inverse of 'x'
}
