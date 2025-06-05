
## This function creates a special "matrix" object that can cache its median.
## It includes functions to set and get the matrix, and to set and get the cached median.


## This function computes the median of the special "matrix" returned by makeCacheMatrix.

makeCacheMatrix <- function(x = matrix()) {
                m <- NULL
            set <- function(y) {
                    x <<- y
                    m <<- NULL
            }
            get <- function() x
            setmedian <- function(median) m <<- median
            getmedian <- function() m
            list(set = set, get = get,
                 setmedian = setmedian,
                 getmedian = getmedian)
    
}




## If the median has already been calculated (and the matrix has not changed),
## then it retrieves the median from the cache.



        cacheMedian <- function(x, ...) {
    m <- x$getmedian()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- median(data, ...)
    x$setmedian(m)
    m
}


