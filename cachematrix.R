## These functions cache the inverse of a matrix to avoid redundant computations.


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inver <- NULL #set inverse as null

  set <- function(y) {
  x <<- y #set new matrix
  inver <<- NULL #reset the inverse
}
get <- function()x #return matrix

setinverse <- function(inverse) inver <<- inverse #cache inverse

getinverse <- function() inver #return cached inverse

list(set = set, get = get,
     setinverse = setinverse,
     getinverse = getinverse)

}


## computes the inverse of the matrix if not already cached,
## or retrieves it from the cache if it has already been calculated.

cacheSolve <- function(x, ...) {
  inver <- x$getinverse()
  if(!is.null(inver)) {
    message("getting cached data")
    return(inver)                  #return cached inverse
  }
  data <- x$get() #set matrix
  inver <- solve(data, ...) #solve for inverse
  x$setinverse(inver) ## Return a matrix that is the inverse of 'x'
  inver  
}

 ##Example matrix

m <- matrix(c(6, 8, 12, 4), nrow = 2, ncol = 2) #create matrix
cache_m <- makeCacheMatrix(m)  #create cache matrix
invcache_m <- cacheSolve(cache_m)  #get inverse 
print(invcache_m)
re_invchache_m<- cacheSolve(cache_m)
print(re_invchache_m)