## Homework in week 3

## Create an object(special matrix) which containing an inverse attributes 

makeCacheMatrix <- function(x = matrix()) {
    InvMatrix <- NULL
    set <- function(y){
        x <<- y
        InvMatrix <<- NULL
    }
    
    get <- function(){
        x
    }
    
    setInv <- function(Solve){
        InvMatrix <- Solve
    }
    
    getInv <- function(){
        InvMatrix
    }
    
    list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      result <- x$getInv()
      if(!is.null(result)){
          message("getting inverse matrix")
          return(result)
      }
      
      data <- x$get()
      result <- solve(x)
      x$setInv(result)
      result
}
