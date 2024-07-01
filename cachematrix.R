
# By this function a special matrix object is created to cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  i<-NULL
#Initializes "x" as an empty matrix and initializes "i" as NULL.
  set<-function(y){
    x<<-y
    i<<-NULL
  }
#set function takes new matrix "y" as input and replaces "y" to "x" and resets "i" to NULL.
  get<-function()x
  setinverse<-function(inverse)i<-inverse
  getinverse<-function()i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
  
}
#get function returns the current matrix "x".
#setinverse function caches the inverse in "i". 
#getmean function returns the cached inverse of "i".


# This function computes the inverse if it's not cached, otherwise retrieves the cached inverse.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i<-x$getinverse()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
#Retrives cached inverse and if the cached inverse has been computed before.

  data<-x$get()
  i<-solve(data)
  x$setinverse(i)
  i
}
#if cached inverse is NULL, retreives current vector data, computes the inverse and the inverse is cached.
