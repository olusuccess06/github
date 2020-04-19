## We need to write two functions that will cache the inverse of a matrix x
## we will use the example that it was provided

## makeCacheMatrix - creates a matrix obj where the cache is the inverse of the input

makeCacheMatrix <- function(x = matrix()) {
	inv<-NULL
	set<-function(y){
		x<<-y
		inv<<-NULL}
	get<-function(){x}
	setInverse <- function(inverse) inv <<- inverse
 	getInverse <- function() {inv} 
  	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
	
}


## cacheSolve - computes the inverse and returns it, if the inverse was already computed, 
##then the function should return the inverse from the cache

cacheSolve <- function(x, ...) {
       inv<- x$getInverse()
 	 if(!is.null(inv)){
  		message("getting cached data")
  		return(inv)}
  	 m<- x$get()
  	 inv <- solve(m,...)
  	 x$setInverse(inv)
  	 inv

}
