makeMatrix <- function(x = matrix()) {
  i<-NULL
  set<-function(y){
    x<<-y
    i<<-NULL
  }
  get<-function() x
  setmat<-function(solve) i<<- solve
  getmat<-function() i
  list(set=set, get=get,
       setmat=setmat,
       getmat=getmat)
}

cacheSolve <- function(x=matrix(), ...) {
  i<-x$getmat()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  matrix<-x$get()
  i<-solve(matrix, ...)
  x$setmat(i)
  i
}
