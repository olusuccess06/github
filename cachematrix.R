## Put comments here that give an overall description of what your
## functions do
## This function creates a special "matrix" object to cache its inverse.
makeCacheMatrix <- function(x = matrix()){
  i <- NULL #Initialize the inverse matrix
  
  #Función para guardar una nueva matriz y resetear la inversa
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  #Función para obtener la matriz guardada
  get <- function() {
    x # Devuelveme la matriz
  }
  
  #Función para guardar la inversa
  setinv <- function(inv) {
    i <<- inv
  }  
  
  #Función para obtener la inversa guardada
  getinv <- function(){
    i
  }
  
  # Devolvemos estas cuatro funciones como una lista. Si no devolvemos la funciones en forma de lista, no podremos acceder a las funciones internas
  # Las listas están pensadas para almacenar funciones, a diferencias de otras estructuras como por ejemplo los data frame.
  list(set = set, get = get, setinv = setinv, getinv = getinv)

}


## This function check if the inverse matrix has already been calculated. 
##If so, it gets the value from the cache and skips the computation. 
##Otherwise, it calculates the mean of the data and sets the inverse value in the cache via the setinv function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinv() # Intenta obtener la inversa
  
  # Si la inversa ya está calculada, la usamos
  if(!is.null(i)) {
    message ("getting cached data")
    return(i)
  }
  
  # Si no está calculada, la calculamos
  data <- x$get()
  i <- solve(data,...)
  x$setinv(i) # Guardamos la inversa
  
  i # Devolvemos la inversa
}
