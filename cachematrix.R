## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## Creates a special object that saves a matrix and its inverse in cache 

makeCacheMatrix <- function(x = matrix()) {
        
        inv <- NULL #Inicialize the inverse as NULL
        
        # Define function
        
        set <- function() {
                x <<- y
                inv <<- NULL #Errase cache
        }
        
        #Define function for obtain matrix
        get <- function() x
        
        #Define function to establish inverse matriz in cache 
        setinverse <- function(inverse) inv <<- inverse
        
        #Define function to obtain the inverse in cache 
        getinverse <- function() inv
        
        #Return a list with the functions
        list(
                set = set,
                get = get,
                setinverse = setinverse,
                getinverse = getinverse
        )

}


## Write a short comment describing this function

## Calculates the inverse of the matrix or takes it from cache if the matrix exists

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse() # Intenta obtener la inversa de la caché
        
        # Si la inversa está en caché, la retorna
        if (!is.null(inv)) {
                message("Obteniendo datos en caché")
                return(inv)
        }
        
        # Si no está en caché, calcula la inversa
        data <- x$get() # Obtiene la matriz original
        inv <- solve(data, ...) # Calcula la inversa con solve()
        x$setinverse(inv) # Guarda la inversa en caché
        
        inv # Retorna la inversa calculada
}

matrix <- matrix(c(4, 3, 2, 1), nrow = 2, ncol = 2)
obj <- makeCacheMatrix(matrix)
cacheSolve(obj)
