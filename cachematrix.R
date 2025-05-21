## Cria um objeto que armazena uma matriz e seu inverso em cache
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  ## define uma nova matriz e limpa o cache do inverso
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  ## retorna a matriz armazenada
  get <- function() x
  
  ## armazena o inverso em cache
  setinv <- function(inverse) inv <<- inverse
  
  ## retorna o inverso em cache (ou NULL se ainda não existir)
  getinv <- function() inv
  
  list(
    set    = set,
    get    = get,
    setinv = setinv,
    getinv = getinv
  )
}

## Retorna o inverso da "matriz" criada por makeCacheMatrix,
## buscando em cache se já tiver sido calculado
cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setinv(inv)
  
  inv
}
