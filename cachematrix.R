## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(solveMatrix) inv <<- solveMatrix
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv  
}

matrix_data <- matrix(c(4, 7, 2, 6), nrow = 2, ncol = 2)

# Step 2: Create a cache matrix object using makeCacheMatrix
cached_matrix <- makeCacheMatrix(matrix_data)

# Step 3: Compute the inverse for the first time (it will calculate)
inverse_matrix1 <- cacheSolve(cached_matrix)
print("Inverse Matrix:")
print(inverse_matrix1)

# Step 4: Call cacheSolve again to see cached data
inverse_matrix2 <- cacheSolve(cached_matrix)
print("Inverse Matrix (from cache):")
print(inverse_matrix2)
