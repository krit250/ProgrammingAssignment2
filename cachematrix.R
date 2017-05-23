## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  
  set <- function(y) {
    
    x <<- y
    
    inv <<- NULL
    
  }
  
  get <- function() x
  
  setInverse <- function(inverse) inv <<- inverse
  
  getInverse <- function() inv
  
  list(set = set,
       
       get = get,
       
       setInverse = setInverse,
       
       getInverse = getInverse)
  
}

### makeCacheMatrix: Function created for "matrix" object that can cache its inverse ###
makeCacheMatrix <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve =  setsolve,
       getsolve = getsolve)
}


### This function uses argument returned 4m makeCacheMatrix to retrieve inverse matrix 4m cached value ###
cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}

#### USE BELOW CODE FOR VERIFICATION ####
M <- matrix(1:4, nrow = 2, ncol = 2) ## Creates an inverse matrix M ##
aMatrix <- makeCacheMatrix(M) # Input M to makeCacheMatrix function #
aMatrix$get() # Retrieve the value of matrix #
cacheSolve(aMatrix) # Get the inverse of M #
invM <- aMatrix$getsolve() # Create inverse matrix of M from its CACHED value #
M %*% invM ## Multiply M by its inverse to get identity ##
