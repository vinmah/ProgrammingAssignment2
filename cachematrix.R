## A pair of functions that store a matrix, update the matrix, 
## calculate the inverse, and cache that inverse

## Stores a matrix and its inverse. Returns a list of functions
## that set the matrix, get the value of the matrix, cache the 
## matrix's inverse, and returns the value of that inverse

makeCacheMatrix <- function(x = matrix()) {
  storedInverse <- NULL
  set <- function(y) {
    x <<- y
    storedInverse <<- NULL
  }
  get <- function() x
  setInverse <- function (inverseMatrix) storedInverse <<- inverseMatrix
  getInverse <- function() storedInverse
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}


## Calculate the inverse of the matrix created with the makeCacheMatrix function
## Returns the matrix's inverse after caching the result

cacheSolve <- function(x, ...) {
  theInverse <- x$getInverse()
  if (!is.null(theInverse)){
    message("getting cached inverse")
    return(theInverse)
  }
  storedMatrix <- x$get()
  theInverse <- solve(storedMatrix, ...)
  x$setInverse(theInverse)
  theInverse
}
