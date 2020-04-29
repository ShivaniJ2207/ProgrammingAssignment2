## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  matinv <- NULL
  set <- function(y){
    x <<- y
    matinv <<- NULL
  }
  get <- function() x
  setInverse <- function(solveMatrix) matinv <<- solveMatrix
  getInverse <- function() matinv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
matinv <- x$getInverse()
  if(!is.null(matinv)){
    message("Receiving the cached data")
    return(matinv)
  }
  value <- x$get()
  matinv <- solve(value)
  x$setInverse(matinv)
  matinv
}
