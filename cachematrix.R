## Put comments here that give an overall description of what your
## functions do
# Caching Inverse of a Matrix
## Write a short comment describing this function
## makeCacheMatrix creates a matrix that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  matinv <- NULL
  ## this is a function to set the matrix
  set <- function(y){   
    x <<- y
    matinv <<- NULL
  }
  ## this function returns the matrix
  get <- function() x   
  
  ##this function sets the inverse of the matrix
  setInv <- function(solveMat) matinv <<- solveMat  
  
  ##this function returns the inverse of the matrix
  getInv <- function() matinv 
  
  ## return list of functions for matrix
  list(set = set, get = get, setInv = setInv, getInv = getInv) 
}


## Write a short comment describing this function
## cacheSolve function computes the inverse of a matrix if not already calculated
##If the inverse has already been calculated, this function returns the cached inverse
cacheSolve <- function(x, ...) {
  matinv <- x$getInv()  ##get the inverse of the matrix
  if(!is.null(matinv)){   ##condition to check if the inverse of the matrix is already computed
    message("The inverse is cached")
    return(matinv)
  }
  value <- x$get()  ##if the inverse not present already, get the matrix
  matinv <- solve(value) ##function which returns the inverse of a matrix
  x$setInv(matinv) ##cache the inverse of the matrix 
  matinv ##return the matrix
}
