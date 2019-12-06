## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse.
## It will return a list containing a function to:
## set the value of the matrix, get the value of the matrix, 
## set the value of the inverse, get the value of the inverse, 
makeCacheMatrix <- function(x = matrix()) {
  inverseMatrix <- NULL
  
  # set the value of the matrix
  setMatrix <- function(y){
    x <<- y
    inverseMatrix <<- NULL
  }
  
  # get the value of the matrix
  getMatrix <- function() x
  
  # set the value of the inverse
  setInverse <- function(inverse) inverseMatrix <<- inverse
  
  # get the value of the inverse
  getInverse <- function() inverseMatrix
  
  list(setMatrix = setMatrix, getMatrix = getMatrix, 
       setInverse = setInverse, getInverse = getInverse)
}

## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getInverse()
  if (!is.null(inverse)){
    message("getting cached data")
    return(inverse)
  }
  matrix <- x$getMatrix()
  #Computing the inverse of a square matrix can be done with the solve function in R. 
  # For example, if X is a square invertible matrix, then solve(X) returns its inverse.
  inverse <- solve(matrix, ...)
  x$setInverse(inverse)
  inverse
}
