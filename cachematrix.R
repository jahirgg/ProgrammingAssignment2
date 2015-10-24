## Put comments here that give an overall description of what your
## functions do

## This function creates a list of functions to store the values of 
## a square matrix and it's inverse
## functions set() and get() store the value of the matrix and 
## return it's value respectively
## function setInverse() store the value of the inverse (calculated elsewhere)
## function getInverse() returns the value of the stored matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y){
    x = y
    inv <<- NULL
  }
  
  get <- function() x
  
  setInverse <- function(i) inv <<- i
  
  getInverse <- function() inv
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function looks for the inverse of the matrix store in the variable x passed
## x contains a list of functions as described above
## First it looks for a stored inverse, if there is no value, it computes the value
## stores it in the x$inv variable and returns it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if (!is.null(inv)){
      message("getting cached data")
      return(inv)
    }
    matrix <- x$get()
    inv <- solve(matrix)
    x$setInverse(inv)
    inv
    
}
