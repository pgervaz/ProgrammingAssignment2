## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## This function creates a special vector (list) with four functions, one that
## sets de value of the given matrix x, one that gets the value of the given
## matrix x, one that sets the inverse of the matrix x, and one that gets the
## value of inverse of the matrix x.


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## Write a short comment describing this function

## This function calculates the value of the inverse of a matrix taking by 
## argument the special vector (list) created with the previous function. 
## If the value has already been calculated, then it prints the previously
## stored value and a message saying "getting cached data", if the inverse
## nas not been calculated it calculates and stores the value.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
  
  
  
}
