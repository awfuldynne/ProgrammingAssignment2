## 
## makeCacheMatrix - Creates a matrix object that can cache its inverse
## cacheSolve - Retrieves the inverse of a matrix object returned from 
##              makeCacheMatrix

## makeCacheMatrix creates a list containing the follow functions:
##  set - set the value of the matrix
##  get - get the value of the matrix
##  setInverse - set the value of the inverse of the matrix
##  getInverse - get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setInverse <- function(i) inverse <<- i
    getInverse <- function() inverse
    list(set = set, get = get, setInverse = setInverse,
         getInverse = getInverse)
}


## cacheSolve returns the inverse of a matrix object returned from 
## makeCacheMatrix.
## If the inverse has already been computed, it returns the cached inverse.

cacheSolve <- function(x, ...) {
    i <- x$getInverse()
    
    if(!is.null(i)) {
        message("getting cached inverse")
        return(i)
    }
    
    data <- x$get()
    i <- solve(data, ...)
    
    x$setInverse(i)
    
    return(i)
}
