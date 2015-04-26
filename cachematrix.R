## This two funcitons provide the functionlity to calculation and cache the inverse
## matrix. The mackeCacheMatrix is a wrapper and the cacheSolve calculates and polulates
## the matrix.

## The makeCacheMatrix is a wrapper around a matrix providing getter and setter 
## funcitons for teh initial matrix and its inverse.
makeCacheMatrix <- function(x = matrix()) {
    
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## The cacheSolve function provides teh functionality to get either the cached
## inverse matrix or calculate and store it.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
