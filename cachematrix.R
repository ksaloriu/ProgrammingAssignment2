## A closure implementing a matrix than can cache its inverse
## and a solver function for finding either the pre-cached inverse
## or for calculating and then caching the inverse.

## Create a list three functions that together implement a
## matrix that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    getMatrix <- function() x
    getInverse <- function() inverse
    setInverse <- function(inv) inverse <<- inv
    list(getMatrix=getMatrix, getInverse=getInverse, setInverse=setInverse)
}

## Return the inverse of a matrix created with makeCacheMatrix. If the
## inverse has been already calculated, a cached inverse is directly
## returned with no computation necessary. If no cached inverse is
## found it is calculated, stored and returned.

cacheSolve <- function(x, ...) {
    inverse <- x$getinverse()
    if (is.null(inverse)) {
        inverse <- solve(x$getmatrix())
        x$setinverse(inverse)
    }
    inverse
}

