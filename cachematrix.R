
## This file contains functions that provide support for potentialy 
## time consuming  recomputation of the inverse of a matrix


## Create "matrix" object (a list) that caches matrix inverse
##
## @param x Matrix whose inverse is to be cached
## @return A list of helper functions that set/get matrix
##     and cache/return the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
    xinv <- NULL
    set <- function(y) {
        x <<- y
        xinv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) xinv <<- inverse
    getInverse <- function() xinv
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Computes inverse of a matrix using caching
##
## @param x Matrix to be inverted
## @return Inverse of the input matrix

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    xinv <- x$getInverse()
    if(!is.null(xinv)) {
        message("getting cached data")
        return(xinv)
    }
    matrix <- x$get()
    inverse <- solve(matrix, ...)
    
    x$setInverse(inverse)
    inverse
}
