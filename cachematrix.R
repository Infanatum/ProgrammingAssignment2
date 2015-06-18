## Functions for cache and compute the inverse of a matrix below: 

## This function creates the object "matrix"
## that can cache its inverse.

makeCacheMatrix <- function(mtrx = matrix()) {
    inverse <- NULL
    set <- function(x) {
        mtrx <<- x;
        inverse <<- NULL;
    }
    get <- function() return(mtrx);
    setinv <- function(inv) inverse <<- inv;
    getinv <- function() return(inverse);
    return(list(set = set, get = get, setinv = setinv, getinv = getinv))
}


## This function computes the inverse of the
## "matrix" returned by function above.

cacheSolve <- function(mtrx, ...) {
    inverse <- mtrx$getinv()
    if(!is.null(inverse)) {
        message("Getting cached data")
        return(inverse)
    }
    data <- mtrx$get()
    invserse <- solve(data, ...)
    mtrx$setinv(inverse)
    return(inverse)
}