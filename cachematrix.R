
## This file contains R-functions that cache the inverse of a matrix,
## in a closure, so future calls to get the inverse may return the
## cached inverse of the matrix (instead of recomputing the inverse)


## Given a matrix, this function returns an object that caches the matrix and
## its inverse with the accessors and setters for the matrix and its inverse.

makeCacheMatrix <- function (x = matrix()) {
    inv <- NULL
    
    set <- function (mat) {
        x <<- mat        # setting new matrix
        inv <<- NULL     # so, clear the cached inverse also
    }
    get <- function () x
    setinv <- function (solution) inv <<- solution
    getinv <- function () inv
    
    list (set = set, get = get, setinv = setinv, getinv = getinv)
}


## Takes an object x created by makeCacheMatrix and
## returns the inverse of the matrix data from the object x.

cacheSolve <- function (x, ...) {
    inv <- x$getinv()
    if (!is.null(inv)) inv else x$setinv(solve(x$get(), ...))
}
