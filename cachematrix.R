## Script created for programming assignment 2. Available in my github repository
## This script contains two functions:
## makeCacheMatrix: Creates a list that stores a matrix in the cache as well as
##                  it inverse
## cacheSolve: Takes a list created with makeCacheMatrix as input and returns the
##             inverse. First looks if the inverse is already in cache, if so,
##             retrieves de inverse, if not, stores the matrix and the inverted
##             matrix in cache.

makeCacheMatrix <- function(x = matrix()) {
        ## Create a null matrix to store de inverse
        x_inv <- matrix()
        ## setx stores in cache the x matrix and x_inv matrix
        setx <- function (y) {
                x <<- y
                x_inv <<- NULL
        }
        ## getx return the matrix
        getx <- function() x
        ## setinv stores in cache inverse of matrix x
        setinv <- function(y_inv) x_inv <<- y_inv
        ## getinv return the inverted matrix
        getinv <- function() x_inv
        list(setx = setx, getx = getx, setinv = setinv, getinv = getinv)
}


## cacheSolve: Takes a list created with makeCacheMatrix as input and returns the
##             inverse. First looks if the inverse is already in cache, if so,
##             retrieves de inverse, if not, stores the matrix and the inverted
##             matrix in cache.

cacheSolve <- function(x, ...) {
        ## Validates if we already have an inverse
        my_inv <- x$getinv()
        if (!is.na(my_inv)) {
                return(my_inv)
        }
        ## my_inv for x is not already calculated, we compute it
        my_matrix <- x$getx()
        my_inv <- solve(my_matrix)
        x$setinv(my_inv)
        my_inv
}
