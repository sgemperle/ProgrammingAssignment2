## R Programming
## Programming Assignment 2

## Author: S. Gemperle
## 30 March 2016


## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix.
## Following are a pair of functions that cache the inverse of a matrix.

## The makeCacheMatrix function creates a list with four functions:
##      1. set the value of a matrix
##      2. get the value of the matrix
##      3. set the value of the inverse of the matrix
##      4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## The cacheSolve function computes the inverse of the matrix. It first checks if the inverse has already been calculated.
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}


## Example

x <- matrix(rnorm(9), 3, 3)
mat <- makeCacheMatrix(x)
mat$get()
cacheSolve(mat)
cacheSolve(mat)
