## This file defines functions to create a special object that
## stores a numeric matrix and cache's its inverse.
## And to calculate the inverse of the special "matrix" created
## with the above function.
## They assume that the matrix supplied is always invertible
## (assignment's assumption).

## Creates a special "matrix", which can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        slv <- NULL
        set <- function(y) {
                x <<- y
                slv <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) slv <<- solve
        getsolve <- function() slv
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## Calculates the inverse of the "matrix" created with the above function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        slv <- x$getsolve()
        if(!is.null(slv)) {
                message("getting cached data")
                return(slv)
        }
        data <- x$get()
        slv <- solve(data, ...)
        x$setsolve(slv)
        slv
}
