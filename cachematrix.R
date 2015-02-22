# makeCacheMatrix builds our functions to get and set our matrices and their inverses.
# cacheSolve solves for the inverse of a matrix.
# both of these functions are used in this assignment to cache the inverse of a matrix.


# This function initializes functions that get and set the values of a matrix and its inverse.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# This function first checks if the inverse of a matrix exists, then returns it.
# If no inverse exists, it computes the inverse and then returns it.
# This function is assuming that the matrix is always invertible.
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}


