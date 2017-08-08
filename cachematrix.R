## These functions take in a square matrix and output the inverse. If the
## computation has already been completed it retrieves the answer from
## the cache. If not it computes the answer.

## This function reads in a square matrix, calculates the inverse, and then
## caches the result.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function takes a matrix and then either prints the value of the
## inverse stored in cache or computes the result.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
