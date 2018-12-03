## Matrix inversion is usually a costly computation and there may be some benefit to
## caching the inverse of a matrix rather than compute it repeatedly. The two functions
## defined below try to implementation a special matrix object that can cache its inverse

## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    invMetrix<- NULL

    set <- function(y) {
        x <<- y
        invMetrix <<- NULL
    }

    get <- function() x
    setinvMetrix <- function(inverse) invMetrix <<- inverse
    getinvMetrix <- function() invMetrix

    list(set = set, get = get,
         setinvMetrix = setinvMetrix,
         getinvMetrix = getinvMetrix)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve
## should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    invMetrix <- x$getinvMetrix()

    if( ! is.null(invMetrix)) {
        message("getting cached data.")
        return(invMetrix)
    }

    data <- x$get()
    invMetrix <- solve(data)
    x$setinvMetrix(invMetrix)
    invMetrix
}
