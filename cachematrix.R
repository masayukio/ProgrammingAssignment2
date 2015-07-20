## Matrix inversion is usually a costly computation and there may be
## some benefit to caching the inverse of a matrix rather than compute
## it repeatedly.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    # set cached value is unset (NULL)
    inv <- NULL
    set <- function(mat) {
        x <<- mat
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inversion) inv <<- inversion
    getinv <- function() inv
    # return special "matrix object"
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned
## by makeCacheMatrix above. If the inverse has already been
## calculated (and the matrix has not changed), then the cachesolve
## should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    # search cached value
    inv <- x$getinv()
    # if cached value exists, return it
    if (!is.null(inv)) {
        # message("getting cached data") # for debug only
        return(inv)
    }
    # otherwise, calculate the inverse matrix, cache it
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    ## Return a matrix that is the inverse of 'x'
    inv
}
