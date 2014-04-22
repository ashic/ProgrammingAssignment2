## The solution consists of two functions:
## mackeCacheMatrix is used to initialise a cache from a given matrix.
## cacheSolve takes in a list returned by a previous call to makeCacheMatrix(...) and returns the inverse of the target matrix.


## makeCacheMatrix takes in a matrix and returns a list with members that refer to functions
## which can be used to get or set the underlying matrix, and to get and set the value of the 
## the inverse. These values are cached, and the inverse is re-used unless the underlying matrix changes.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL #holds the cached value
    
    ## replace the underlying matrix
    set <- function(y) {
        ##replace the underlying matrix in the 'outer' environment.
        x <<- y
        ##matrix has changed, so the cache of the inverse should be invalidated.
        inv <<- NULL
    }
    
    ## returns the underlying matrix.
    get <- function() x
    
    ## store the cached inverse
    setInverse <- function(inverse) inv <<- inverse
    
    ## fetch the inverse
    getInverse <- function() inv
    
    ## return a list that can be used to call the functions.
    list(set = set, get = get,
        setInverse = setInverse,
        getInverse = getInverse)    
}


## cacheSolve takes in a list returned by makeCacheMatrix and returns the inverse of the target matrix.
## It attempts to fetch the inverse from the cache. If not found, then it calculates the inverse and 
## caches it. 
## Note: The value for a matrix is cached the first time cacheResolve(...) is called. Subsequent calls to
## cacheResolve return the cached value. Any changes to additional (i.e. ...) arguments are ignored after the
## first call.

cacheSolve <- function(x, ...) {
    ## try to fetch cached value
    inv <- x$getInverse()
    
    if(!is.null(inv)) {
        ## cache hit
        message("getting cached data")
        return(inv)
    }
    
    ## cache miss. calculate the inverse, cache it, then return,
    data <- x$get()
    
    message("data not found in cache. this is either a new matrix, or it has been changed since the inverse was cached.")
    message("calculating the inverse...")
    message("warning: only first calculation is cached.")
    message("Subsequent calls using different additional arguments will return the initially cached value.")
    
    inv <- solve(data, ...)
    x$setInverse(inv)
    inv
}
