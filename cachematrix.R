## R Programming Assignment 2
## Matrix that can cache its inverse

## Creates a "matrix" that can cache its inverse ("solve")
makeCacheMatrix <- function(x = matrix()) {
    ## initialize cache
    s <- NULL
    
    ## create get/set functions
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() {
        x
    }
    setSolve <- function(inverse) {
        s <<- inverse
    }
    getSolve <- function() {
        s
    }
    
    ## return the functions
    list(set = set, get = get, setSolve = setSolve, getSolve = getSolve)
}


## Calculates and caches inverse, or returns the cached inverse
cacheSolve <- function(x, ...) {
    ## Check for cached inverse
    s <- x$getSolve()
    if (is.null(s)) {
        ## No cached inverse; calculate and cache it
        s <- solve(x$get())
        x$setSolve(s)
    } else {
        ## Cached inverse found
        message("returning cached data")
    }
    ## Return the inverse
    s
}
