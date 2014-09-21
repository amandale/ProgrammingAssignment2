## This pair of functions cache the inverse of a matrix.

## Creates a matrix to cache. 
makeCacheMatrix <- function( m = matrix() ) {

    x <- NULL

    ##Initial set/get functions
    set <- function( matrix ) {
            m <<- matrix
            x <<- NULL
    }

    get <- function() {
    	m
    }

    ## set/get functions for inverse matrix
    setInverse <- function(inverse) {
        x <<- inverse
    }

    getInverse <- function() {
        x
    }

    ## return list
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


##retrieves the inverse  of x 
cacheSolve <- function(x, ...) {

    ## intital cases
    m <- x$getInverse()
    if( !is.null(m) ) {
            message("Getting cached data...")
            return(m)
    }

    data <- x$get()

    ## solve for inverse
    m <- solve(data) %*% data
    x$setInverse(m)

    ## return
    m
}
