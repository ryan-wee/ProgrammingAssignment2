## These functions calculates and caches the inverse matrix of a given invertible matrix.

## This function creates a matrix object to be passed into cacheSolve

makeCacheMatrix <- function(x = matrix()) {
		m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(msolve) m <<- msolve
        getInverse <- function() m
        list(set = set, get = get, setInverse = setInverse, 
             getInverse = getInverse)
}


## this function calculates the inverse matrix of the matrix object from the above function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}

