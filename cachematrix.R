## The first function makeCacheMatrix creates a matrix,
## which contains functions to set the value of the matrix,
## to get the value of the matrix,
## to set the inverse of the matrix and
## to get the inverse of the matrix.


makeCacheMatrix <- function(x = matrix()) {
                m <- NULL
                set <- function(y) {
                        x <<- y
                        m <<- NULL
                }
                get <- function() x
                setinverse <- function(solve) m <<- solve
                getinverse <- function() m
                matrix(set = set, get = get,
                setinverse =setinverse,
                getinverse = getinverse)
}


## This function calculates the inverse of the matrix
## created with the above function. Before calculating the inverse,
## it first checks to see if the inverse has been already calculated.
## If this is the case, it gets the inverse from the cache and
## skips its calculating. If not, it calculates its inverse and
## sets the inverse value in the cache via the setinverse function.


cacheSolve <- function(x, ...) {
            m <- x$getinverse()
            if(!is.null(m)) {
                    message("getting cached data")
                    return(m)
            }
            data <- x$get
            m <- solve(data, ...)
            x$setinverse(m)
        ## Return a matrix that is the inverse of 'x'
            m
}
