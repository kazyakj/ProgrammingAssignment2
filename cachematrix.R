## This function creates a special "matrix" object that can cache its inverse
## Does NOT check if matrix is invertible

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    setMatrix <- function(y) {
        x <<- y
        m <<- NULL
    }
    getMatrix <- function() {
        x
    }
    setInverse <- function(inv) {
        m <<- inv
    }
    getInverse <- function() {
        m
    }
    list(setMatrix = setMatrix, getMatrix = getMatrix,
         setInverse = setInverse,
         getInverse = getInverse
     )
}


## cacheSolve computes the inverse of the "matrix" returned by makeCacheMatrix
## If  inverse is already calculated then cacheSolve will retrieve from cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$getMatrix()
    m <- solve(data, ...)
    x$setInverse(m)
    m
}
