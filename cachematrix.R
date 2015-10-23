## makeCacheMatrix creates a special "matrix" object that can cache its inverse
## N ote: Does NOT check if matrix is invertible
makeCacheMatrix <- function(x = matrix()) {
    
    m <- NULL
    
    ## set matrix to be inverted
    setMatrix <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    #get matrix to be inverted
    getMatrix <- function() {
        x
    }
    
    #set inverse of matrix
    setInverse <- function(inv) {
        m <<- inv
    }
    
    ## get inverse of matrix
    getInverse <- function() {
        m
    }
    
    list(setMatrix = setMatrix, getMatrix = getMatrix,
         setInverse = setInverse,
         getInverse = getInverse
     )
}


## cacheSolve computes the inverse of the "matrix" returned by makeCacheMatrix
## If inverse is already calculated then cacheSolve will retrieve from cache.
cacheSolve <- function(x, ...) {

    
    ## If inverse matrix already stored in cache 
    ## then return it
    m <- x$getInverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    ## if inverse matrix does NOT exist in cache
    ## then compute, store, and return it.
    m <- solve(x$getMatrix(), ...)
    x$setInverse(m)
    m
}
