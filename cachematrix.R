##The makeCacheMatrix function creates a special "matrix"
##which is really a list containing functions to
##set: set the value of matrix
##get: get the value of matrix
##setInverse: set the inverse of the matrix
##getInverse: get the inverse of the matrix
##Usage Example:
##mdat <- matrix(c(1L,0L,5L,2L,1L,6L,3L,4L,0L), nrow = 3, ncol = 3, byrow = TRUE)
##c <- makeCacheMatrix(mdat)
##cacheSolve(c)
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() {
                x
        }
        setInverse <- function(invMtx) { 
                m <<- invMtx
        }
        getInverse <- function() {
                m
        }
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

##The cacheSolve function calculates the inverse of the special "matrix" created with the makeCacheMatrix function. 
##However, it first checks to see if the inverse matrix has already been calculated. 
##If so, it gets the inverse matrix from the cache and skips the computation. 
##Otherwise, it calculates the inverse matrix and sets inverse matrix in the cache via makeCacheMatrix$setInverse function.
cacheSolve <- function(x, ...) {
        ## get inverse matrix of x from cache 
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        mtx <- x$get()
        m <- solve(mtx, ...)
        x$setInverse(m)
        m        
}
