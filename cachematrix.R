## These functions calculate the inverse of an input matrix and stores it in a
## cache. If the inverse of the input matrix already has been calculated, the
## calculation is skipped and the inverse matrix is instead loaded from the
## cache. This function assumes that the input matrix is nonsingular.

## The first function creates a set of functions stored in a list. These
##functions are used later in the subsequent function.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        get <- function () x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list(get = get,
             setinv = setinv,
             getinv = getinv)
        
}

##The second function uses as an input the output from the first function. It
##calculates the inverse of the matrix originally input in the first function.
##It checks whether the inverse of the input matrix already has been calculated. 
##If so, the calculation is skipped, and instead the inverse matrix is loaded
##from the cache. If not, the inverse of the input matrix is calculated and
##stored in the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}