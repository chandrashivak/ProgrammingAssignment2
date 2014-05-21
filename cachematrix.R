## The library cachematrix. R contains two functions
## 1. makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## 2. cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

## The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse of matrix
## get the value of the inverse of matrix

makeCacheMatrix <- function(x = numeric()) {
        m <- NULL
        ## Set the value of matrix
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        ## Get the values of Matrix
        get <- function() x
        ## Setting the inverse of Matrix
        setinv <- function(inv) m <<- inv
        ## Getting the inverse of matrix
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}
## Finding the inverse if it already does not exist, Otherwise, it gets it from Cache
cacheSolve <- function(x, ...) {
        ## Getting the existing inverse of the matrix
        m <- x$getinv()
        ## if the existing inverse is not null, it retreiving from Cache
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        ## if the existing inverse is null, that means the data is either new or has been changed
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
