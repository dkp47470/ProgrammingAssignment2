## R-Programming Class Programming Assignment #2
##  CacheMatrix.R - Inverse Matrix Cache Program
##   David Patterson (GitHub: dkp47470, Email: david.k.patterson@gmail.com)
##
## makeCacheMatrix: This function creates a special "matrix" object 
##   that can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix"
##   returned by makeCacheMatrix above. If the inverse has already been
##   calculated (and the matrix has not changed), then the cachesolve will
##   retrieve the inverse from the cache.

makeCacheMatrix <- function(x = matrix()) {
## Returns a list containing functions to
##      1. Set the matrix
##      2. Get the matrix
##      3. Set the inverse
##      4. Get the inverse
##     This function is called by cacheSolve()
    
    invertedMx <- NULL
    set <- function(y) {
        x <<- y
        invertedMx <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) invertedMx <<- inverse 
    getinv <- function() invertedMx
    list(set=set, get=get, setinv=setinv, getinv=getinv)
}


cacheSolve <- function(x, ...) {
    ## Returns a matrix that is the inverse of 'x'
    
    invertedMx <- x$getinv()
    
    # Use the cached inverse value if exists 
    if (!is.null(invertedMx)){
        message("getting cached data")
        return(invertedMx)
    }
    
    # ... otherwise, calculate the inverse of the matrix 
    data <- x$get()
    invertedMx <- solve(data, ...)
    
    # sets the value of the inverse in the cache via the setinv function.
    x$setinv(invertedMx)
    
    return(invertedMx)
}