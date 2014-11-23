-Initial-commit-for-Programming-Assignment-2-
=============================================
## 1. Initializes a variable 'm' 
##    (which will be used to save inverse matrix latter, i.e. a cached data);
## . Providing function get() to obtain "raw" matrix (of which one needs to find 
##    its inverse);
## Provides function setImatrix() to assign computed inverse matrix (of x) to m;
## Provides function getImatrix() to obtain the cached inverse matrix.

makeCacheMatrix <- function(x=matrix()) {
    m <- NULL
    get <- function() x
    setImatrix <- function(Imatrix) m <<- Imatrix
    getImatrix <- function() m

    # return a list of functions as an R object
    list(get=get, setImatrix=setImatrix, getImatrix=getImatrix)
}


## This function does the actual inversing of matrix x.  It first checks if the in-
## verse matrix has been found; if yes, returns the result and quits. If not, the 
## inverse of x is calculated, saved to cached, and returned.
## list returned from
##       calling makeCacheMatrix(x).

cacheSolve <- function(x, ...) {
    m <- x$getImatrix()
    if(!is.null(m)){
        message("Cached data found. Getting result... Done.")
        return(m)
    }
    else {
        message("No cached data found. Calculating inverse matrix...")
        data <- x$get() # obtains matrix from object x
        m <- solve(data) # finds inverse matrix
        x$setImatrix(m) # assigns resulting inverse matrix to object x
        message("Done.")
        return(m)
    }
}
