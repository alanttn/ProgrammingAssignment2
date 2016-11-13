## Cache the inverse of a matrix rather than compute it repeatedly with the 
## two functions below

## This function creates a special "matrix" object that can cache its inverse.
## Usage: <variable><-makeCacheMatrix(<matrix>)

makeCacheMatrix <- function(x = matrix()) {
        invX <- NULL
        
        ## set
        set <- function(y) {
                x <<- y
                invX <<- NULL
        }
        
        ## get
        get <- function() x
        
        ## setmean
        setSolve <- function(solve) invX <<- solve
        
        ## getmean
        getSolve <- function() invX
        
        list(set = set, get = get,
             setSolve = setSolve,
             getSolve = getSolve)

}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
## Usage: cacheSolve(<variable>)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invX <- x$getSolve()
        if(!is.null(invX)) {
                message("getting cached data")
                return(invX)
        }
        data <- x$get()
        invX <- solve(data, ...)
        x$setSolve(invX)
        invX
}
