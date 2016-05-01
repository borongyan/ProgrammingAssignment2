## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    #initiate the cache m
    m <- NULL
    set <- function(y) {
        x <<- y ## assign the input matrix y to the variable x in the
        ## parent environment
        m <<- NULL  ## re-initialize m in the parent environment to null
    }
    #get the output of matrix x
    get <-function() x
    #set the cache m equal to the inverse of x
    setInverse <- function(inverse) m <<- inverse
    #get the inverse value of x
    getInverse <- function() m
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse() 
    if(!is.null(m)) { 
        ##Print the results
        message("getting cached data")
        return(m)
    }
    # inverse of the original data
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m
}
