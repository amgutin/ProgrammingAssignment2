## Functions makeCacheMatrix and cacheSolve are designed to compute
## inverse of a matrix efficiently. Specifically, after the inverse
## matrix is computed, the result is cached. Next time the inverse 
## of the matrix is requested, the cached result will be used
## instead of recomputing the inverse matrix. 

## makeCacheMatrix is a function with one argument, a matrix x. 
## The function returns a list of four functions:
## 1. set - sets the values of the original matrix
## 2. get - gets the values of the original matrix
## 3. setinv - sets the values of the inverse matrix
## 4. getinv - gets the values of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    sol <- NULL #sol is NULL if the inverse has not been computed
    set <- function(y) {
        x <<- y
        sol <<- NULL
    }
    get <- function() x
    setinv <- function(inv) sol <<- inv
    getinv <- function() sol
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## cacheSolve is a function that returns the inverse matrix. The 
## first argument is a list created by makeCacheMatrix.

cacheSolve <- function(x, ...) {
    ## Gets the inverse matrix if it has been computed.
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    ## Computes the inverse matrix and caches it.
    data <- x$get()
    inv <- solve(data, ...) 
    x$setinv(inv) 
    inv 
}
