## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix(x = matrix())
## returns a list of functions that set and get values of matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get, 
         setinv = setinv,
         getinv = getinv)
}


## cacheSolve(x, ...)
## returns the inverse of the given matrix x
## if the inverse is not cached, it caches the inverse to avoid calculating the next time

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
