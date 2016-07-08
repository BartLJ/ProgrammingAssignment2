## The functions 'makeCacheMatrix' and 'cacheSolve' allow to create a special 'matrix' object
## that caches the value of its inverse matrix.

## The function 'makeCacheMatrix' creates a special 'matrix' object, containing a list of functions to:
## - set the value of the matrix
## - get the value of the matrix
## - set the value of the inverse matrix
## - get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    inverse_matrix <- NULL
    set <- function(y) {
        x <<- y
        inverse_matrix <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inverse_matrix <<- inverse
    getinverse <- function() inverse_matrix
    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## The function 'cacheSolve' calculates the inverse matrix of the special 'matrix' created with
## the function 'makeCacheMatrix'. At the first call of the function, the inverse matrix is
## calculated and cached. At every next call, the cached value is returned.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse_matrix <- x$getinverse()
    if(!is.null(inverse_matrix)) {
        message("getting cached data")
        return(inverse_matrix)
    }
    data <- x$get()
    ## Assume for this assignment that 'data' is an invertible matrix
    inverse_matrix <- solve(data)
    x$setinverse(inverse_matrix)
    inverse_matrix
}
