## Assignment 2

## Function that creates a special matrix that initializes the value of a matrix 
## and its inverse along with defining functions to set and get the value of its 
## matrix and its inverse.  Assigning a matrix using this function enables one 
## to check if the value of the matrix inverse has been assigned

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Takes a matrix assigned using the function makeCacheMatrix.  If the inverse
## has not been calculated (inv = NULL), then perform the calculation.  If the 
## inverse has been calculated, return the value in memory

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)){
        message("Getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
