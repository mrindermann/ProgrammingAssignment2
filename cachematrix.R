## makeCacheMatrix creates a list of 4 functions.
## these functions read in and print out a matrix 
## (set and get),
## and also store and print the inverse of the
## matrix (setinv and getinv).
## the inverse of the matrix is stored in the variable
## inv to reduce the workload in case the inverse of
## the matrix is requested more than once

## makeCacheMatrix is a matrix 'object' that stores
## information about a matrix and its inverse

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


## cacheSolve returns the inverse of a matrix. it first checks if 
## the inverse has already been calculated. If so it returns a 
## cashed result otherwise it will calculate the inverse and return
## it to the matrix 'object' to store the inverse for later use

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)){
        message('getting cached data!')
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinv(inv)
    inv
        ## Return a matrix that is the inverse of 'x'
}