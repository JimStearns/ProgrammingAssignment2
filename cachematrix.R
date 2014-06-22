# Programming Assignment #2, R Programming, 
# Course #2 of Johns Hopkins Data Science sequence on Coursera.

# Submitter: Jim Stearns
# Date: 22 June 2014

# Starts with template and function prototypes supplied by instructor.
# Please see git history.

# makeCacheMatrix: creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    x.inv <- NULL
    set <- function(y) {
        # Use "<<-" operator to save matrix in variable scoped in function makeCacheMatrix.
        x <<- y
        x.inv <<- NULL
    }
    get <- function() x
    # Use "<<-" operator to save matrix in variable scoped in function makeCacheMatrix.
    setinverse <- function(inverse) x.inv <<- inverse
    getinverse <- function() x.inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

# cacheSolve: compute and return the inverse of the special "matrix" returned 
# by makeCacheMatrix above. 
#
# If the inverse has already been calculated (and the matrix has not changed), 
# then cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    
    # Return a matrix that is the inverse of 'x'
    x.inv <- x$getinverse()
    if (!is.null(x.inv)) {
        message("Using cached inverse")
        return(x.inv)
    }
    xtoinvert = x$get()
    x.inverted <- solve(xtoinvert)
    x$setinverse(x.inverted)
    return(x.inverted)
}
