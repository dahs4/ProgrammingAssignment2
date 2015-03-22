# Assigment 2 for R Programming
# Compute and cache a matrix inversion, assuming that input matrix is invertible. If inversion already cached, then skip calculation and use cached inversion.
#     done in two steps: 1) function "makeCacheMatrix" to cache and retrieve the matrix inverse, and 2) function "cacheSolve" to compute the inverse if it does not exist, or retrieve existing inverse from makeCacheMatrix if it does.

# takes a matrix argument, and creates a list of functions that act on that matrix. initializes a variable "inverse" to be used to cache the computed inverse matrix
makeCacheMatrix <- function(x = matrix()) {
    inverse = NULL
    set <- function(y) { # store a matrix
        x <<- y
        inverse <<- NULL
    }
    get <- function() x # print stored matrix
    setinverse <- function(solve) inverse <<- solve # used by cacheSolve function to store the inverse matrix
    getinverse <- function() inverse # used by cacheSolve function to retrieve stored inverse matrix
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

# takes the makeCacheMatrix function as argument, and retrieves a stored matrix if there is one, and computes it if there isn't
cacheSolve <- function(x, ...) {
    inverse <- x$getinverse()
    if(!is.null(inverse)) { # checks for stored inverse
        message("Retrieving...")
        return(inverse)
    }
    data <- x$get() # gets the stored matrix, then computes the inverse, stores and caches it, then reports it
    inverse <- solve(data, ...)
    x$setinverse(inverse)
    inverse
}