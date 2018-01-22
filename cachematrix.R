## Put comments here that give an overall description of what your
## functions do:
## The function cacheSolve together with makeCacheMatrix caches
## an inverse matrix for a given matrix.

## Write a short comment describing this function:
## 1. makeCacheMatrix - creates a special "matrix" object that 
## can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get, 
             setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function:
## 2. cacheSolve - computes the inverse of the special "matrix" 
## returned by makeCacheMatrix. If the inverse has already been 
## calculated, then cachesolve should retrieve the inverse from 
## the cache.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
        ## Return a matrix that is the inverse of 'x'
}
