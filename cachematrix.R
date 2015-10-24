## Put comments here that give an overall description of what your
## functions do

## Description: makeCacheMatrix is a function to create a special matrix object
## which can cache its own inverse. 
## It sets a value of a matrix, gets the value of the matrix, sets the value of
## the inverse, and get the value of the inverse, and returns these as a list.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        
        ## return a list with the following 4 values
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Description: cacheSolve is a function to calculate the inverse of a matrix,
## after first checking to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache, avoiding this costly computation.
## If not, it will perform the caclulation and put the value in the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
