## Together the functions below return the inverse of a square matrix, either from
## cache if available or calculated anew if unavailable.
## Created by Tuyet Hayes on Saturday, March 18th, for R Programming Coursera course.

## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        
        ## set value of matrix
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
                
        ## get value of matrix              
        get <- function() x
        
        ## set value of inverse
        setinverse <- function(inverse) i <<- inverse
        
        ## get value of inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes the inverse of the special square "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then cacheSolve retrieves the inverse from 
## the cache.  

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        ## checks to see if the inverse has already been calculated
        i <- x$getinverse()
        
        ## returns data from cache if inverse is already calculated
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        
        ## calculates the inverse of the data and sets the value of the inverse 
        ## in the cache via the setinverse function
        
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
