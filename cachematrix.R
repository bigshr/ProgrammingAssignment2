## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a special "matrix", 
## which is really a list containing a function to :
##      1. set the matrix
##      2. get the matrix
##      3. set the inverse matrix
##      4. get the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        # The cached inverse of matrix x
        i <- NULL
        
        # This closure method sets the data(i.e. matrix)
        # and clears the cached value to NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        
        # This closure returns the matrix data
        get <- function() x
        
        # This closure sets inverse of matrix x to the cache
        setinv <- function(inv) i <<- inv
        
        # This closure gets inverse of matrix from the cache
        getinv <- function() i
        
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve returns the inverse of the matrix. It first checks if the inverse 
## has already been computed. If so, it gets the result and skips the
## computation. If not, it computes the inverse, sets the value in the cache 
## using setinverse function.
## assumption: matrix is always invertible.

cacheSolve <- function(x, ...) {
        # fetching inverse from cache
        i <- x$getinv()
        
        # checking whether cached value already exists
        if(!is.null(i)) {
                message("getting cached data")
                
                # returning cached inverse matrix
                return(i)
        }
        
        # if cached value does not exist then
        # fetching matrix data for which inverse
        # is to be calculatd
        data <- x$get()
        
        # using solve function to calculate inverse of the matrix
        # function solves equation x %*% i = b for x, where b is 
        # a vector or matrix and %*% is matrix multiplication. 
        # for i to be inverse of the matrix x, b should be an identity matrix
        # if the value of b is not specified in solve fuction then
        # identity matrix is assumed
        # the value returned by solve function is i which in our case 
        # is the inverse matrix
        # for more information about solve use ?solve
        # ref: http://www.endmemo.com/program/R/solve.php
        i <- solve(data, ...)
        
        # storing the computed inverse matrix in cache
        x$setinv(i)
        
        # returning the computed inverse matrix
        i
}
