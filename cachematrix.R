
## makeCacheMatrix creates a special "matrix" object that can cache its inverse
## while returning a list of functions to get and set values
##
makeCacheMatrix <- function(x = matrix()) {
    # cached inverse is NULL until setInverse is called
    inv <- NULL
    
    # assigns the data to x and clear the cached inverse
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    # returns x
    get <- function() x
    
    # sets the cached inverse apparantly to whatever is passed
    setInverse <- function(inverse) inv <<- inverse
    
    # returns the inverse which may still be NULL
    getInverse <- function() inv
    
    # return a list of functions that manipulate the data of the cacheMatrix
    return(list(set = get, get = get,
                setInverse = setInverse, getInverse = getInverse))
}


## cacheSolve computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the
## inverse from the cache.
##
cacheSolve <- function(x, ...) {
    # check if the inverse of x already exists
    if(!is.null(x$getInverse())) {
        # let it be known that the inverse was cached so we know
        # we're not unnecessarily computing it
        message("getting cached data")
        
        # return the cached inverse
        return(x$getInverse())
    }

    # the inverse has not been calculated yet
    # solve the input parameter's matrix for its inverse
    x$setInv(solve(x$get()))

    # return the inverse
    x$getInverse()
}


# ## my minimal test driver
# ## ctrl+shft+c to (un)comment blocks of code
#
# setwd("~/OnlineEDU/Coursera/DS_Specialization/R_Programming/Assignment_2/
#      ProgrammingAssignment2")
# source("cachematrix.R")
# 
# # make a random square nxn matrix between 2x2 and 10x10
# n <- sample(2:10, 1)
# M <- makeCacheMatrix(matrix(rnorm(n * n), n, n))
# 
# # soleve it twice to check if the cached inverse is persistent
# cacheSolve(M)
# cacheSolve(M)