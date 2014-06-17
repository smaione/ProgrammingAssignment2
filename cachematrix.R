## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    # the cached inverse will remain null until set
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
           setInverse = setInverse,
           getInverse = getInverse))
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the
## inverse from the cache.
cacheSolve <- function(x, ...) {
    # check if the inverse of x already exists
    if(!is.null(x$getInverse())) {
        # if so, return it
        return(x$getInverse())
    }

    # the inverse has not been calculated yet
    # solve the input parameter's matrix for its inverse
    x$setInv(solve(x$get()))

    # return the inverse
    x$getInverse()
}
