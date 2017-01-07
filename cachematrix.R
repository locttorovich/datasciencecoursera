# Matrix inversion is usually a costly computation, and therefore there may be
# some benefit to caching the inverse of a matrix rather than computing it
# repeatedly.
#
# The two functions makeCacheMatrix and cacheSolve work together to calculate an
# inverse matrix, store it in cache, and pull from cache when needed again, 
# avoiding the need for multiple calculations.
#
# Note: the functions assume that the matrix is square and invertible.

# makeCacheMatrix returns a list containing functions to set the matrix,
# get the matrix, set the inverse and return the inverse. This list is 
# the input to the cacheSolve function,which pulls the cached version, if
# it exists, or calcs and stores it if it does not.

makeCacheMatrix <- function(x = matrix()) {

    inv <- NULL 
    
    #set the variables in the cache environment
    set <- function(y) {  
        x <<- y
        inv <<- NULL
    }
    
    #define functions to set and retreive variables
    
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    
    #create output list to pass to cacheSolve
    
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

# cacheSolve takes the function list from makeCacheMatrix, and uses it to 
# retreive the cached inverse matrix, if it exists, or to calculate and
# cache the inverse matrix, if it does not.

cacheSolve <- function(x, ...) {
    
    inv <- x$getinv()
    
    # check if cached inverse already exists; if so, return it
    if(!is.null(inv)) { 
        message("getting cached data")
        return(inv)
    }
    
    # if cached inverse not found, compute the inverse and cache it
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv     
}
