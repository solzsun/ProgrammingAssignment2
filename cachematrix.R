## Calculates the inverse of a matrix, and caches the solution.
## Subsequent calls will retrieve the cached solution,
## thus speeding up computation time

## Make a cached matrix, which is a list of 4 functions. 
# set: sets the value of the matrix
# get: get the current value of the set matrix
# setinv: set the matrix inverse (cache it!)
# getinv: get the cached matrix inverse
makeCacheMatrix <- function(x = matrix()) {
    
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(newInv) inv <<- newInv
    getinv <- function() inv
    
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
    
}


## First, get the cached matrix inverse.
## If it is not NULL, simply return this value (to save time)
## If it is NULL, calculate the inverse and cache for future calls.
cacheSolve <- function(x, ...) {
    
    inv <- x$getinv()
    if (!is.null(inv)) {
        message("Getting Cached Data!")
        return(inv)
    }
    
    data <- x$get()
    inv <- solve(data,...)
    x$setinv(inv)
    inv
}