##This function allows to store the matrix 
## and cache its inverse value after computation
makeCacheMatrix <- function(x = matrix()) {
    ##  cached inverse of the matrix
    i <- NULL
    
    ## set the matrix value and reset cache
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
        
    get <- function() x
    setInvertedValue <- function(inv) i <<- inv
    getInvertedValue <- function() i
    list(set = set, get = get,
         setInvertedValue = setInvertedValue,
         getInvertedValue = getInvertedValue)
}


##This function computes matrix inverse, if needed 
## or returns cached value if it is already calculated
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getInvertedValue()
    
    ## check if cached value exists
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setInvertedValue(i)
    i
}
