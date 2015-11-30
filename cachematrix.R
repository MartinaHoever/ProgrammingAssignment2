## The 'makeCacheMatrix' and 'cacheSolve' functions can be used to cache the
## inverse of a matrix, so it does not need to be calculated multiple times.

## The function 'makeCacheMatrix' has four functions in it:
##  - set
##  - get
##  - setInverseMatrix
##  - getInverseMatrix
## 
## The function 'set' will (re)set the value of x to the matrix that is 
## given when the function 'set' is called.
##
## The function 'get' will return the matrix.
##
## The function 'setInverseMatrix' will set the value of the variable 
## inverseMatrix to the matrix that is given when 'setInverseMatrix'is 
## called. Note that this not calculate the inverse matrix!
## 
## The function 'getInverseMatrix'will return the current value of the 
## variable InverseMatrix

makeCacheMatrix <- function(x = matrix()) {

    ## Set the value of the inverse matrix to NULL
    inverseMatrix <- NULL
    
    ## Set or change the cached matrix and
    ## set the value of the inverse matrix to NU
    set <- function(y) {
        x <<- y
        inverseMatrix <<- NULL
    }
    
    ## Retrieve the cached matrix
    get <- function() x
    
    ## Set the cached matrix inverse
    setInverseMatrix <- function(y) inverseMatrix <<- y
    
    ## Retrieve the cached matrix inverse
    getInverseMatrix <- function() inverseMatrix
    
    # Return internal function list
    list(set = set, get = get,
         setInverseMatrix = setInverseMatrix,
         getInverseMatrix = getInverseMatrix)
}

## This 'CacheSolve' function returns an inverse matrix of x.
## This function checks if the inverse matrix has already been calculated for x.
## If it has already been calculated, it will return the previously calculated 
## value. If it had not been calculated, it will calculate the inverse matrix 
## and save it and return this newly calculated inverse matrix.

cacheSolve <- function(x, ...) {
    
    ## Check the cache; if not null, the cached inverse can be returned
    im <- x$getInverseMatrix()
    if(!is.null(im)) {
        message("getting cached data")
        return(im)
    }
    data <- x$get()
    im <- solve(data, ...)
    x$setInverseMatrix(im)
    im
}
