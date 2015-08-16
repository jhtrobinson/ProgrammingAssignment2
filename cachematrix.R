## R Programming - Programming assignment 2 - Lexical Scoping

## Caching the inverse of a matrix in a data structure to avoid
## computing it multiple times

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

    # initialise the inverse to NULL
    inv <- NULL

    ## functions to get and set the matrixelf  Note
    ## that the inverse is overwritten with NULL when
    ## we reset the matrix; the old inverse is no 
    ## longer valid and we do not want to return it.
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x

    # functions to get and set the inverse
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv

    # list containing the functions
    list(
        set = set,
        get = get,
        setInverse = setInverse,
        getInverse = getInverse
    )
}


## This function will return the inverse of x
## It will return the cached inverse if it has
## already been calculated.  Otherwise it will compute the
## inverse using solve(), cache the inverse and return it.

cacheSolve <- function(x, ...) {
message("hello")
    ## return the cached inverse if it exists
    inv <- x$getInverse()
    if(!is.null(inv)){
        message("getting cached data")
        return (inv)
    }
    message("computing")
    ## the inverse has not been cached, so get
    ## the matrix itself. Then compute, cache and 
    ## return the inverse.
    mat <- x$get()
    inv <- solve(mat)
    x$setInverse(inv)
    inv
}
