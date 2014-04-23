## These function have the purpose of caching the computation of the inverse
## of a matrix

## makeCacheMatrix : creates a list storing the necessary computations
##      to get the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
    ## Initalisation of the matrix m
    m <- NULL
    ## Function to set the default values
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    ## Function to get the value of matrix x
    get <- function() x
    ## Function to store the value of the inverse of matrix x
    setinv <- function(solve) m <<- solve
    ## Function to get back the value of the inverse of matrix x
    getinv <- function() m
    ## Return values as a list
    list(set = set, get = get,
            setinv = setinv,
            getinv = getinv)

}


## cacheSolve : get either the stored value of the inverse of a matrix or
##      compute this value if it is not stored

cacheSolve <- function(x, ...) {
    ## Assign the potentially stored value of inverse matrix x to the variable m
    m <- x$getinv()
    ## If there is something stored in the list, we don't compute
    ## but we get the cached data
    if(!is.null(m)) {
        message("getting cached data")
        ## return the value of m and exit the function
        return(m)
    }
    ## Assign the value of matrix x to the variable data
    data <- x$get()
    ## Compute the inverse of the matrix
    m <- solve(data, ...)
    ## Store the result of computation
    x$setinv(m)
    ## Return the result
    m
}
