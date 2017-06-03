##
# R Programming - Week 3
# Programming Assignment 2
#
# Find the inverse of an invertible (nonsingular) square matrix. Cache the
# result for duplicate calls to solve for the inverse.
#
# We assume that the matrix is always invertible
#
# To test type the following statements in RStudio:
#
#       aMatrix <- matrix(c(4,3,3,2),nrow=2,ncol=2) # create 2x2 matrix
#       aMakeMatrix <- makeCacheMatrix(aMatrix)     # create caching object
#       aMatrixinv <- cacheSolve(aMakeMatrix)       # calculate the inverse
#       aMatrixinv <- cacheSolve(aMakeMatrix)       # use the cached inverse
#       aMatrix %*% aMatrixinv                      # identity matrix
#
# makeCachMatrix - instantiate a matrix caching object in the 
#                  R environment
##
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get    <- function() x
    setinv <- function(solve) m <<- solve
    getinv <- function() m
    list ( set = set, 
           get = get, 
           setinv = setinv, 
           getinv = getinv
         )
}
#
# cacheSolve - determines the inverse of a square matrix.
#              If the matrix has already been calculated
#              it returns the cached value.
#
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinv()
    if ( !is.null(m)){
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setinv(m)
    m
}
