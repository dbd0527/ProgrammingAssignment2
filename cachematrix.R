## These two functions cache the inverse of a matrix,
## which may be helpful if a matrix's inverse has to be
## computed repeatedly. By caching the inverse, it can be
## looked up in the cache more quickly than recomputing it.

## Here's an example of commands for these functions:
## First, enter the matrix whose inverse you want.
##       > x <- matrix(c(-1, -2, 1, 1), 2, 2)
##       > a <- makeCacheMatrix(x)
## Then, get the inverse.
##       > cacheSolve(a)

## The makeCacheMatrix function creates a special 'matrix' object
## that can cache (but does not compute) its inverse.
## It is actually just a list of four functions to:
##      set the value of the matrix (setMat)
##      get the value of the matrix (getMat)
##      set the value of the inverse (setInvMat)
##      get the value of the invoice (getInvMat)

makeCacheMatrix <- function(x = matrix()) {
        ## x is the matrix first inputted into makeCacheMatrix.
        ## Clear out the object im, which will store x's inverse.
        im <- NULL
        
        ## If you want to change the matrix stored in makeCacheMatrix,
        ## you can use this setMat function:
        setMat <- function(y = matrix()) {
                x <<- y
                im <<- NULL
        }
        
        ## getMat returns the matrix stored in makeCacheMatrix.
        getMat <- function() {x}
        
        ## setInvMat stores an inputted inverse into makeCacheMatrix.
        ## Inverse would be set by the cacheSolve function.
        setInvMat <- function(InvMat = matrix()) {
                im <<- InvMat
        }
        
        ## getInvMat returns the inverse stored in makeCacheMatrix
        getInvMat <- function() {im}
        
        list (setMat = setMat,
              getMat = getMat,
              setInvMat = setInvMat,
              getInvMat = getInvMat)
}

## The cacheSolve function returns the inverse of a square matrix
## either by getting it from the cache, 
## or by computing it and then storing it in the cache. 

cacheSolve <- function(x, ...) {

        ## Checks cached value of getInvMat in x and moves it to im
        im <- x$getInvMat()
        
        ## If there is a value in im, get it
        if (!is.null(im)) {
                message ("Getting cached inverse.")
                return(im)
        }
        
        ## else, there is not a cached inverse in x,
        ## so get the matrix from makeCacheMatrix 
        ## and calculate its inverse
        matrix <- x$getMat()
        im <- solve(matrix, ...)
        
        ## Then set the value of the inverse in the cache
        ## via the setInvMat function
        x$setInvMat(im)
        return(im)
}
