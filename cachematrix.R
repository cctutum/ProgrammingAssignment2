# Matrix inversion is usually a costly computation and there may be 
# some benefit to caching the inverse of a matrix rather than computing 
# it repeatedly. The following two functions are used to cache the 
# inverse of a matrix.

# makeCacheMatrix() function prepares a list (size of 4) of functions. 
# It stores a matrix together with the cashed inverse of 
# it using the following four functions:
#
# 1. set(): stores the matrix
# 2. get(): returns the matrix
# 3. setInv(): stores the inverse of the matrix
# 4. getInv(): returns the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInv <- function(inverse) inv <<- inverse
    getInv <- function() inv
    list(set=set, get=get, setInv=setInv, getInv=getInv)
}


# cacheSolve() function returns the inverse of the matrix. 
# It first checks if the inverse has already been computed. 
# If it is already computed and cached, it gets the inverse of the matrix 
# via getInv() function and does not perform any computation. 
# Otherwise, it computes the inverse of the matrix, stores it 
# in the cache with setInv() function.

# ASSUMPTION: The matrix is always invertible.
cacheSolve <- function(x, ...) {
    inv <- x$getInv()
    # if the inverse of the matrix is available:
    if(!is.null(inv)) {
        message("cached data received!")
        return(inv)
    } else {
    data <- x$get()
    inv <- solve(data)
    x$setInv(inv)
    return(inv)
    }
}

## Sample run:
## > A <- matrix(c(1,2,3,2,0,4,3,4,5),nrow=3,ncol=3)
## > B <- makeCacheMatrix(A)
## > B$get()
##       [,1] [,2] [,3]
## [1,]    1    2    3
## [2,]    2    0    4
## [3,]    3    4    5

## No cache in the first run
## > invA <- cacheSolve(B)
## > invA
##            [,1]       [,2]       [,3]
## [1,] -1.3333333  0.1666667  0.6666667
## [2,]  0.1666667 -0.3333333  0.1666667
## [3,]  0.6666667  0.1666667 -0.3333333

## Retrieving from the cache in the second run
## > invA <- cacheSolve(B)
## cached data received!
## > invA
##            [,1]       [,2]       [,3]
## [1,] -1.3333333  0.1666667  0.6666667
## [2,]  0.1666667 -0.3333333  0.1666667
## [3,]  0.6666667  0.1666667 -0.3333333
