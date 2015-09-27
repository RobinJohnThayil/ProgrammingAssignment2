## This file contains 2 functions:
## 1) makeCacheMatrix - creates a special "matrix" object that can cache its inverse.
## 2) cacheSolve      - This function computes the inverse of the special "matrix" returned by makeCacheMatrix(). 
##                    - If the inverse has already been calculated (and the matrix has not changed), 
##                      then the cachesolve should retrieve the inverse from the cache
## Assumptions:
## 1) The matrix supplied is always invertible

## makeCacheMatrix - creates a special "matrix" object that can cache its inverse.
##                 - Variable "currentMatrix" stores value of the matrix
##                 - Variable "inverseMatrix" stores value of the inverse
makeCacheMatrix <- function(currentMatrix = matrix()) {

        inverseMatrix <- NULL
        set <- function(y) {
                currentMatrix  <<- y
                inverseMatrix <<- NULL
        }
        get <- function() currentMatrix
        setinverse <- function(value) inverseMatrix <<- value
        getinverse <- function() inverseMatrix
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## cacheSolve      - This function computes the inverse of the special "matrix" returned by makeCacheMatrix()
##                 - If the inverse has already been calculated (and the matrix has not changed), 
##                   then the cacheSolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data)
        x$setinverse(i)
        i
}
