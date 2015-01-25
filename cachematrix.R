## This file implements a cache mechanism for Matrix inversion.
## This mechanism is implementated through a cacheMatrix list structure and 
## use parent environment to cache inversion results. This structure is 
## used by a custom Solve function cacheSolve that take a cacheMatrix
## as an input an try to retrieve the cached inverse if available.
## Otherwise, it calculate the inverse and populate the cache.
## 
## 
## This file contains definition of the functions makeCacheMatrix and cacheSolve
## *****************************************************************************

## makeCacheMatrix :`
## *****************
## makeCacheMatrix creates CacheMatrix "object" with 4 function, that can store  
## a matrix and its inverse.
## The functions set() and get(x) are used to set or get matrix data.
## The function setinv(x) is used to cached the inverse of the matrix
## The funciton getinv() is used to retrieve inverse cached value.
##
## In reality the "object" is a list of the 4 function that set and
## access the cached value and datas in parent environment.

makeCacheMatrix <- function(x = matrix()) {
        set <- function(y) {
                x   <<- y
                inv <<- NULL
        }
        
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        cache_s <- list(set = set, get = get, setinv= setinv, getinv = getinv)
        cache_s$set(x)
        cache_s
}


## cacheSolve :`
## ************
## cacheSolve is a custom Solve function. It takes a cacheMatrix list as input.
## It first test if the inverse cache is populated.
## If there is an inverse cached , it retrieves it from the environment 
## using getinv() and skip the computation.
## Otherwise it retrieves the matrix datas using get(), 
## compute inverse of the matrix using function solve
## and finally cache the result using setinv().
## The matrix inverse is returned in both case.

cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinv(inv)
        inv
}
