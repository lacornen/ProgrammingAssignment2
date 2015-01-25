## This file implements a cache mechanism for Matrix inversion.
## This mechanism is implementated through a cacheMatrix list structure and 
## use parent environment to cache inversion results. This structure is 
## used by a custom Solve function cacheSolve that take a cacheMatrix
## as an input an try to retrieve the cached inverse if available.
## Otherwise, it calculate the invert and populate the cache.
## 
## 
## This file contains definition of the functions makeCacheMatrix and cacheSolve
## *****************************************************************************

## makeCacheMatrix :`
## *****************
## makeCacheMatrix creates CacheMatrix "object" with 4 function, that can store  
## matrix and its invert.
## The functions set() and get(x) are used to set or get matrix data.
## The function setinv(x) is used to cached the inverse of the matrix
## The funciton getinv() is used to retrieve cached value.
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
## It first test if the cache invert is setted.
## If there is an invert cached , it retrieves the invert from the environment 
## using getinv() and skip the computation.
## Otherwise is retrieve the Matrix datas using get(), 
## run invert computation using function solve on them
## and finally cache the result using setinv().
## The inverted matrix is returned in both case.

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
