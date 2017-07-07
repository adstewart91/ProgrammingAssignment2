## Author:  Andrew D. Stewart - adstewart91@hotmail.com  5 July 2017
## Programming Assignment 2 - Week 3 - R Programming

## This file provides a pair of functions that cache the inverse of a matrix.
## 

## The makeCacheMatrix function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmtx <- function(mtx) m <<- mtx
        getmtx <- function() m
        list(set = set, get = get,
             setmtx = setmtx,
             getmtx = getmtx)
}


## The cacheSolve functions computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the matrix 
## has not changed), then cacheSolve will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m <- x$getmtx()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setmtx(m)
        m
}

