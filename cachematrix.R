## Put comments here that give an overall description of what your
## functions do
##
## In this script there are two functions 'makeCacheMatrix' and 'cacheSolve'.
## makeCacheMatrix creates a special matrix object and caches its inverse matrix in the parent environment.
## cacheSolve checks whether an inverse matrix has been calculated previously and retrieves it or calculates the inverse matrix.
##
## Write a short comment describing this function
##
## makeCacheMatrix creates a list which contrains 4 functions:
## set - sets the value of the matrix object
## get - retrieves the value of the matrix object
## setinv - sets the value of the inverted matrix object
## getinv - retrieves the inverted matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,setinv = setinv,
             getinv = getinv)
}


## Write a short comment describing this function
##
## 1. Retrieves the inverse matrix from cache
## 2. Checks whether the the inverse matrix has been calculated or whether the matrix changed.
## 3. If it has not been calculated or the matrix changed, it retrieves the matrix and calculates the inverse matrix and returns its value to makeCacheMatrix.
## 4. Otherwise it retrieve the inverse matrix from the cache.
## 5. It prints out either the cached or new calculated inverse matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, diag(nrow(data)))
        x$setinv(inv)
        inv
}
