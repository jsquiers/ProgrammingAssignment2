## Programming Assignment 2: Caching the Inverse of a Matrix



## makeCacheMatrix: This function creates a special "matrix" object that can 
## cache its inverse.

## The first function, makeCacheMatrix creates a list containing a function to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        local_matrix <- NULL
        set <- function(y) {
                cacheX <<- y
                cacheM <<- NULL
        }
        get <- function() cacheX
        setcacheM <- function(local_matrix) cacheM <<- local_matrix
        getcacheM <- function() cacheM
        list(set = set, get = get, setcacheM = setcacheM, getcacheM = getcacheM)
}


## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
        local_matrix <- x$getcacheM()
        if(!is.null(local_matrix)) {
                message("getting cached data")
                return(local_matrix)
        }
        firstmatrix <- x$get()
        lastmatrix <- solve(firstmatrix)
        x$setcacheM(lastmatrix)
        lastmatrix
}
