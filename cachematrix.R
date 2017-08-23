## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## the makeCacheMatrix create a special matrix with following functions:
## set the value of matrix, get the value of matrix
## set the value of inverse, get the value of inverse

makeCacheMatrix <- function(x = matrix()) {
     inv <- NULL
     set <- function(y){
          x <<- y
          inv <<- NULL
     }
     get <- function() x
     set_inv <- function(invert) inv <<- invert
     get_inv <- function() inv
     list(set = set, get = get,
          set_inv = set_inv,
          get_inv = get_inv)
}


## Write a short comment describing this function
## cacheSolve checks if the matrix inverse is computed
## if yes, the computation is skipped and get value of inverse from the cache
## otherwise, compute the inverse matrix and store the value in the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     inv <- x$get_inv()
     if(!is.null(inv)){
          message("getting cached data")
          return(inv)
     }
     matr <- x$get()
     inv <- solve(matr)
     x$set_inv(inv)
     inv
}
