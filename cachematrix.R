## Put comments here that give an overall description of what your
## functions do
# These two functions reduces the computation efforts of calculating the inverse of a matrix many times by computing it only once, 
# and storing it, so that as long as the matrix is not changed we do not need to compute the inverse more then once.

## Write a short comment describing this function
# This function takes an (optional) input of a matrix (square) and returns a 'special' matrix made out of it.
# This function has the capability to store (cache) the inverse of the matrix.
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y){
        x <<- y
        inverse <<- NULL
    } 
    get <- function() x
    set_inverse <- function(inv) inverse <<- inv
    get_inverse <- function() inverse
    
    list(set = set,
         get = get, 
         set_inverse = set_inverse, 
         get_inverse = get_inverse)
}


## Write a short comment describing this function
# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$get_inverse()
    if(!is.null(inv)){
        inv
    }
    else{
        d <- x$get()
        inv <- solve(d, ...)
        x$set_inverse(inv)
        inv
    }
}
