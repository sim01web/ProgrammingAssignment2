## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix creates a special "matrix" that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        setM <- function(y) {
                x <<- y
               inv <<- NULL
        }
        getM <- function() x
        setInv <- function(solve) inv <<- solve
        getInv <- function() inv
        list(setM = setM, getM = getM,
             setInv = setInv,
             getInv = getInv)

}


## Write a short comment describing this function
## cacheSolve will calculate the inverse of the special matrix created with the above function.
## If the inverse has already been calculated, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setInv function.

cacheSolve <- function(x, ...) {
        
        inv <- x$getInv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$getM()
       inv <-solve(data, ...)
        x$setInv(inv)
       inv
}
        ## Return a matrix that is the inverse of 'x'

