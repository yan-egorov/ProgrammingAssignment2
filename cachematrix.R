## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates an object which can cache inverse matrix.
## It returns a list of methods:
## set: sets matrix x and  clear x_inv
## get: returns matrix x
## setSolve: saves solve value
## getSolve: returns saved x_inv
 
makeCacheMatrix <- function(x = matrix()) {
	inv_x <- NULL
        set <- function(y) {
                x <<- y
                inv_x <<- NULL
        }
        get <- function() x
        setSolve <- function(solve) inv_x <<- solve
        getSolve <- function() inv_x
        list(set = set, get = get,
             setSolve = setSolve,
             getSolve = getSolve)
}


## Write a short comment describing this function
## This function takes an object called x and type "special matrix"
## which contains not only initial matrix but an inverse x also if
## it was calculated before. If no (inv_x == NA) it will be calculated 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv_x <- x$getSolve()
        if(!is.null(inv_x)) {
                message("getting cached data")
                return(inv_x)
        }
        data <- x$get()
        inv_x <- solve(data, ...)
        x$setSolve(inv_x)
        inv_x
}
