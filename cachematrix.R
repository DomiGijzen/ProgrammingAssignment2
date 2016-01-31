## This function will calculate the inverse of a given matrix if 
## not already calculated and returns its value.

## makeCacheMatrix sets the variables(input, output and functions) to 
## calculate the inverse.

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inverse <<- solve
        getinverse <- function() inverse
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## cacheSolve returns the already calculated inverse or
## calculates it, using the variables created by makeCacheMatrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()
        ## if already calculated, return this
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        ## Else, calculate the inverse
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse
}
