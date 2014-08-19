## These function will save on computing time for the inverse o
## of a matrix(in this case)
## by checking the cache to see if the value has already been computeed

## This function creates a matrix as well as several subroutines that will be
## referenced from the parent environment (outside this function)

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## This function will output the inverse of the input matrix
## but first it will check the cache memory to see if the compuation 
## has already taken place and simply retrieve it from memory.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
