## File contains 2 functions : one to create a special matrix and one to inverse special matrix using cache when possible

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## basic functions allowing to set/get a value to the matrix and to setinv/getinv the inverse of the matrix 

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(value) inv <<- value
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesSolve should 
## retrieve the inverse from the cache.
##  WARNING : the matrix supplied must be inversible

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting the cache inverse matrix")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinv(inv)
        inv
}
