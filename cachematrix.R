## The below functions cache the inverse of a matrix.

## The below function creates a special matrix which can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        invrse <- NULL
        set <- function(y) {
                x <<- y
                invrse <<- NULL
        }
        get <- function() x
        setInvrse <- function(inverse) invrse <<- inverse
        getInvrse <- function() invrse
        list(set = set,
             get = get,
             setInvrse = setInverse,
             getInvrse = getInverse)
}


## In the below function the inverse of the matrix created by 
## the above function is created. If the inverse has already been calculated 
## then it should retrieve the inverse of the matrix from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invrse <- x$getInverse()
        if (!is.null(invrse)) {
                message("getting cached data")
                return(invrse)
        }
        matrx <- x$get()
        invrse <- solve(matrx, ...)
        x$setInverse(invrse)
        invrse
}