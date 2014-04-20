#Matrix inversion is usually a costly computation and their may be some benefit 
#to caching the inverse of a matrix rather than compute it repeatedly 
#Functions below cache the inverse of a matrix.

#Creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

#Computes the inverse of the special "matrix" returned by makeCacheMatrix. 
#If the inverse has already been calculated (and the matrix has not changed), 
#then the cachesolve retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)
    inv    
}