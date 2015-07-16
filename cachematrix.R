## The pair of functions below cache the inverse of a matrix.
## The first function, makeCacheMatrix creates a special "matrix" object that 
## can cache its inverse. 
## The second function, cacheSolve computes the inverse of the special "matrix"
## returned by makeCacheMatrix. If the inverse has already been calculated (and
## the matrix has not changed), then the cacheSolve should retrieve the inverse
## from the cache.
    

## This function creates a matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y){
        x <<- y
        i <<- NULL
    }
    get <- function () x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function returns a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)){
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve (data,...)
    x$setinverse(i)
    i
}
