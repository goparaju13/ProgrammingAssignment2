## makeCacheMatrix method can get or set the value of the matrix and get or set the value of the inverse of the matrix
## functions do

## gets or sets the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {

        minv <- NULL
        set <- function(y) {
                x <<- y
                minv <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) minv <<- solve
        getinverse <- function() minv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## Gets the inverse if available in the cache or else calculates it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
                minv <- x$getinverse()
                if(!is.null(minv)) {
                        message("getting cached data")
                        return(minv)
                }
                data <- x$get()
                minv <- solve(data, ...)
                x$setinverse(minv)
                minv
        
}
