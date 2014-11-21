## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than computing it
## repeatedly (there are also alternatives to matrix inversion that we will
## not discuss here). I have written two function that cache the inverse of 
## a matrix.


## This function creates a special "matrix" object that can cache its 
## inverse.
makeCacheMatrix <- function(x = matrix()) {
        internal_matrix <- NULL
        set <- function(y) {
                x <<- y
                internal_matrix <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) internal_matrix <<- inverse
        getinverse <- function() internal_matrix
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
		## reading the inverse matrix from special "matrix"
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
        		## There were the a "matrix" in cache
                return(m)
        }
        ## Because it did not go in the if statement, so the "matrix"
        ## have to be inversed
        ## getting the matrix from makeCacheMatrix
        data <- x$get()
        ## doing the inverse with solve function
        m <- solve(data, ...)
        ## storing it to the makeCacheMatrix 
        x$setinverse(m)
        ## Return a matrix that is the inverse of 'x'
        m
}