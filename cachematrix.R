## The following pair of functions can be used to cache the inverse of a matrix

##  makeCacheMatrix creates a list of functions to 1. set the value of the martix
##  2. get the vakue of the matrix. 3. set the inverse of the matrix 4. get the
##  inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                        x <<- y
                        inv <<- NULL 
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


##  cacheSolve takes a matrix created with makeCacheMatrix and calculates
##  the inverse of that matrix.But before calcuating that inverse matrix it
##  checks to see if a value for it already exists in the cache. If so,
##  it gets the inverse matrix from the cache and skips the calculation.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
        ## Return a matrix that is the inverse of 'x'
}
