## The function reads in a matrix and computes the inverse
## It also caches the inverse for a particular matrix
## If the inverse has already been computed, the function will simply
## return the value from the cache

## This function takes in a matrix and 
##set the value of the matrix
##get the value of the matrix
##set the value of the inverse
##set the value of the inverse

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


## This function computes the inverse of the matrix created above
## It first checks if the inverse has already been calculated, if so it gets
## the inverse from the cache and skips computation

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	 inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
