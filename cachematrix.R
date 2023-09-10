## Pair of functions that cache the inverse of a matrix

## Function creating a matrix objext that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
                }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set,                                         ## return list of the methods
             get = get, 
             setInverse = setInverse, 
             getInverse = getInverse)                
}


## This function computes the inverse of the special matrix returned by the makeCacheMatrix function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
                }
        data <- x$get()                                ##get the matrix from our object
        inv <- solve(data)                           
        x$setInverse(inv)                              ##setting the inverse to the object
        inv                                            ##return
}
